"""Single-owner actor bridge to the local Tek9 JSONL ingest worker."""

from __future__ import annotations

import json
import subprocess
from dataclasses import dataclass
from typing import Any, Callable, ClassVar, Mapping, Sequence

import pykka

from zara.actors import BoundedActor


class Tek9WorkerError(RuntimeError):
    """Base failure for the local Tek9 ingest process."""


class Tek9WorkerUnavailable(Tek9WorkerError):
    """The worker died or its pipe became unusable."""


class Tek9WorkerProtocolError(Tek9WorkerError):
    """The local worker returned malformed or unexpected protocol data."""


class Tek9GenerationConflict(Tek9WorkerError):
    """Tek9 rejected a stale source-generation write."""

    def __init__(
        self,
        *,
        expected_generation: int,
        actual_generation: int,
        requested_generation: int,
    ) -> None:
        super().__init__(
            "Tek9 ingest generation conflict: "
            f"expected={expected_generation} actual={actual_generation} "
            f"requested={requested_generation}"
        )
        self.expected_generation = expected_generation
        self.actual_generation = actual_generation
        self.requested_generation = requested_generation


@dataclass(frozen=True)
class _WorkerCall:
    payload: Mapping[str, Any]
    replay_safe: bool


class Tek9WorkerActor(BoundedActor):
    """Own one Tek9 subprocess and serialize every request through its pipes."""

    mailbox_size: ClassVar[int] = 64
    mailbox_overflow: ClassVar[str] = "block"

    def __init__(
        self,
        command: Sequence[str],
        *,
        popen_factory: Callable[..., Any] = subprocess.Popen,
        max_response_bytes: int = 1024 * 1024,
    ) -> None:
        super().__init__()
        if not command or not all(isinstance(item, str) and item for item in command):
            raise ValueError("Tek9 worker command must contain non-empty strings")
        if isinstance(max_response_bytes, bool) or not isinstance(max_response_bytes, int):
            raise ValueError("max_response_bytes must be an integer")
        if max_response_bytes < 1:
            raise ValueError("max_response_bytes must be positive")
        self._command = tuple(command)
        self._popen_factory = popen_factory
        self._max_response_bytes = max_response_bytes
        self._process = None

    def on_start(self) -> None:
        self._spawn()

    def on_stop(self) -> None:
        self._discard_process(terminate=True)

    def on_receive(self, message: Any) -> Any:
        if isinstance(message, _WorkerCall):
            return self._request(message.payload, replay_safe=message.replay_safe)
        return super().on_receive(message)

    def _spawn(self) -> None:
        self._discard_process(terminate=False)
        try:
            self._process = self._popen_factory(
                list(self._command),
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.DEVNULL,
                text=True,
                encoding="utf-8",
                bufsize=1,
            )
        except Exception as error:
            self._process = None
            raise Tek9WorkerUnavailable("could not start Tek9 ingest worker") from error
        if self._process.stdin is None or self._process.stdout is None:
            self._discard_process(terminate=True)
            raise Tek9WorkerUnavailable("Tek9 worker did not expose pipes")

    def _request(self, payload: Mapping[str, Any], *, replay_safe: bool) -> dict[str, Any]:
        if self._process is None or self._process.poll() is not None:
            self._discard_process(terminate=False)
            if not replay_safe:
                raise Tek9WorkerUnavailable(
                    "Tek9 worker unavailable; reconcile status before retrying mutation"
                )
            self._spawn()

        try:
            return self._request_once(payload)
        except Tek9GenerationConflict:
            raise
        except Tek9WorkerError:
            self._discard_process(terminate=True)
            if not replay_safe:
                raise
            self._spawn()
            return self._request_once(payload)

    def _request_once(self, payload: Mapping[str, Any]) -> dict[str, Any]:
        process = self._process
        if process is None or process.stdin is None or process.stdout is None:
            raise Tek9WorkerUnavailable("Tek9 worker is not running")

        try:
            encoded = json.dumps(
                dict(payload),
                ensure_ascii=False,
                allow_nan=False,
                sort_keys=True,
                separators=(",", ":"),
            )
        except (TypeError, ValueError) as error:
            raise Tek9WorkerProtocolError("Tek9 request is not strict JSON") from error

        try:
            process.stdin.write(encoded + "\n")
            process.stdin.flush()
            line = process.stdout.readline()
        except (BrokenPipeError, OSError, ValueError) as error:
            raise Tek9WorkerUnavailable("Tek9 worker pipe failed") from error

        if not line:
            raise Tek9WorkerUnavailable("Tek9 worker closed its response pipe")
        if len(line.encode("utf-8")) > self._max_response_bytes:
            raise Tek9WorkerProtocolError("Tek9 worker response exceeds byte limit")

        try:
            response = json.loads(line)
        except (TypeError, ValueError, json.JSONDecodeError) as error:
            raise Tek9WorkerProtocolError("Tek9 worker returned invalid JSON") from error
        if not isinstance(response, dict) or type(response.get("ok")) is not bool:
            raise Tek9WorkerProtocolError("Tek9 worker response has invalid shape")

        if response["ok"]:
            return response

        code = response.get("code")
        if code == "generation_conflict":
            try:
                raise Tek9GenerationConflict(
                    expected_generation=int(response["expected_generation"]),
                    actual_generation=int(response["actual_generation"]),
                    requested_generation=int(response["requested_generation"]),
                )
            except (KeyError, TypeError, ValueError) as error:
                raise Tek9WorkerProtocolError(
                    "Tek9 generation conflict response is malformed"
                ) from error

        if not isinstance(code, str) or not code:
            raise Tek9WorkerProtocolError("Tek9 worker error response has no code")
        raise Tek9WorkerError(f"Tek9 worker rejected request: {code}")

    def _discard_process(self, *, terminate: bool) -> None:
        process = self._process
        self._process = None
        if process is None:
            return

        for stream_name in ("stdin", "stdout"):
            stream = getattr(process, stream_name, None)
            if stream is not None:
                try:
                    stream.close()
                except Exception:
                    pass

        if not terminate or process.poll() is not None:
            return

        try:
            process.terminate()
            process.wait(timeout=2)
        except Exception:
            try:
                process.kill()
            except Exception:
                pass


class Tek9WorkerClient:
    """Typed synchronous facade over the Tek9 worker actor."""

    def __init__(
        self,
        actor_ref,
        *,
        timeout: float,
        max_records: int,
    ) -> None:
        self._actor_ref = actor_ref
        self._timeout = timeout
        self._max_records = max_records

    @classmethod
    def start(
        cls,
        command: Sequence[str],
        *,
        popen_factory: Callable[..., Any] = subprocess.Popen,
        timeout: float = 30.0,
        max_records: int = 4096,
        max_response_bytes: int = 1024 * 1024,
    ) -> "Tek9WorkerClient":
        if not isinstance(timeout, (int, float)) or isinstance(timeout, bool) or timeout <= 0:
            raise ValueError("timeout must be positive")
        if isinstance(max_records, bool) or not isinstance(max_records, int) or max_records < 1:
            raise ValueError("max_records must be a positive integer")
        ref = Tek9WorkerActor.start(
            command,
            popen_factory=popen_factory,
            max_response_bytes=max_response_bytes,
        )
        return cls(ref, timeout=float(timeout), max_records=max_records)

    def stop(self) -> None:
        self._actor_ref.stop(block=True, timeout=self._timeout)

    def status(self, source_id: str) -> dict[str, Any]:
        source = _bounded_text("source_id", source_id, 512)
        return self._ask(
            {"op": "status", "source_id": source},
            replay_safe=True,
        )

    def apply_batch(
        self,
        *,
        source_id: str,
        generation: int,
        expected_generation: int,
        documents: Sequence[Mapping[str, Any]] = (),
        nodes: Sequence[Mapping[str, Any]] = (),
        edges: Sequence[Mapping[str, Any]] = (),
        database_name: str | None = None,
        graph_name: str | None = None,
        watermark: str | int | None = None,
    ) -> dict[str, Any]:
        source = _bounded_text("source_id", source_id, 512)
        new_generation = _positive_int("generation", generation)
        expected = _nonnegative_int("expected_generation", expected_generation)
        docs = _bounded_records("documents", documents, self._max_records)
        graph_nodes = _bounded_records("nodes", nodes, self._max_records)
        graph_edges = _bounded_records("edges", edges, self._max_records)
        if watermark is not None and not isinstance(watermark, (str, int)):
            raise ValueError("watermark must be a string, integer, or None")

        payload: dict[str, Any] = {
            "op": "apply_batch",
            "source_id": source,
            "generation": new_generation,
            "expected_generation": expected,
            "documents": docs,
            "nodes": graph_nodes,
            "edges": graph_edges,
        }
        if database_name is not None:
            payload["database_name"] = _bounded_text(
                "database_name", database_name, 512
            )
        if graph_name is not None:
            payload["graph_name"] = _bounded_text("graph_name", graph_name, 512)
        if watermark is not None:
            payload["watermark"] = watermark

        return self._ask(payload, replay_safe=False)

    def _ask(self, payload: Mapping[str, Any], *, replay_safe: bool) -> dict[str, Any]:
        try:
            response = self._actor_ref.ask(
                _WorkerCall(payload=dict(payload), replay_safe=replay_safe),
                timeout=self._timeout,
            )
        except pykka.Timeout as error:
            raise Tek9WorkerUnavailable("Tek9 worker request timed out") from error
        if not isinstance(response, dict):
            raise Tek9WorkerProtocolError("Tek9 actor returned invalid response")
        return response


def _bounded_text(name: str, value: Any, max_length: int) -> str:
    if not isinstance(value, str) or not value or len(value) > max_length:
        raise ValueError(f"{name} must be a bounded non-empty string")
    return value


def _positive_int(name: str, value: Any) -> int:
    if isinstance(value, bool) or not isinstance(value, int) or value <= 0:
        raise ValueError(f"{name} must be a positive integer")
    return value


def _nonnegative_int(name: str, value: Any) -> int:
    if isinstance(value, bool) or not isinstance(value, int) or value < 0:
        raise ValueError(f"{name} must be a non-negative integer")
    return value


def _bounded_records(
    name: str,
    value: Sequence[Mapping[str, Any]],
    max_records: int,
) -> list[dict[str, Any]]:
    if isinstance(value, (str, bytes)) or not isinstance(value, Sequence):
        raise ValueError(f"{name} must be a sequence")
    if len(value) > max_records:
        raise ValueError(f"{name} exceeds {max_records} records")
    result: list[dict[str, Any]] = []
    for item in value:
        if not isinstance(item, Mapping):
            raise ValueError(f"{name} entries must be mappings")
        result.append(dict(item))
    return result


__all__ = [
    "Tek9GenerationConflict",
    "Tek9WorkerActor",
    "Tek9WorkerClient",
    "Tek9WorkerError",
    "Tek9WorkerProtocolError",
    "Tek9WorkerUnavailable",
]
