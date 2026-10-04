"""Process-isolated owner for registered predicate execution authority.

The plugin-facing side receives only :class:`PredicateAuthorityClient`. Trusted
predicate bindings live in a separate process and are selected by logical
operation. Even a caller that recovers the raw transport can send only bounded
JSON frames; executable predicate identity is never accepted from the caller.
"""

from __future__ import annotations

import json
import multiprocessing
import threading
import time
from dataclasses import dataclass
from multiprocessing.connection import Connection
from typing import Any, Mapping, Protocol

from zara.plugins.predicate_authority import (
    PredicateAuthorityError,
    PredicateExecutionProvenance,
    PredicateInvocationRequest,
    PredicateInvocationResult,
    PredicateVerdict,
    validate_inert_arguments,
)


MAX_AUTHORITY_FRAME_BYTES = 64 * 1024
_MAX_REPLAY_IDS = 4096


@dataclass(frozen=True)
class RegisteredPredicateBinding:
    """Trusted construction-time mapping never sent by the plugin caller."""

    operation: str
    namespace: str
    predicate: str
    arity: int
    generation: int

    def __post_init__(self) -> None:
        PredicateExecutionProvenance(
            namespace=self.namespace,
            predicate=self.predicate,
            arity=self.arity,
            generation=self.generation,
            request_id="binding-check",
        )
        PredicateInvocationRequest(
            request_id="binding-check",
            operation=self.operation,
            expected_generation=self.generation,
            arguments={},
            timeout_ms=1,
        )


@dataclass(frozen=True)
class PredicateExecutionOutcome:
    """Domain result produced by the trusted child-side executor."""

    verdict: PredicateVerdict
    data: Mapping[str, Any]
    error_code: str = ""

    def __post_init__(self) -> None:
        if not isinstance(self.verdict, PredicateVerdict):
            raise PredicateAuthorityError("verdict must be a PredicateVerdict")
        object.__setattr__(self, "data", validate_inert_arguments(self.data))


class PredicateExecutor(Protocol):
    def __call__(
        self,
        binding: RegisteredPredicateBinding,
        arguments: Mapping[str, Any],
        timeout_ms: int,
    ) -> PredicateExecutionOutcome: ...


class PredicateAuthorityClient:
    """Caller-facing inert request client.

    Hostile same-process code may recover the transport through introspection.
    The child therefore treats every received byte as untrusted. Recovering the
    transport does not recover executable predicate authority.
    """

    def __init__(self, transport: Connection, process: multiprocessing.Process) -> None:
        self._transport = transport
        self._process = process
        self._lock = threading.Lock()

    def invoke(self, request: PredicateInvocationRequest) -> PredicateInvocationResult:
        frame = _encode_request(request)
        with self._lock:
            if not self._process.is_alive():
                return _unavailable()
            try:
                self._transport.send_bytes(frame)
            except (BrokenPipeError, EOFError, OSError):
                return _unavailable()

            wait_seconds = max(request.timeout_ms / 1000.0, 0.001)
            if not self._transport.poll(wait_seconds):
                self._kill_worker_after_timeout()
                return PredicateInvocationResult(
                    verdict=PredicateVerdict.CANCELLED,
                    provenance=None,
                    data={},
                    error_code="authority_timeout",
                )
            try:
                payload = self._transport.recv_bytes(MAX_AUTHORITY_FRAME_BYTES)
            except (EOFError, OSError):
                return _unavailable()
        return _decode_result(payload)

    def _kill_worker_after_timeout(self) -> None:
        try:
            self._process.terminate()
            self._process.join(0.25)
            if self._process.is_alive():
                self._process.kill()
                self._process.join(0.25)
        finally:
            try:
                self._transport.close()
            except OSError:
                pass

    def close(self) -> None:
        try:
            self._transport.close()
        except OSError:
            pass


class PredicateAuthorityProcess:
    """Core lifecycle handle for one immutable authority generation.

    Reload/revoke terminates this process and creates a new generation. There is
    intentionally no mutable registration control channel for plugin code to
    discover through same-interpreter introspection.
    """

    def __init__(
        self,
        *,
        process: multiprocessing.Process,
        client: PredicateAuthorityClient,
    ) -> None:
        self._process = process
        self.client = client

    @classmethod
    def start(
        cls,
        *,
        bindings: Mapping[str, RegisteredPredicateBinding],
        executor: PredicateExecutor,
        process_name: str = "zara-predicate-authority",
    ) -> "PredicateAuthorityProcess":
        normalized = _normalize_bindings(bindings)
        methods = multiprocessing.get_all_start_methods()
        method = "fork" if "fork" in methods else methods[0]
        ctx = multiprocessing.get_context(method)
        parent, child = ctx.Pipe(duplex=True)
        process = ctx.Process(
            target=_authority_worker,
            name=process_name,
            args=(child, normalized, executor),
            daemon=True,
        )
        process.start()
        child.close()
        client = PredicateAuthorityClient(parent, process)
        return cls(process=process, client=client)

    def stop(self, *, timeout: float = 1.0) -> None:
        self.client.close()
        if self._process.is_alive():
            self._process.terminate()
        self._process.join(timeout)
        if self._process.is_alive():
            self._process.kill()
            self._process.join(timeout)

    @property
    def pid(self) -> int | None:
        return self._process.pid


def _normalize_bindings(
    bindings: Mapping[str, RegisteredPredicateBinding],
) -> dict[str, RegisteredPredicateBinding]:
    normalized: dict[str, RegisteredPredicateBinding] = {}
    for operation, binding in bindings.items():
        if operation != binding.operation:
            raise PredicateAuthorityError("binding key must equal logical operation")
        if operation in normalized:
            raise PredicateAuthorityError(f"duplicate logical operation: {operation}")
        normalized[operation] = binding
    if not normalized:
        raise PredicateAuthorityError("at least one predicate binding is required")
    generations = {binding.generation for binding in normalized.values()}
    if len(generations) != 1:
        raise PredicateAuthorityError("one authority process must use one generation")
    return normalized


def _encode_request(request: PredicateInvocationRequest) -> bytes:
    return _encode_json(
        {
            "protocol": request.protocol,
            "request_id": request.request_id,
            "operation": request.operation,
            "expected_generation": request.expected_generation,
            "arguments": dict(request.arguments),
            "timeout_ms": request.timeout_ms,
        }
    )


def _decode_request(payload: bytes) -> PredicateInvocationRequest:
    raw = _decode_json(payload)
    if set(raw) != {
        "protocol",
        "request_id",
        "operation",
        "expected_generation",
        "arguments",
        "timeout_ms",
    }:
        raise PredicateAuthorityError("request frame contains unsupported fields")
    return PredicateInvocationRequest(
        protocol=raw["protocol"],
        request_id=raw["request_id"],
        operation=raw["operation"],
        expected_generation=raw["expected_generation"],
        arguments=raw["arguments"],
        timeout_ms=raw["timeout_ms"],
    )


def _encode_result(result: PredicateInvocationResult) -> bytes:
    provenance = None
    if result.provenance is not None:
        provenance = {
            "namespace": result.provenance.namespace,
            "predicate": result.provenance.predicate,
            "arity": result.provenance.arity,
            "generation": result.provenance.generation,
            "request_id": result.provenance.request_id,
        }
    return _encode_json(
        {
            "verdict": result.verdict.value,
            "provenance": provenance,
            "data": dict(result.data),
            "error_code": result.error_code,
        }
    )


def _decode_result(payload: bytes) -> PredicateInvocationResult:
    raw = _decode_json(payload)
    if set(raw) != {"verdict", "provenance", "data", "error_code"}:
        raise PredicateAuthorityError("result frame contains unsupported fields")
    provenance_raw = raw["provenance"]
    provenance = None
    if provenance_raw is not None:
        if not isinstance(provenance_raw, dict):
            raise PredicateAuthorityError("result provenance must be an object")
        provenance = PredicateExecutionProvenance(**provenance_raw)
    return PredicateInvocationResult(
        verdict=PredicateVerdict(raw["verdict"]),
        provenance=provenance,
        data=raw["data"],
        error_code=raw["error_code"],
    )


def _encode_json(payload: Mapping[str, Any]) -> bytes:
    encoded = json.dumps(
        payload,
        ensure_ascii=True,
        allow_nan=False,
        separators=(",", ":"),
        sort_keys=True,
    ).encode("utf-8")
    if len(encoded) > MAX_AUTHORITY_FRAME_BYTES:
        raise PredicateAuthorityError("predicate-authority frame exceeds byte budget")
    return encoded


def _decode_json(payload: bytes) -> dict[str, Any]:
    if len(payload) > MAX_AUTHORITY_FRAME_BYTES:
        raise PredicateAuthorityError("predicate-authority frame exceeds byte budget")
    try:
        value = json.loads(payload.decode("utf-8"))
    except (UnicodeDecodeError, json.JSONDecodeError) as exc:
        raise PredicateAuthorityError("invalid predicate-authority JSON frame") from exc
    if not isinstance(value, dict):
        raise PredicateAuthorityError("predicate-authority frame must be an object")
    return value


def _unavailable() -> PredicateInvocationResult:
    return PredicateInvocationResult(
        verdict=PredicateVerdict.ERROR,
        provenance=None,
        data={},
        error_code="authority_unavailable",
    )


def _error_result(
    code: str,
    *,
    verdict: PredicateVerdict = PredicateVerdict.BLOCKED,
) -> PredicateInvocationResult:
    return PredicateInvocationResult(
        verdict=verdict,
        provenance=None,
        data={},
        error_code=code,
    )


def _authority_worker(
    transport: Connection,
    bindings: Mapping[str, RegisteredPredicateBinding],
    executor: PredicateExecutor,
) -> None:
    replay_ids: set[str] = set()
    try:
        while True:
            try:
                payload = transport.recv_bytes(MAX_AUTHORITY_FRAME_BYTES)
            except (EOFError, OSError):
                return

            try:
                request = _decode_request(payload)
                result = _execute_request(
                    request=request,
                    bindings=bindings,
                    executor=executor,
                    replay_ids=replay_ids,
                )
            except PredicateAuthorityError as exc:
                result = PredicateInvocationResult(
                    verdict=PredicateVerdict.ERROR,
                    provenance=None,
                    data={"message": str(exc)[:256]},
                    error_code="malformed_request",
                )
            except Exception:
                result = _error_result(
                    "authority_internal_error",
                    verdict=PredicateVerdict.ERROR,
                )

            try:
                transport.send_bytes(_encode_result(result))
            except (BrokenPipeError, EOFError, OSError):
                return
    finally:
        try:
            transport.close()
        except OSError:
            pass


def _execute_request(
    *,
    request: PredicateInvocationRequest,
    bindings: Mapping[str, RegisteredPredicateBinding],
    executor: PredicateExecutor,
    replay_ids: set[str],
) -> PredicateInvocationResult:
    if request.request_id in replay_ids:
        return _error_result("replayed_request")

    binding = bindings.get(request.operation)
    if binding is None:
        return _error_result(
            "unsupported_operation",
            verdict=PredicateVerdict.UNSUPPORTED,
        )

    if request.expected_generation != binding.generation:
        return _error_result("stale_generation")

    if len(replay_ids) >= _MAX_REPLAY_IDS:
        return _error_result("replay_journal_exhausted")
    replay_ids.add(request.request_id)

    started = time.monotonic()
    outcome = executor(binding, request.arguments, request.timeout_ms)
    elapsed_ms = int((time.monotonic() - started) * 1000)
    if elapsed_ms > request.timeout_ms:
        return _error_result(
            "execution_deadline_exceeded",
            verdict=PredicateVerdict.CANCELLED,
        )

    provenance = None
    if outcome.verdict is PredicateVerdict.SUCCEEDED:
        provenance = PredicateExecutionProvenance(
            namespace=binding.namespace,
            predicate=binding.predicate,
            arity=binding.arity,
            generation=binding.generation,
            request_id=request.request_id,
        )
    return PredicateInvocationResult(
        verdict=outcome.verdict,
        provenance=provenance,
        data=outcome.data,
        error_code=outcome.error_code,
    )
