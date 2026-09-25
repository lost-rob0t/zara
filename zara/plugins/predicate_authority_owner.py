"""Process-isolated owner for registered predicate execution authority.

The plugin-facing side receives only :class:`PredicateAuthorityClient`. Trusted
predicate bindings live in a separate process and are selected by logical
operation. Even a caller that recovers the raw transport can send only bounded
JSON frames; executable predicate identity is never accepted from the caller.
"""

from __future__ import annotations

import json
import multiprocessing
import os
import signal
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
_AUTHORITY_STARTUP_TIMEOUT_SECONDS = 2.0


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


class _AuthorityLifecycle:
    """One generation's process and descendant-lifetime owner."""

    def __init__(
        self,
        process: multiprocessing.Process,
        *,
        process_group_id: int | None,
    ) -> None:
        self._process = process
        self._process_group_id = process_group_id
        self._terminated = False
        self._lock = threading.Lock()

    @property
    def pid(self) -> int | None:
        return self._process.pid

    def is_alive(self) -> bool:
        return self._process.is_alive()

    def terminate(self, *, timeout: float) -> None:
        with self._lock:
            if self._terminated:
                return
            self._terminated = True
            _terminate_authority_generation(
                self._process,
                process_group_id=self._process_group_id,
                timeout=timeout,
            )


class PredicateAuthorityClient:
    """Caller-facing inert request client.

    Hostile same-process code may recover the transport through introspection.
    The child therefore treats every received byte as untrusted. Recovering the
    transport does not recover executable predicate authority.
    """

    def __init__(self, transport: Connection, lifecycle: _AuthorityLifecycle) -> None:
        self._transport = transport
        self._lifecycle = lifecycle
        self._lock = threading.Lock()

    def invoke(self, request: PredicateInvocationRequest) -> PredicateInvocationResult:
        frame = _encode_request(request)
        with self._lock:
            if not self._lifecycle.is_alive():
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
            self._lifecycle.terminate(timeout=0.25)
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
        lifecycle: _AuthorityLifecycle,
        client: PredicateAuthorityClient,
    ) -> None:
        self._lifecycle = lifecycle
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
        try:
            process_group_id = _await_worker_ready(parent, process)
        except Exception:
            try:
                parent.close()
            finally:
                _terminate_worker_only(process, timeout=0.25)
            raise
        lifecycle = _AuthorityLifecycle(
            process,
            process_group_id=process_group_id,
        )
        client = PredicateAuthorityClient(parent, lifecycle)
        return cls(lifecycle=lifecycle, client=client)

    def stop(self, *, timeout: float = 1.0) -> None:
        self.client.close()
        self._lifecycle.terminate(timeout=timeout)

    @property
    def pid(self) -> int | None:
        return self._lifecycle.pid


def _await_worker_ready(
    transport: Connection,
    process: multiprocessing.Process,
) -> int | None:
    if not transport.poll(_AUTHORITY_STARTUP_TIMEOUT_SECONDS):
        raise PredicateAuthorityError("predicate-authority worker startup timed out")
    try:
        payload = transport.recv_bytes(MAX_AUTHORITY_FRAME_BYTES)
    except (EOFError, OSError) as exc:
        raise PredicateAuthorityError("predicate-authority worker failed during startup") from exc
    raw = _decode_json(payload)
    if set(raw) != {"kind", "pid", "process_group_id"} or raw["kind"] != "ready":
        raise PredicateAuthorityError("invalid predicate-authority startup frame")
    pid = raw["pid"]
    if not isinstance(pid, int) or isinstance(pid, bool) or pid != process.pid:
        raise PredicateAuthorityError("predicate-authority startup pid mismatch")

    process_group_id = raw["process_group_id"]
    if os.name != "posix":
        if process_group_id is not None:
            raise PredicateAuthorityError("unexpected predicate-authority process group")
        return None
    if (
        not isinstance(process_group_id, int)
        or isinstance(process_group_id, bool)
        or process_group_id != pid
    ):
        raise PredicateAuthorityError("invalid predicate-authority process group")
    try:
        if os.getsid(pid) != pid or os.getpgid(pid) != process_group_id:
            raise PredicateAuthorityError("predicate-authority private session not established")
    except ProcessLookupError as exc:
        raise PredicateAuthorityError("predicate-authority worker exited during startup") from exc
    return process_group_id


def _terminate_authority_generation(
    process: multiprocessing.Process,
    *,
    process_group_id: int | None,
    timeout: float,
) -> None:
    wait_seconds = max(0.0, float(timeout))
    if os.name == "posix" and process_group_id is not None:
        _terminate_process_group(
            process,
            process_group_id=process_group_id,
            timeout=wait_seconds,
        )
        return
    _terminate_worker_only(process, timeout=wait_seconds)


def _terminate_process_group(
    process: multiprocessing.Process,
    *,
    process_group_id: int,
    timeout: float,
) -> None:
    if process_group_id <= 1 or process_group_id == os.getpgrp():
        raise PredicateAuthorityError("refusing unsafe predicate-authority process group")

    _signal_process_group(process_group_id, signal.SIGTERM)
    process.join(timeout)
    if _process_group_exists(process_group_id):
        _signal_process_group(process_group_id, signal.SIGKILL)
    process.join(timeout)


def _process_group_exists(process_group_id: int) -> bool:
    try:
        os.killpg(process_group_id, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    return True


def _signal_process_group(process_group_id: int, signum: int) -> None:
    try:
        os.killpg(process_group_id, signum)
    except ProcessLookupError:
        pass


def _terminate_worker_only(
    process: multiprocessing.Process,
    *,
    timeout: float,
) -> None:
    if process.is_alive():
        process.terminate()
    process.join(timeout)
    if process.is_alive():
        process.kill()
        process.join(timeout)


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
    process_group_id = None
    if os.name == "posix":
        os.setsid()
        process_group_id = os.getpgrp()
    try:
        transport.send_bytes(
            _encode_json(
                {
                    "kind": "ready",
                    "pid": os.getpid(),
                    "process_group_id": process_group_id,
                }
            )
        )
    except (BrokenPipeError, EOFError, OSError):
        try:
            transport.close()
        except OSError:
            pass
        return

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
