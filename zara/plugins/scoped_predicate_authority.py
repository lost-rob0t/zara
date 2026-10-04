"""Activation-scoped facade over the Core registered-predicate authority owner.

The base predicate owner isolates executable predicate identity in a child process.
This module binds that process to one trusted activation scope at construction time
without adding principal/workspace/activation fields to the plugin-facing wire
protocol.  Public callers still carry only logical operation ids and inert data.
"""

from __future__ import annotations

import threading
from dataclasses import dataclass
from typing import Any, Mapping, Protocol

from zara.plugins.predicate_authority import (
    PredicateInvocationRequest,
    PredicateInvocationResult,
    PredicateVerdict,
)
from zara.plugins.predicate_authority_owner import (
    PredicateAuthorityClient,
    PredicateAuthorityProcess,
    PredicateExecutionOutcome,
    RegisteredPredicateBinding,
)


_MAX_SCOPE_TEXT = 256


def _scope_text(value: object, *, name: str) -> str:
    if not isinstance(value, str) or not value or len(value) > _MAX_SCOPE_TEXT:
        raise ValueError(f"{name} must contain 1 to {_MAX_SCOPE_TEXT} characters")
    if "\x00" in value or any(ord(char) < 0x20 for char in value):
        raise ValueError(f"{name} contains unsupported control characters")
    return value


@dataclass(frozen=True)
class PredicateAuthorityScope:
    """Trusted #897 activation scope, never accepted from plugin/model input."""

    principal: str
    workspace: str
    activation_id: str

    def __post_init__(self) -> None:
        object.__setattr__(self, "principal", _scope_text(self.principal, name="principal"))
        object.__setattr__(self, "workspace", _scope_text(self.workspace, name="workspace"))
        object.__setattr__(
            self,
            "activation_id",
            _scope_text(self.activation_id, name="activation_id"),
        )


class ScopedPredicateExecutor(Protocol):
    """Trusted child-side executor that receives activation scope out-of-band."""

    def __call__(
        self,
        scope: PredicateAuthorityScope,
        binding: RegisteredPredicateBinding,
        arguments: Mapping[str, Any],
        timeout_ms: int,
    ) -> PredicateExecutionOutcome: ...


@dataclass(frozen=True)
class _ScopedExecutorAdapter:
    scope: PredicateAuthorityScope
    executor: ScopedPredicateExecutor

    def __call__(
        self,
        binding: RegisteredPredicateBinding,
        arguments: Mapping[str, Any],
        timeout_ms: int,
    ) -> PredicateExecutionOutcome:
        return self.executor(self.scope, binding, arguments, timeout_ms)


def _cancelled() -> PredicateInvocationResult:
    return PredicateInvocationResult(
        verdict=PredicateVerdict.CANCELLED,
        provenance=None,
        data={},
        error_code="authority_cancelled",
    )


class ScopedPredicateAuthorityClient:
    """Plugin-facing client with cancellation fencing but no scope authority data."""

    def __init__(
        self,
        client: PredicateAuthorityClient,
        cancelled: threading.Event,
    ) -> None:
        self._client = client
        self._cancelled = cancelled

    def invoke(self, request: PredicateInvocationRequest) -> PredicateInvocationResult:
        if self._cancelled.is_set():
            return _cancelled()
        try:
            result = self._client.invoke(request)
        except (EOFError, OSError):
            if self._cancelled.is_set():
                return _cancelled()
            raise
        if self._cancelled.is_set():
            return _cancelled()
        return result

    def close(self) -> None:
        self._client.close()


class ScopedPredicateAuthorityProcess:
    """One immutable predicate authority process bound to one activation scope.

    The scope is supplied only by trusted construction code.  It is captured by
    the child-side executor adapter and is never serialized into caller request
    frames or stored on the plugin-facing client.
    """

    def __init__(
        self,
        *,
        owner: PredicateAuthorityProcess,
        client: ScopedPredicateAuthorityClient,
        cancelled: threading.Event,
    ) -> None:
        self._owner = owner
        self.client = client
        self._cancelled = cancelled

    @classmethod
    def start(
        cls,
        *,
        scope: PredicateAuthorityScope,
        bindings: Mapping[str, RegisteredPredicateBinding],
        executor: ScopedPredicateExecutor,
        process_name: str = "zara-scoped-predicate-authority",
    ) -> "ScopedPredicateAuthorityProcess":
        if not isinstance(scope, PredicateAuthorityScope):
            raise TypeError("scope must be a PredicateAuthorityScope")
        cancelled = threading.Event()
        owner = PredicateAuthorityProcess.start(
            bindings=bindings,
            executor=_ScopedExecutorAdapter(scope=scope, executor=executor),
            process_name=process_name,
        )
        client = ScopedPredicateAuthorityClient(owner.client, cancelled)
        return cls(owner=owner, client=client, cancelled=cancelled)

    def cancel(self, *, timeout: float = 1.0) -> None:
        """Fence one activation generation and suppress every late result."""

        self._cancelled.set()
        self._owner.stop(timeout=timeout)

    def stop(self, *, timeout: float = 1.0) -> None:
        self._owner.stop(timeout=timeout)

    @property
    def pid(self) -> int | None:
        return self._owner.pid
