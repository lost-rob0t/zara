from __future__ import annotations

import inspect
from dataclasses import dataclass
from typing import Any, Awaitable, Callable, Optional


class HookRegistrationError(ValueError):
    pass


class HookInvocationError(RuntimeError):
    pass


HookCallback = Callable[..., Any]
BaseCallable = Callable[..., Any]

_ALLOWED_KINDS = frozenset({"before", "after", "around", "override"})
_OVERRIDE_CAPABLE_KINDS = frozenset({"around", "override"})
_MAX_OWNER_LENGTH = 128
_MAX_ABS_PRIORITY = 100_000


@dataclass(frozen=True)
class HookRegistration:
    registration_id: int
    kind: str
    owner: str
    priority: int
    sequence: int
    callback: HookCallback


@dataclass(frozen=True)
class HookDiagnostic:
    registration_id: int
    kind: str
    owner: str
    priority: int
    sequence: int


class AgentLoopAdviceRegistry:
    def __init__(self, *, enabled: bool, allow_override: bool) -> None:
        self.enabled = bool(enabled)
        self.allow_override = bool(allow_override)
        self._next_registration_id = 1
        self._next_sequence = 1
        self._registrations: dict[int, HookRegistration] = {}

    def register(
        self,
        kind: str,
        owner: str,
        priority: int,
        callback: HookCallback,
    ) -> int:
        self._validate_registration(kind, owner, priority, callback)

        registration_id = self._next_registration_id
        sequence = self._next_sequence
        self._next_registration_id += 1
        self._next_sequence += 1
        self._registrations[registration_id] = HookRegistration(
            registration_id=registration_id,
            kind=kind,
            owner=owner,
            priority=priority,
            sequence=sequence,
            callback=callback,
        )
        return registration_id

    def unregister(self, registration_id: Optional[int]) -> bool:
        if registration_id is None:
            return False
        return self._registrations.pop(registration_id, None) is not None

    def clear_owner(self, owner: str) -> int:
        registration_ids = [
            registration.registration_id
            for registration in self._registrations.values()
            if registration.owner == owner
        ]
        for registration_id in registration_ids:
            del self._registrations[registration_id]
        return len(registration_ids)

    def list_registrations(self) -> tuple[HookRegistration, ...]:
        return tuple(self._snapshot())

    def diagnostics(self) -> tuple[HookDiagnostic, ...]:
        return tuple(
            HookDiagnostic(
                registration_id=registration.registration_id,
                kind=registration.kind,
                owner=registration.owner,
                priority=registration.priority,
                sequence=registration.sequence,
            )
            for registration in self._snapshot()
        )

    async def invoke(self, base_callable: BaseCallable, *args: Any, **kwargs: Any) -> Any:
        if not callable(base_callable):
            raise HookInvocationError("base callable must be callable")
        if not self.enabled:
            return await self._call(base_callable, *args, **kwargs)

        snapshot = self._snapshot()
        overrides = [registration for registration in snapshot if registration.kind == "override"]
        if len(overrides) > 1:
            raise HookInvocationError("multiple active override hooks are ambiguous")

        for registration in snapshot:
            if registration.kind == "before":
                await self._call(registration.callback, *args, **kwargs)

        target = self._build_target(base_callable, snapshot, overrides)
        result = await target(*args, **kwargs)

        for registration in snapshot:
            if registration.kind == "after":
                await self._call(registration.callback, result)

        return result

    def _build_target(
        self,
        base_callable: BaseCallable,
        snapshot: list[HookRegistration],
        overrides: list[HookRegistration],
    ) -> Callable[..., Awaitable[Any]]:
        selected: BaseCallable = overrides[0].callback if overrides else base_callable

        async def target(*args: Any, **kwargs: Any) -> Any:
            return await self._call(selected, *args, **kwargs)

        around = [registration for registration in snapshot if registration.kind == "around"]
        continuation: Callable[..., Awaitable[Any]] = target
        for registration in reversed(around):
            inner = continuation

            async def wrapped(
                *args: Any,
                _callback: HookCallback = registration.callback,
                _inner: Callable[..., Awaitable[Any]] = inner,
                **kwargs: Any,
            ) -> Any:
                return await self._call(_callback, _inner, *args, **kwargs)

            continuation = wrapped

        return continuation

    def _snapshot(self) -> list[HookRegistration]:
        return sorted(
            self._registrations.values(),
            key=lambda registration: (registration.priority, registration.sequence),
        )

    def _validate_registration(
        self,
        kind: str,
        owner: str,
        priority: int,
        callback: HookCallback,
    ) -> None:
        if kind not in _ALLOWED_KINDS:
            raise HookRegistrationError(f"unknown hook kind: {kind!r}")
        if not isinstance(owner, str) or not owner or len(owner) > _MAX_OWNER_LENGTH:
            raise HookRegistrationError("owner must be a non-empty bounded string")
        if isinstance(priority, bool) or not isinstance(priority, int):
            raise HookRegistrationError("priority must be an integer")
        if abs(priority) > _MAX_ABS_PRIORITY:
            raise HookRegistrationError("priority is outside the supported range")
        if not callable(callback):
            raise HookRegistrationError("callback must be callable")
        if kind in _OVERRIDE_CAPABLE_KINDS and not (
            self.enabled and self.allow_override
        ):
            raise HookRegistrationError(
                f"{kind} hooks require hooks enabled and allow_override enabled"
            )

    @staticmethod
    async def _call(callback: HookCallback, *args: Any, **kwargs: Any) -> Any:
        result = callback(*args, **kwargs)
        if inspect.isawaitable(result):
            return await result
        return result
