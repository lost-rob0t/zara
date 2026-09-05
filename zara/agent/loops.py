from __future__ import annotations

from dataclasses import dataclass
from threading import RLock
from typing import Awaitable, Callable, Dict


AgentLoopCallback = Callable[..., Awaitable[dict]]


class UnknownAgentLoopBackend(LookupError):
    pass


class AgentLoopBackendOverrideDisabled(PermissionError):
    pass


@dataclass(frozen=True)
class AgentLoopRegistration:
    registration_id: int
    name: str
    owner: str
    callback: AgentLoopCallback


@dataclass(frozen=True)
class AgentLoopDiagnostic:
    registration_id: int
    name: str
    owner: str


class AgentLoopRegistry:
    def __init__(self) -> None:
        self._lock = RLock()
        self._next_registration_id = 1
        self._by_name: Dict[str, AgentLoopRegistration] = {}
        self._by_id: Dict[int, AgentLoopRegistration] = {}

    def register(self, name: str, owner: str, callback: AgentLoopCallback) -> int:
        normalized_name = self._normalize_name(name)
        normalized_owner = self._normalize_owner(owner)
        if not callable(callback):
            raise TypeError("agent loop callback must be callable")

        with self._lock:
            if normalized_name in self._by_name:
                raise ValueError(f"agent loop backend already registered: {normalized_name}")

            registration_id = self._next_registration_id
            self._next_registration_id += 1
            registration = AgentLoopRegistration(
                registration_id=registration_id,
                name=normalized_name,
                owner=normalized_owner,
                callback=callback,
            )
            self._by_name[normalized_name] = registration
            self._by_id[registration_id] = registration
            return registration_id

    def resolve(self, name: str) -> AgentLoopRegistration:
        normalized_name = self._normalize_name(name)
        with self._lock:
            registration = self._by_name.get(normalized_name)
        if registration is None:
            raise UnknownAgentLoopBackend(
                f"unknown agent loop backend: {normalized_name}"
            )
        return registration

    def unregister(self, registration_id: int, *, owner: str) -> bool:
        normalized_owner = self._normalize_owner(owner)
        with self._lock:
            registration = self._by_id.get(registration_id)
            if registration is None or registration.owner != normalized_owner:
                return False
            self._by_id.pop(registration_id, None)
            self._by_name.pop(registration.name, None)
            return True

    def list_registrations(self) -> tuple[AgentLoopRegistration, ...]:
        with self._lock:
            return tuple(
                sorted(
                    self._by_id.values(),
                    key=lambda registration: registration.registration_id,
                )
            )

    def diagnostics(self) -> tuple[AgentLoopDiagnostic, ...]:
        return tuple(
            AgentLoopDiagnostic(
                registration_id=registration.registration_id,
                name=registration.name,
                owner=registration.owner,
            )
            for registration in self.list_registrations()
        )

    @staticmethod
    def _normalize_name(name: str) -> str:
        normalized = str(name).strip().lower()
        if not normalized:
            raise ValueError("agent loop backend name must not be empty")
        return normalized

    @staticmethod
    def _normalize_owner(owner: str) -> str:
        normalized = str(owner).strip()
        if not normalized:
            raise ValueError("agent loop backend owner must not be empty")
        return normalized
