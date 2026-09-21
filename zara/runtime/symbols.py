"""Canonical programmable symbol registry for Zara packages.

This module supplies the owner-scoped override stack needed by the Emacs-grade
package model. It is intentionally UI- and language-neutral: Python, Prolog and
native adapters can all project registrations into the same namespace.
"""

from __future__ import annotations

import threading
from dataclasses import dataclass
from typing import Any, Iterable, Optional


class SymbolRegistrationError(ValueError):
    pass


class SymbolLookupError(LookupError):
    pass


_MAX_SYMBOL_LENGTH = 160
_MAX_KIND_LENGTH = 48
_MAX_OWNER_LENGTH = 128
_MAX_DOCS_LENGTH = 4096
_MAX_SOURCE_LENGTH = 512
_MAX_CAPABILITIES = 64
_MAX_CAPABILITY_LENGTH = 128
_MAX_ABS_PRIORITY = 100_000

_LAYER_ORDER = {
    "core": 0,
    "package": 100,
    "user": 200,
    "session": 300,
}


def _is_portable_identifier(value: object, *, max_length: int) -> bool:
    """Return whether ``value`` is stable across Python/Prolog/native adapters."""

    return (
        isinstance(value, str)
        and 0 < len(value) <= max_length
        and all(0x21 <= ord(character) <= 0x7E for character in value)
    )


@dataclass(frozen=True)
class SymbolSpec:
    symbol: str
    kind: str
    value: Any
    priority: int = 0
    docs: str = ""
    capabilities: tuple[str, ...] = ()
    source: str = ""


@dataclass(frozen=True)
class SymbolRegistration:
    registration_id: int
    symbol: str
    kind: str
    owner: str
    layer: str
    priority: int
    sequence: int
    value: Any
    docs: str
    capabilities: tuple[str, ...]
    source: str


@dataclass(frozen=True)
class SymbolDiagnostic:
    registration_id: int
    symbol: str
    kind: str
    owner: str
    layer: str
    priority: int
    sequence: int
    active: bool
    docs: str
    capabilities: tuple[str, ...]
    source: str


class ProgrammableSymbolRegistry:
    """One introspectable namespace with deterministic, reversible overrides.

    Precedence is deterministic:
      core < package < user < session
    then numeric priority, then registration sequence (later wins).

    A package unload removes only registrations owned by that package, causing
    the previous definition to become active automatically.
    """

    def __init__(self) -> None:
        self._lock = threading.RLock()
        self._next_registration_id = 1
        self._next_sequence = 1
        self._registrations: dict[int, SymbolRegistration] = {}

    def register(
        self,
        *,
        symbol: str,
        kind: str,
        owner: str,
        value: Any,
        layer: str = "package",
        priority: int = 0,
        docs: str = "",
        capabilities: Iterable[str] = (),
        source: str = "",
    ) -> int:
        spec = SymbolSpec(
            symbol=symbol,
            kind=kind,
            value=value,
            priority=priority,
            docs=docs,
            capabilities=tuple(capabilities),
            source=source,
        )
        with self._lock:
            normalized = self._validate_spec(spec, owner=owner, layer=layer)
            self._validate_kind_compatibility((normalized,), excluding_owner=None)
            return self._register_unlocked(normalized, owner=owner, layer=layer)

    def replace_owner(
        self,
        owner: str,
        specs: Iterable[SymbolSpec],
        *,
        layer: str = "package",
    ) -> tuple[int, ...]:
        """Atomically replace one owner's generation.

        All new definitions are validated before the live generation changes.
        A failed reload therefore leaves the previous generation active.
        """

        raw_specs = tuple(specs)
        with self._lock:
            normalized = tuple(
                self._validate_spec(spec, owner=owner, layer=layer)
                for spec in raw_specs
            )
            seen_symbols: set[str] = set()
            for spec in normalized:
                if spec.symbol in seen_symbols:
                    raise SymbolRegistrationError(
                        f"owner generation contains duplicate symbol {spec.symbol!r}"
                    )
                seen_symbols.add(spec.symbol)

            self._validate_kind_compatibility(
                normalized,
                excluding_owner=None,
            )

            old_ids = [
                registration_id
                for registration_id, registration in self._registrations.items()
                if registration.owner == owner
            ]
            for registration_id in old_ids:
                del self._registrations[registration_id]

            return tuple(
                self._register_unlocked(spec, owner=owner, layer=layer)
                for spec in normalized
            )

    def unregister(
        self,
        registration_id: Optional[int],
        *,
        owner: Optional[str] = None,
    ) -> bool:
        if registration_id is None:
            return False
        with self._lock:
            registration = self._registrations.get(registration_id)
            if registration is None:
                return False
            if owner is not None and registration.owner != owner:
                return False
            del self._registrations[registration_id]
            return True

    def clear_owner(self, owner: str) -> int:
        self._validate_owner(owner)
        with self._lock:
            registration_ids = [
                registration_id
                for registration_id, registration in self._registrations.items()
                if registration.owner == owner
            ]
            for registration_id in registration_ids:
                del self._registrations[registration_id]
            return len(registration_ids)

    def resolve(self, symbol: str) -> SymbolRegistration:
        self._validate_symbol(symbol)
        with self._lock:
            chain = self._chain_unlocked(symbol)
            if not chain:
                raise SymbolLookupError(f"unknown symbol: {symbol}")
            return chain[-1]

    def get(self, symbol: str) -> Any:
        return self.resolve(symbol).value

    def describe(self, symbol: str) -> tuple[SymbolDiagnostic, ...]:
        self._validate_symbol(symbol)
        with self._lock:
            chain = self._chain_unlocked(symbol)
            if not chain:
                return ()
            active_id = chain[-1].registration_id
            return tuple(
                SymbolDiagnostic(
                    registration_id=registration.registration_id,
                    symbol=registration.symbol,
                    kind=registration.kind,
                    owner=registration.owner,
                    layer=registration.layer,
                    priority=registration.priority,
                    sequence=registration.sequence,
                    active=registration.registration_id == active_id,
                    docs=registration.docs,
                    capabilities=registration.capabilities,
                    source=registration.source,
                )
                for registration in chain
            )

    def symbols(self, *, kind: Optional[str] = None) -> tuple[str, ...]:
        if kind is not None:
            self._validate_kind(kind)
        with self._lock:
            names = {
                registration.symbol
                for registration in self._registrations.values()
                if kind is None or registration.kind == kind
            }
            return tuple(sorted(names))

    def _register_unlocked(
        self,
        spec: SymbolSpec,
        *,
        owner: str,
        layer: str,
    ) -> int:
        registration_id = self._next_registration_id
        sequence = self._next_sequence
        self._next_registration_id += 1
        self._next_sequence += 1
        self._registrations[registration_id] = SymbolRegistration(
            registration_id=registration_id,
            symbol=spec.symbol,
            kind=spec.kind,
            owner=owner,
            layer=layer,
            priority=spec.priority,
            sequence=sequence,
            value=spec.value,
            docs=spec.docs,
            capabilities=spec.capabilities,
            source=spec.source,
        )
        return registration_id

    def _chain_unlocked(self, symbol: str) -> list[SymbolRegistration]:
        registrations = [
            registration
            for registration in self._registrations.values()
            if registration.symbol == symbol
        ]
        return sorted(
            registrations,
            key=lambda registration: (
                _LAYER_ORDER[registration.layer],
                registration.priority,
                registration.sequence,
            ),
        )

    def _validate_kind_compatibility(
        self,
        specs: tuple[SymbolSpec, ...],
        *,
        excluding_owner: Optional[str],
    ) -> None:
        existing_kinds: dict[str, str] = {}
        for registration in self._registrations.values():
            if excluding_owner is not None and registration.owner == excluding_owner:
                continue
            current = existing_kinds.setdefault(registration.symbol, registration.kind)
            if current != registration.kind:
                raise SymbolRegistrationError(
                    f"symbol {registration.symbol!r} already has inconsistent kinds"
                )

        for spec in specs:
            existing_kind = existing_kinds.get(spec.symbol)
            if existing_kind is not None and existing_kind != spec.kind:
                raise SymbolRegistrationError(
                    f"symbol {spec.symbol!r} is already registered as {existing_kind!r}"
                )
            existing_kinds[spec.symbol] = spec.kind

    def _validate_spec(
        self,
        spec: SymbolSpec,
        *,
        owner: str,
        layer: str,
    ) -> SymbolSpec:
        if not isinstance(spec, SymbolSpec):
            raise SymbolRegistrationError("package generation entries must be SymbolSpec")
        self._validate_symbol(spec.symbol)
        self._validate_kind(spec.kind)
        self._validate_owner(owner)
        if layer not in _LAYER_ORDER:
            raise SymbolRegistrationError(
                f"unknown symbol layer {layer!r}; expected one of {tuple(_LAYER_ORDER)}"
            )
        if isinstance(spec.priority, bool) or not isinstance(spec.priority, int):
            raise SymbolRegistrationError("priority must be an integer")
        if abs(spec.priority) > _MAX_ABS_PRIORITY:
            raise SymbolRegistrationError("priority is outside the supported range")
        if not isinstance(spec.docs, str) or len(spec.docs) > _MAX_DOCS_LENGTH:
            raise SymbolRegistrationError("docs must be a bounded string")
        if not isinstance(spec.source, str) or len(spec.source) > _MAX_SOURCE_LENGTH:
            raise SymbolRegistrationError("source must be a bounded string")

        capabilities = tuple(spec.capabilities)
        if len(capabilities) > _MAX_CAPABILITIES:
            raise SymbolRegistrationError("too many capability requirements")
        if any(
            not isinstance(capability, str)
            or not capability
            or len(capability) > _MAX_CAPABILITY_LENGTH
            for capability in capabilities
        ):
            raise SymbolRegistrationError(
                "capabilities must contain non-empty bounded strings"
            )
        if len(set(capabilities)) != len(capabilities):
            raise SymbolRegistrationError("capabilities must not contain duplicates")

        return SymbolSpec(
            symbol=spec.symbol,
            kind=spec.kind,
            value=spec.value,
            priority=spec.priority,
            docs=spec.docs,
            capabilities=capabilities,
            source=spec.source,
        )

    @staticmethod
    def _validate_symbol(symbol: str) -> None:
        if not _is_portable_identifier(symbol, max_length=_MAX_SYMBOL_LENGTH):
            raise SymbolRegistrationError(
                "symbol must be a non-empty bounded portable ASCII token"
            )

    @staticmethod
    def _validate_kind(kind: str) -> None:
        if not _is_portable_identifier(kind, max_length=_MAX_KIND_LENGTH):
            raise SymbolRegistrationError(
                "kind must be a non-empty bounded portable ASCII token"
            )

    @staticmethod
    def _validate_owner(owner: str) -> None:
        if not _is_portable_identifier(owner, max_length=_MAX_OWNER_LENGTH):
            raise SymbolRegistrationError(
                "owner must be a non-empty bounded portable ASCII token"
            )


__all__ = [
    "ProgrammableSymbolRegistry",
    "SymbolDiagnostic",
    "SymbolLookupError",
    "SymbolRegistration",
    "SymbolRegistrationError",
    "SymbolSpec",
]
