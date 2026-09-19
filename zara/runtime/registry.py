"""Canonical backend-neutral runtime discovery and selection state.

The registry is deliberately transport- and implementation-neutral. Concrete
runtime adapters publish bounded observations here; they do not gain execution,
plugin, principal, or secret authority by registering a descriptor.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from enum import Enum
from threading import RLock
from typing import Iterable

ZARA_RUNTIME_PROTOCOL = "ZARA-RUNTIME/1"
_RUNTIME_ID = re.compile(r"^[a-z0-9][a-z0-9._-]{0,63}$")
_PROTOCOL = re.compile(r"^ZARA-RUNTIME/([1-9][0-9]*)$")
_PROVENANCE = re.compile(
    r"^[a-z0-9][a-z0-9._-]{0,63}:[a-z0-9][a-z0-9._-]{0,127}$"
)
_OPAQUE_REF = re.compile(r"^[a-z][a-z0-9._-]{0,31}:[a-z0-9][a-z0-9._-]{0,127}$")


class RuntimeRegistryError(RuntimeError):
    """Base class for bounded runtime registry failures."""


class RuntimeUnavailable(RuntimeRegistryError):
    """Raised when a runtime is absent or cannot currently be selected."""


class IncompatibleRuntimeProtocol(RuntimeRegistryError):
    """Raised when an observed runtime speaks an unsupported protocol major."""


class RuntimeHealth(str, Enum):
    STARTING = "starting"
    READY = "ready"
    BUSY = "busy"
    DEGRADED = "degraded"
    FAILED = "failed"
    STOPPED = "stopped"


class RuntimeLocality(str, Enum):
    EMBEDDED = "embedded"
    LOCAL_PROCESS = "local_process"
    LOCAL_SIDECAR = "local_sidecar"
    REMOTE = "remote"


class RuntimeTransport(str, Enum):
    IN_PROCESS = "in_process"
    STDIO = "stdio"
    LOOPBACK_HTTP = "loopback_http"
    BINDER = "binder"
    ZARA_REMOTE = "zara_remote"


class ControlOwner(str, Enum):
    RUNTIME = "runtime"
    ZARA = "zara"
    MIXED = "mixed"


def _bounded_text(value: str, *, field: str, limit: int, allow_empty: bool = False) -> str:
    if not isinstance(value, str):
        raise TypeError(f"{field} must be a string")
    if len(value) > limit:
        raise ValueError(f"{field} exceeds {limit} characters")
    if not allow_empty and not value:
        raise ValueError(f"{field} must not be empty")
    if value != value.strip():
        raise ValueError(f"{field} must not contain surrounding whitespace")
    if any(ord(char) < 0x20 or ord(char) == 0x7F for char in value):
        raise ValueError(f"{field} contains control characters")
    return value


def _bounded_tokens(values: Iterable[str], *, field: str, limit: int) -> tuple[str, ...]:
    items = tuple(values)
    if len(items) > limit:
        raise ValueError(f"{field} exceeds {limit} entries")
    if len(set(items)) != len(items):
        raise ValueError(f"{field} contains duplicate entries")
    for item in items:
        if not isinstance(item, str) or _RUNTIME_ID.fullmatch(item) is None:
            raise ValueError(f"invalid {field} entry: {item!r}")
    return items


def _opaque_ref(value: str, *, field: str, prefix: str) -> str:
    if not isinstance(value, str):
        raise TypeError(f"{field} must be a string")
    if not value.startswith(prefix) or _OPAQUE_REF.fullmatch(value) is None:
        raise ValueError(f"invalid {field}: expected opaque {prefix!r} host reference")
    return value


def _opaque_refs(
    values: Iterable[str],
    *,
    field: str,
    prefix: str,
    limit: int,
) -> tuple[str, ...]:
    items = tuple(values)
    if len(items) > limit:
        raise ValueError(f"{field} exceeds {limit} entries")
    if len(set(items)) != len(items):
        raise ValueError(f"{field} contains duplicate entries")
    for item in items:
        _opaque_ref(item, field=field, prefix=prefix)
    return items


@dataclass(frozen=True)
class RuntimeDescriptor:
    id: str
    display_name: str
    protocol: str
    runtime_version: str
    implementation_version: str
    installed: bool
    available: bool
    health: RuntimeHealth
    locality: RuntimeLocality
    transport: RuntimeTransport
    capabilities: tuple[str, ...] = ()
    profiles: tuple[str, ...] = ()
    provider_control: ControlOwner = ControlOwner.ZARA
    model_control: ControlOwner = ControlOwner.ZARA
    supports_streaming: bool = False
    supports_cancel: bool = False
    supports_context_handles: bool = False
    supports_host_tools: bool = False
    provenance: str = ""

    def __post_init__(self) -> None:
        if not isinstance(self.id, str) or _RUNTIME_ID.fullmatch(self.id) is None:
            raise ValueError(f"invalid runtime id: {self.id!r}")
        _bounded_text(self.display_name, field="display_name", limit=128)
        protocol = _bounded_text(self.protocol, field="protocol", limit=32)
        if _PROTOCOL.fullmatch(protocol) is None:
            raise ValueError(f"invalid runtime protocol: {protocol!r}")
        _bounded_text(self.runtime_version, field="runtime_version", limit=64)
        _bounded_text(
            self.implementation_version,
            field="implementation_version",
            limit=64,
        )
        for field in (
            "installed",
            "available",
            "supports_streaming",
            "supports_cancel",
            "supports_context_handles",
            "supports_host_tools",
        ):
            if type(getattr(self, field)) is not bool:
                raise TypeError(f"{field} must be a boolean")
        if not isinstance(self.health, RuntimeHealth):
            raise TypeError("health must be RuntimeHealth")
        if not isinstance(self.locality, RuntimeLocality):
            raise TypeError("locality must be RuntimeLocality")
        if not isinstance(self.transport, RuntimeTransport):
            raise TypeError("transport must be RuntimeTransport")
        if not isinstance(self.provider_control, ControlOwner):
            raise TypeError("provider_control must be ControlOwner")
        if not isinstance(self.model_control, ControlOwner):
            raise TypeError("model_control must be ControlOwner")
        provenance = _bounded_text(
            self.provenance,
            field="provenance",
            limit=512,
            allow_empty=True,
        )
        if provenance and _PROVENANCE.fullmatch(provenance) is None:
            raise ValueError(
                "invalid provenance: expected opaque 'source:identity' token without credentials"
            )
        object.__setattr__(
            self,
            "capabilities",
            _bounded_tokens(self.capabilities, field="capabilities", limit=64),
        )
        object.__setattr__(
            self,
            "profiles",
            _bounded_tokens(self.profiles, field="profiles", limit=32),
        )

    @property
    def protocol_compatible(self) -> bool:
        return self.protocol == ZARA_RUNTIME_PROTOCOL

    @property
    def selectable(self) -> bool:
        return (
            self.protocol_compatible
            and self.installed
            and self.available
            and self.health not in {RuntimeHealth.FAILED, RuntimeHealth.STOPPED}
        )

    def to_wire(self) -> dict[str, object]:
        """Return the bounded descriptor shape exposed across host/UI seams."""

        return {
            "id": self.id,
            "display_name": self.display_name,
            "protocol": self.protocol,
            "runtime_version": self.runtime_version,
            "implementation_version": self.implementation_version,
            "installed": self.installed,
            "available": self.available,
            "health": self.health.value,
            "locality": self.locality.value,
            "transport": self.transport.value,
            "capabilities": list(self.capabilities),
            "profiles": list(self.profiles),
            "provider_control": self.provider_control.value,
            "model_control": self.model_control.value,
            "supports_streaming": self.supports_streaming,
            "supports_cancel": self.supports_cancel,
            "supports_context_handles": self.supports_context_handles,
            "supports_host_tools": self.supports_host_tools,
            "provenance": self.provenance,
        }


@dataclass(frozen=True)
class RuntimeSelection:
    runtime_id: str
    generation: int


@dataclass(frozen=True)
class RuntimeInvocationBinding:
    """Host-issued context/capability references bound to one selected generation.

    References are opaque identifiers only. The runtime registry never resolves
    them into principal, plugin, secret, filesystem, shell, eval, or tool
    authority; canonical host stores remain the source of truth.
    """

    runtime_id: str
    generation: int
    context_ref: str
    capability_refs: tuple[str, ...] = ()

    def __post_init__(self) -> None:
        if not isinstance(self.runtime_id, str) or _RUNTIME_ID.fullmatch(self.runtime_id) is None:
            raise ValueError(f"invalid runtime id: {self.runtime_id!r}")
        if (
            not isinstance(self.generation, int)
            or isinstance(self.generation, bool)
            or self.generation <= 0
        ):
            raise ValueError("generation must be a positive integer")
        _opaque_ref(self.context_ref, field="context_ref", prefix="ctx:")
        object.__setattr__(
            self,
            "capability_refs",
            _opaque_refs(
                self.capability_refs,
                field="capability_refs",
                prefix="cap:",
                limit=64,
            ),
        )


@dataclass(frozen=True)
class RuntimeRegistrySnapshot:
    generation: int
    descriptors: tuple[RuntimeDescriptor, ...]
    selection: RuntimeSelection | None


class RuntimeRegistry:
    """Failure-atomic discovery and selection authority for ZARA-RUNTIME/1."""

    def __init__(self) -> None:
        self._lock = RLock()
        self._generation = 0
        self._descriptors: dict[str, RuntimeDescriptor] = {}
        self._selection: RuntimeSelection | None = None

    def refresh(self, observed: Iterable[RuntimeDescriptor]) -> RuntimeRegistrySnapshot:
        """Atomically replace the observed runtime set.

        A semantic discovery change advances the generation. Re-publishing the
        same descriptor set is a no-op even if observation order differs, so
        harmless polling cannot stale an in-flight binding. A previously selected
        runtime survives a changed observation only if the fresh descriptor is
        still selectable; changed observations always fence the prior generation.
        """

        replacement: dict[str, RuntimeDescriptor] = {}
        for descriptor in observed:
            if not isinstance(descriptor, RuntimeDescriptor):
                raise TypeError("runtime discovery must yield RuntimeDescriptor values")
            if descriptor.id in replacement:
                raise ValueError(f"duplicate runtime id: {descriptor.id}")
            replacement[descriptor.id] = descriptor

        with self._lock:
            if replacement == self._descriptors:
                return self._snapshot_unlocked()

            next_generation = self._generation + 1
            current_id = self._selection.runtime_id if self._selection is not None else None
            current = replacement.get(current_id) if current_id is not None else None
            selection = (
                RuntimeSelection(current_id, next_generation)
                if current is not None and current.selectable
                else None
            )
            self._descriptors = replacement
            self._generation = next_generation
            self._selection = selection
            return self._snapshot_unlocked()

    def discover(self) -> tuple[RuntimeDescriptor, ...]:
        with self._lock:
            return self._sorted_descriptors_unlocked()

    def selectable(self) -> tuple[RuntimeDescriptor, ...]:
        with self._lock:
            return tuple(
                descriptor
                for descriptor in self._sorted_descriptors_unlocked()
                if descriptor.selectable
            )

    def health(self, runtime_id: str) -> RuntimeHealth:
        return self.capabilities(runtime_id).health

    def capabilities(self, runtime_id: str) -> RuntimeDescriptor:
        with self._lock:
            descriptor = self._descriptors.get(runtime_id)
            if descriptor is None:
                raise RuntimeUnavailable(f"runtime {runtime_id!r} is not discovered")
            return descriptor

    def select(self, runtime_id: str) -> RuntimeSelection:
        if runtime_id == "auto":
            raise RuntimeUnavailable("'auto' is routing policy, not a runtime identity")
        with self._lock:
            descriptor = self._descriptors.get(runtime_id)
            if descriptor is None:
                raise RuntimeUnavailable(f"runtime {runtime_id!r} is not discovered")
            if not descriptor.protocol_compatible:
                raise IncompatibleRuntimeProtocol(
                    f"runtime {runtime_id!r} uses incompatible protocol {descriptor.protocol!r}"
                )
            if not descriptor.selectable:
                raise RuntimeUnavailable(f"runtime {runtime_id!r} is not selectable")
            self._generation += 1
            self._selection = RuntimeSelection(runtime_id, self._generation)
            return self._selection

    def current(self) -> RuntimeSelection | None:
        with self._lock:
            return self._selection

    def bind_invocation(
        self,
        context_ref: str,
        *,
        capability_refs: Iterable[str] = (),
    ) -> RuntimeInvocationBinding:
        """Bind opaque host authority references to the current runtime generation.

        This is a fence, not an authorization lookup. Callers must resolve and
        re-check the refs through Zara's canonical principal/plugin/approval stores
        immediately before exercising any privileged capability.
        """

        with self._lock:
            selection = self._selection
            if selection is None:
                raise RuntimeUnavailable("no runtime is selected")
            return RuntimeInvocationBinding(
                runtime_id=selection.runtime_id,
                generation=selection.generation,
                context_ref=context_ref,
                capability_refs=tuple(capability_refs),
            )

    def accepts_binding(
        self,
        binding: RuntimeInvocationBinding,
        *,
        context_ref: str,
    ) -> bool:
        """Return whether a binding is current and belongs to this exact context."""

        if not isinstance(binding, RuntimeInvocationBinding):
            return False
        try:
            expected_context = _opaque_ref(context_ref, field="context_ref", prefix="ctx:")
        except (TypeError, ValueError):
            return False
        with self._lock:
            selection = self._selection
            return bool(
                selection is not None
                and binding.runtime_id == selection.runtime_id
                and binding.generation == selection.generation
                and binding.generation == self._generation
                and binding.context_ref == expected_context
            )

    def accepts_generation(self, runtime_id: str, generation: int) -> bool:
        with self._lock:
            selection = self._selection
            return bool(
                selection is not None
                and selection.runtime_id == runtime_id
                and selection.generation == generation
                and generation == self._generation
            )

    def snapshot(self) -> RuntimeRegistrySnapshot:
        with self._lock:
            return self._snapshot_unlocked()

    def _snapshot_unlocked(self) -> RuntimeRegistrySnapshot:
        return RuntimeRegistrySnapshot(
            generation=self._generation,
            descriptors=self._sorted_descriptors_unlocked(),
            selection=self._selection,
        )

    def _sorted_descriptors_unlocked(self) -> tuple[RuntimeDescriptor, ...]:
        return tuple(self._descriptors[key] for key in sorted(self._descriptors))
