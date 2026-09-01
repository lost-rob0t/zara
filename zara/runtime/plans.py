"""Typed ExecutionPlan mirror of the capability layer (issue #157).

Plans are pure values produced by the pure Prolog selection in
modules/capability_plans.pl and decoded through flat projections. They name
registered capabilities only; concrete launch targets are adapter/platform
state and never ride the plan. Envelope metadata (request ids, principals,
turns) stays outside the plan; the executor consumes request ids for
at-most-once side effects per envelope contract.

Unavailable/denied/ambiguous are plan-level typed outcomes (contract
docs/intentframe-contract.md example 12) - frames are never rewritten with a
fabricated semantic status.
"""

from __future__ import annotations

import enum
from collections import deque
from dataclasses import dataclass, field
from typing import Callable, Mapping, Optional, Union

from zara.runtime.frames import SlotValue, TextValue, validate_value

MAX_TEXT_ARGUMENT_CHARS = 512

MAX_DEVICES = 64
MAX_PROVIDERS = 64
MAX_ALIASES = 256
MAX_CAPABILITIES_PER_DEVICE = 64
MAX_AUTHS = 16
MAX_POLICIES = 8


class PlanStatus(str, enum.Enum):
    READY = "ready"
    UNAVAILABLE = "unavailable"
    AMBIGUOUS = "ambiguous"
    DENIED = "denied"


class PlanLocation(str, enum.Enum):
    SERVER = "server"
    DEVICE = "device"


class PlanSideEffect(str, enum.Enum):
    NONE = "none"
    LOCAL = "local"
    EXTERNAL = "external"


@dataclass(frozen=True)
class PlanArgument:
    name: str
    value: SlotValue


@dataclass(frozen=True)
class ExecutionPlan:
    intent_ns: str
    intent_name: str
    provider: Optional[str] = None
    location: Optional[PlanLocation] = None
    device: Optional[str] = None
    side_effect: PlanSideEffect = PlanSideEffect.NONE
    requires_auth: Optional[str] = None
    status: PlanStatus = PlanStatus.UNAVAILABLE
    reason: Optional[str] = None
    alternatives: tuple[str, ...] = ()
    arguments: tuple[PlanArgument, ...] = ()
    evidence: tuple[str, ...] = ()

    def __post_init__(self) -> None:
        if not isinstance(self.status, PlanStatus):
            raise ValueError(f"unsupported plan status: {self.status!r}")
        if not isinstance(self.side_effect, PlanSideEffect):
            raise ValueError(f"unsupported side effect: {self.side_effect!r}")
        if self.location is not None and not isinstance(self.location, PlanLocation):
            raise ValueError(f"unsupported plan location: {self.location!r}")
        if self.requires_auth is not None and not self.requires_auth.strip():
            raise ValueError("requires_auth must be a non-empty capability id")
        for item in self.evidence:
            if not isinstance(item, str):
                raise ValueError("plan evidence must be strings")
        if self.status is PlanStatus.READY:
            self._validate_ready()
        else:
            self._validate_not_ready()
        for argument in self.arguments:
            validate_plan_argument(argument)

    def _validate_ready(self) -> None:
        if not isinstance(self.provider, str) or not self.provider.strip():
            raise ValueError("ready plans name a registered provider")
        if self.location is None:
            raise ValueError("ready plans carry a location")
        if self.location is PlanLocation.DEVICE and not (
            isinstance(self.device, str) and self.device.strip()
        ):
            raise ValueError("device plans name a device")
        if self.location is PlanLocation.SERVER and self.device is not None:
            raise ValueError("server plans must not name a device")
        if self.reason is not None:
            raise ValueError("ready plans carry no reason")
        if self.alternatives:
            raise ValueError("ready plans carry no alternatives")

    def _validate_not_ready(self) -> None:
        if self.provider is not None or self.location is not None or self.device is not None:
            raise ValueError(f"{self.status.value} plans carry no provider selection")
        if self.arguments:
            raise ValueError(f"{self.status.value} plans carry no arguments")
        if self.status in {PlanStatus.UNAVAILABLE, PlanStatus.DENIED}:
            if not isinstance(self.reason, str) or not self.reason.strip():
                raise ValueError(f"{self.status.value} plans carry a typed reason")
        if self.status is PlanStatus.AMBIGUOUS:
            if self.reason is not None:
                raise ValueError("ambiguous plans carry no reason")
            if not self.alternatives:
                raise ValueError("ambiguous plans list alternatives")


def validate_plan_argument(argument: PlanArgument) -> None:
    if not isinstance(argument.name, str) or not argument.name.strip():
        raise ValueError("plan arguments name a provider argument")
    reason = validate_value(argument.value)
    if reason is not None:
        raise ValueError(f"invalid plan argument {argument.name!r}: {reason}")
    if isinstance(argument.value, TextValue) and len(argument.value.text) > MAX_TEXT_ARGUMENT_CHARS:
        raise ValueError(
            f"plan argument {argument.name!r} exceeds "
            f"{MAX_TEXT_ARGUMENT_CHARS}-character bound"
        )


@dataclass(frozen=True)
class DeviceAdvertisement:
    device_id: str
    owner: str
    capabilities: tuple[str, ...]

    def __post_init__(self) -> None:
        if not isinstance(self.device_id, str) or not self.device_id.strip():
            raise ValueError("device advertisement needs a device_id")
        if not isinstance(self.owner, str) or not self.owner.strip():
            raise ValueError("device advertisement needs an owner")
        if not self.capabilities:
            raise ValueError("device advertisement needs capabilities")
        if len(self.capabilities) > MAX_CAPABILITIES_PER_DEVICE:
            raise ValueError(
                f"device capabilities exceed the {MAX_CAPABILITIES_PER_DEVICE} bound"
            )
        for capability in self.capabilities:
            if not isinstance(capability, str) or not capability.strip():
                raise ValueError("device capabilities are non-empty strings")


@dataclass(frozen=True)
class PreferLocation:
    location: PlanLocation


@dataclass(frozen=True)
class PreferDevice:
    device_id: str


PlanPolicy = Union[PreferLocation, PreferDevice]


@dataclass(frozen=True)
class PlanEnvironment:
    principal: str
    auths: tuple[str, ...] = ()
    devices: tuple[DeviceAdvertisement, ...] = ()
    providers: tuple[str, ...] = ()
    aliases: tuple[tuple[str, str], ...] = ()
    policies: tuple[PlanPolicy, ...] = ()

    def __post_init__(self) -> None:
        if not isinstance(self.principal, str) or not self.principal.strip():
            raise ValueError("plan environment needs a principal")
        if len(self.auths) > MAX_AUTHS:
            raise ValueError(f"auths exceed the {MAX_AUTHS} bound")
        for auth in self.auths:
            if not isinstance(auth, str) or not auth.strip():
                raise ValueError("auths are non-empty capability ids")
        if len(self.devices) > MAX_DEVICES:
            raise ValueError(f"devices exceed the {MAX_DEVICES} bound")
        if len(self.providers) > MAX_PROVIDERS:
            raise ValueError(f"providers exceed the {MAX_PROVIDERS} bound")
        for provider in self.providers:
            if not isinstance(provider, str) or not provider.strip():
                raise ValueError("providers are non-empty ids")
        if len(self.aliases) > MAX_ALIASES:
            raise ValueError(f"aliases exceed the {MAX_ALIASES} bound")
        for provider, alias in self.aliases:
            if not isinstance(provider, str) or not provider.strip():
                raise ValueError("alias rows name a provider")
            if not isinstance(alias, str) or not alias.strip():
                raise ValueError("alias rows carry a non-empty alias")
        if len(self.policies) > MAX_POLICIES:
            raise ValueError(f"policies exceed the {MAX_POLICIES} bound")
        for policy in self.policies:
            if isinstance(policy, PreferLocation):
                if not isinstance(policy.location, PlanLocation):
                    raise ValueError("prefer-location policies carry a PlanLocation")
            elif isinstance(policy, PreferDevice):
                if not isinstance(policy.device_id, str) or not policy.device_id.strip():
                    raise ValueError("prefer-device policies name a device")
            else:
                raise ValueError("policies are PreferLocation or PreferDevice")


class PlanOutcomeStatus(str, enum.Enum):
    EXECUTED = "executed"
    REPLAYED = "replayed"
    REFUSED = "refused"


@dataclass(frozen=True)
class PlanOutcome:
    status: PlanOutcomeStatus
    detail: str
    response: str = ""


PlanAdapter = Callable[[ExecutionPlan], str]


class PlanExecutor:
    """Dispatches typed plans to registered capability adapters.

    Adapters receive the ExecutionPlan object and validate its typed values
    for their platform; text arguments are data, never commands. Side
    effects execute at most once per request_id per executor lifetime; the
    replay window is bounded (persistence is the envelope store's concern,
    see rage/157 adversarial review attack 5). Refusals are typed outcomes,
    never exceptions; adapter failures do not crash the caller.
    """

    def __init__(
        self,
        adapters: Optional[Mapping[str, PlanAdapter]] = None,
        *,
        adapter_resolver: Optional[Callable[[str], Optional[PlanAdapter]]] = None,
        max_tracked: int = 4096,
    ) -> None:
        if (adapters is None) == (adapter_resolver is None):
            raise ValueError("provide either adapters or adapter_resolver, not both")
        if max_tracked < 1:
            raise ValueError("max_tracked must be at least 1")
        self._adapters = dict(adapters) if adapters is not None else None
        if self._adapters is not None:
            for provider, adapter in self._adapters.items():
                if not callable(adapter):
                    raise TypeError(f"adapter for {provider!r} must be callable")
        self._adapter_resolver = adapter_resolver
        self._max_tracked = max_tracked
        self._recent: deque[str] = deque()
        self._seen: set[str] = set()

    def _resolve_adapter(self, provider: str) -> Optional[PlanAdapter]:
        if self._adapter_resolver is not None:
            return self._adapter_resolver(provider)
        return self._adapters.get(provider)  # type: ignore[union-attr]

    def track(self, request_id: str) -> None:
        """Record a request id as possibly side-effected (timeout path)."""
        self._track(request_id)

    def execute(self, plan: ExecutionPlan, request_id: str) -> PlanOutcome:
        if not isinstance(plan, ExecutionPlan):
            raise TypeError("PlanExecutor executes ExecutionPlan instances only")
        if not isinstance(request_id, str) or not request_id.strip():
            raise ValueError("request_id is required for plan execution")
        if plan.status is not PlanStatus.READY:
            return PlanOutcome(
                status=PlanOutcomeStatus.REFUSED, detail="plan_not_ready"
            )
        adapter = self._resolve_adapter(plan.provider)
        if adapter is None:
            return PlanOutcome(
                status=PlanOutcomeStatus.REFUSED, detail="unknown_provider"
            )
        if request_id in self._seen:
            return PlanOutcome(
                status=PlanOutcomeStatus.REPLAYED, detail="replay"
            )
        try:
            response = adapter(plan)
        except Exception as error:
            return PlanOutcome(
                status=PlanOutcomeStatus.REFUSED,
                detail="adapter_failed",
                response=str(error),
            )
        self._track(request_id)
        return PlanOutcome(
            status=PlanOutcomeStatus.EXECUTED, detail="ok", response=response
        )

    def _track(self, request_id: str) -> None:
        if len(self._recent) >= self._max_tracked:
            evicted = self._recent.popleft()
            self._seen.discard(evicted)
        self._recent.append(request_id)
        self._seen.add(request_id)
