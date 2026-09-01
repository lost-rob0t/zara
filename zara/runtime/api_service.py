"""Server api_service providers: versioned registry and plan execution (issue #158).

The registry owns which declared server providers are reachable on this
host; adapters are the only code that executes. Selection stays pure
Prolog (modules/capability_plans.pl); this module builds the server-side
PlanEnvironment (providers from the registry, no device advertisements,
no aliases) and dispatches ready plans through PlanExecutor with typed
timeouts. The ZMQ gateway never touches any of this: execution sits
behind RuntimeHost.
"""

from __future__ import annotations

import asyncio
import logging
import threading
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Callable, Mapping, Optional

from zara.prolog_engine import PrologEngine
from zara.runtime.frames import DurationValue, RefValue, TextValue
from zara.runtime.plans import (
    MAX_PROVIDERS,
    ExecutionPlan,
    PlanEnvironment,
    PlanExecutor,
    PlanOutcome,
    PlanOutcomeStatus,
    PlanStatus,
)

logger = logging.getLogger(__name__)

API_SERVICE_REGISTRY_VERSION = 1

MIN_PROVIDER_TIMEOUT_SECONDS = 0.1
MAX_PROVIDER_TIMEOUT_SECONDS = 60.0

MAX_TIMERS = 64
MAX_TIMER_DURATION_SECONDS = 86400

BUILTIN_PROVIDERS = ("search_server", "timer_server", "admin_restart")


class ProviderSpec:
    """One registered server provider and its execution bounds."""

    __slots__ = ("provider_id", "kind", "timeout_seconds")

    def __init__(self, provider_id: str, kind: str, timeout_seconds: float) -> None:
        if not isinstance(provider_id, str) or not provider_id.strip():
            raise ValueError("provider_id must be a non-empty string")
        if kind != "builtin":
            raise ValueError(f"unsupported provider kind: {kind!r}")
        timeout = float(timeout_seconds)
        if not (
            MIN_PROVIDER_TIMEOUT_SECONDS
            <= timeout
            <= MAX_PROVIDER_TIMEOUT_SECONDS
        ):
            raise ValueError(
                f"timeout_seconds must be within "
                f"[{MIN_PROVIDER_TIMEOUT_SECONDS}, {MAX_PROVIDER_TIMEOUT_SECONDS}]"
            )
        self.provider_id = provider_id
        self.kind = kind
        self.timeout_seconds = timeout

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, ProviderSpec):
            return NotImplemented
        return (
            self.provider_id == other.provider_id
            and self.kind == other.kind
            and self.timeout_seconds == other.timeout_seconds
        )

    def __hash__(self) -> int:
        return hash((self.provider_id, self.kind, self.timeout_seconds))

    def __repr__(self) -> str:
        return (
            f"ProviderSpec(provider_id={self.provider_id!r}, "
            f"kind={self.kind!r}, timeout_seconds={self.timeout_seconds!r})"
        )


class ServiceProviderRegistry:
    """Thread-safe provider reachability and adapter resolution."""

    def __init__(self) -> None:
        self._lock = threading.Lock()
        self._specs: dict[str, ProviderSpec] = {}
        self._adapters: dict[str, Callable[[ExecutionPlan], str]] = {}

    def register(
        self, spec: ProviderSpec, adapter: Callable[[ExecutionPlan], str]
    ) -> None:
        if not callable(adapter):
            raise TypeError(f"adapter for {spec.provider_id!r} must be callable")
        with self._lock:
            if spec.provider_id in self._specs:
                raise ValueError(f"duplicate provider id: {spec.provider_id!r}")
            if len(self._specs) >= MAX_PROVIDERS:
                raise ValueError(f"registry exceeds the {MAX_PROVIDERS}-provider bound")
            self._specs[spec.provider_id] = spec
            self._adapters[spec.provider_id] = adapter

    def unregister(self, provider_id: str) -> None:
        with self._lock:
            if provider_id not in self._specs:
                raise ValueError(f"unknown provider id: {provider_id!r}")
            del self._specs[provider_id]
            del self._adapters[provider_id]

    def provider_ids(self) -> tuple[str, ...]:
        with self._lock:
            return tuple(sorted(self._specs))

    def resolve_adapter(self, provider_id: str) -> Optional[Callable[[ExecutionPlan], str]]:
        with self._lock:
            return self._adapters.get(provider_id)

    def timeout_for(self, provider_id: str) -> Optional[float]:
        with self._lock:
            spec = self._specs.get(provider_id)
        return spec.timeout_seconds if spec is not None else None


class TimerService:
    """Bounded in-process timer table with an injectable clock.

    Policy (issue #158): server timers are recorded service work with due
    tracking and cancellation. Firing delivery (callbacks or events) is a
    later slice; tests drive the clock, never real sleeps.
    """

    def __init__(self, *, clock: Callable[[], float] = time.monotonic) -> None:
        self._clock = clock
        self._lock = threading.Lock()
        self._timers: dict[str, tuple[float, str]] = {}
        self._next_id = 0

    def set(self, duration_seconds: int, label: str, request_id: str) -> str:
        duration = int(duration_seconds)
        if not 0 < duration <= MAX_TIMER_DURATION_SECONDS:
            raise ValueError(
                f"duration must be within (0, {MAX_TIMER_DURATION_SECONDS}] seconds"
            )
        with self._lock:
            if len(self._timers) >= MAX_TIMERS:
                raise ValueError(f"timer table exceeds the {MAX_TIMERS}-timer bound")
            self._next_id += 1
            timer_id = f"timer-{self._next_id}"
            self._timers[timer_id] = (self._clock() + duration, label)
        logger.info("[ApiService] timer %s set for %ss by %s", timer_id, duration, request_id)
        return timer_id

    def cancel(self, timer_id: str) -> bool:
        with self._lock:
            return self._timers.pop(timer_id, None) is not None

    def pending(self) -> tuple[tuple[str, float, str, float], ...]:
        now = self._clock()
        with self._lock:
            return tuple(
                (timer_id, duration, label, due_at)
                for timer_id, (due_at, label) in sorted(self._timers.items())
                for duration in (due_at - now,)
            )

    def due(self) -> tuple[str, ...]:
        now = self._clock()
        with self._lock:
            return tuple(
                timer_id for timer_id, (due_at, _) in sorted(self._timers.items())
                if due_at <= now
            )


def _search_adapter(plan: ExecutionPlan, engine: PrologEngine) -> str:
    query = _text_argument(plan, "query")
    rows = engine.query_once(
        f"config_loader:search_url({_prolog_string(query)}, URL)"
    )
    if not rows or not isinstance(rows.get("URL"), str) or not rows["URL"].strip():
        raise ValueError("search provider produced no URL")
    return rows["URL"]


def _timer_adapter(plan: ExecutionPlan, timers: TimerService) -> str:
    duration = next(
        (argument.value.seconds for argument in plan.arguments if argument.name == "duration"),
        None,
    )
    if not isinstance(duration, int):
        raise ValueError("timer plan carries no duration")
    label_argument = next(
        (argument.value for argument in plan.arguments if argument.name == "label"),
        None,
    )
    label = label_argument.text if isinstance(label_argument, TextValue) else ""
    timer_id = timers.set(duration, label, request_id=plan.provider or "timer")
    return f"timer {timer_id} set for {duration}s"


def _admin_restart_adapter(plan: ExecutionPlan, hook: Optional[Callable[[str], Any]]) -> str:
    if hook is None:
        raise ValueError("admin restart is unavailable on this host")
    hook(plan_side_request_id(plan))
    return "restart requested"


def plan_side_request_id(plan: ExecutionPlan) -> str:
    return f"{plan.intent_ns}.{plan.intent_name}"


def _text_argument(plan: ExecutionPlan, name: str) -> str:
    for argument in plan.arguments:
        if argument.name == name and isinstance(argument.value, TextValue):
            return argument.value.text
    raise ValueError(f"plan carries no text argument {name!r}")


def _prolog_string(value: str) -> str:
    from zara.prolog_engine import _prolog_string as encode

    return encode(value)


class PlanExecutionService:
    """Executes complete frames as typed plans behind RuntimeHost."""

    def __init__(
        self,
        engine: Any,
        registry: ServiceProviderRegistry,
        *,
        executor: Optional[PlanExecutor] = None,
        timers: Optional[TimerService] = None,
        admin_restart_hook: Optional[Callable[[str], Any]] = None,
    ) -> None:
        self.engine = engine
        self.registry = registry
        self.timers = timers if timers is not None else TimerService()
        self._admin_restart_hook = admin_restart_hook
        self._executor = executor if executor is not None else PlanExecutor(
            adapter_resolver=self.registry.resolve_adapter
        )

    @property
    def executor(self) -> PlanExecutor:
        return self._executor

    def environment(self, *, principal: str, auths: tuple[str, ...]) -> PlanEnvironment:
        return PlanEnvironment(
            principal=principal,
            auths=tuple(auths),
            devices=(),
            providers=self.registry.provider_ids(),
            aliases=(),
            policies=(),
        )

    async def execute(
        self,
        frame: Any,
        *,
        principal: str,
        auths: tuple[str, ...] = (),
        request_id: str,
    ) -> PlanOutcome:
        environment = self.environment(principal=principal, auths=tuple(auths))
        plan = await asyncio.to_thread(self.engine.plan_for_frame, frame, environment)
        if plan is None:
            return PlanOutcome(status=PlanOutcomeStatus.REFUSED, detail="selection_failed")
        if plan.status is not PlanStatus.READY:
            return PlanOutcome(
                status=PlanOutcomeStatus.REFUSED,
                detail="plan_not_ready",
                response=_not_ready_response(plan),
            )
        return await self.execute_plan(plan, principal=principal, request_id=request_id)

    async def execute_plan(
        self,
        plan: ExecutionPlan,
        *,
        principal: str,
        auths: tuple[str, ...] = (),
        request_id: str,
    ) -> PlanOutcome:
        timeout = self.registry.timeout_for(plan.provider)
        task = asyncio.create_task(
            asyncio.to_thread(self._executor.execute, plan, request_id)
        )
        try:
            return await asyncio.wait_for(task, timeout=timeout)
        except asyncio.TimeoutError:
            self._executor.track(request_id)
            return PlanOutcome(status=PlanOutcomeStatus.REFUSED, detail="adapter_timeout")
        except asyncio.CancelledError:
            self._executor.track(request_id)
            raise


def _not_ready_response(plan: ExecutionPlan) -> str:
    if plan.status is PlanStatus.AMBIGUOUS:
        return "ambiguous:" + "|".join(plan.alternatives)
    return f"{plan.status.value}:{plan.reason or ''}"


def _specs_from_rows(rows) -> list[ProviderSpec]:
    specs: list[ProviderSpec] = []
    seen: set[str] = set()
    for row in rows:
        provider_id, kind, timeout_seconds = row
        if provider_id in seen:
            raise ValueError(f"duplicate provider id in registry: {provider_id!r}")
        seen.add(provider_id)
        specs.append(ProviderSpec(provider_id, kind, timeout_seconds))
    return specs


def _registry_rows(engine: PrologEngine) -> list[tuple[str, str, int]]:
    version_rows = engine.query_all(
        "kb_server_providers:api_service_registry_version(Version)",
        max_solutions=2,
    )
    versions = {row.get("Version") for row in version_rows}
    if len(versions) != 1:
        raise ValueError("api_service registry version is not declared exactly once")
    declared_version = versions.pop()
    if declared_version != API_SERVICE_REGISTRY_VERSION:
        raise ValueError(
            f"api_service registry version mismatch: KB declares "
            f"{declared_version!r}, runtime expects {API_SERVICE_REGISTRY_VERSION!r}"
        )
    rows = engine.query_all(
        "kb_server_providers:api_service_provider(Id, Kind, Timeout)",
        max_solutions=MAX_PROVIDERS,
    )
    return [
        (str(row["Id"]), str(row["Kind"]), int(row["Timeout"]))
        for row in rows
    ]


def _builtin_adapter(
    provider_id: str,
    *,
    engine: PrologEngine,
    timers: TimerService,
    admin_restart_hook: Optional[Callable[[str], Any]],
) -> Callable[[ExecutionPlan], str]:
    if provider_id == "search_server":
        return lambda plan: _search_adapter(plan, engine)
    if provider_id == "timer_server":
        return lambda plan: _timer_adapter(plan, timers)
    if provider_id == "admin_restart":
        return lambda plan: _admin_restart_adapter(plan, admin_restart_hook)
    raise ValueError(f"no adapter available for builtin provider {provider_id!r}")


def build_api_service(
    config: Mapping[str, Any],
    *,
    engine: Optional[PrologEngine] = None,
    admin_restart_hook: Optional[Callable[[str], Any]] = None,
) -> PlanExecutionService:
    """Build the plan execution service from the server Prolog boot."""
    engine = engine if engine is not None else get_server_engine()
    rows = _registry_rows(engine)
    specs = _specs_from_rows(rows)
    disabled = tuple(config.get("disabled_providers", ()))
    timers = TimerService()
    registry = ServiceProviderRegistry()
    for spec in specs:
        if spec.provider_id in disabled:
            logger.info("[ApiService] provider %s disabled by config", spec.provider_id)
            continue
        adapter = _builtin_adapter(
            spec.provider_id,
            engine=engine,
            timers=timers,
            admin_restart_hook=admin_restart_hook,
        )
        registry.register(spec, adapter)
    logger.info("[ApiService] registered providers: %s", registry.provider_ids())
    return PlanExecutionService(
        engine,
        registry,
        timers=timers,
        admin_restart_hook=admin_restart_hook,
    )


def locate_server_main() -> Path:
    """Locate ``server_main.pl`` across the supported install surfaces."""
    import sys

    candidates = [
        Path.cwd() / "server_main.pl",
        Path(__file__).parent.parent.parent / "server_main.pl",
        Path(sys.prefix) / "share" / "zarathushtra" / "server_main.pl",
        Path("/usr/share/zarathushtra/server_main.pl"),
    ]
    for candidate in candidates:
        if candidate.is_file():
            return candidate.resolve()
    raise FileNotFoundError(f"Could not find server_main.pl. Tried: {candidates}")


_server_engine: Optional[PrologEngine] = None
_server_engine_lock = threading.Lock()


def get_server_engine() -> PrologEngine:
    """Process-wide engine for the server boot.

    PySWIP exposes one process-wide SWI-Prolog runtime; repeated
    instantiation of engines per host build leaks SWI threads and poisons
    the runtime (observed as segfaults in later consults). The server
    boot is consulted at most once per process.
    """
    global _server_engine
    with _server_engine_lock:
        if _server_engine is None:
            _server_engine = PrologEngine(locate_server_main())
        return _server_engine
