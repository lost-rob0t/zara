"""Tests for the server api_service provider registry and plan execution (issue #158)."""

import asyncio
import threading
import time
from pathlib import Path

import pytest
from pytest import MonkeyPatch

from zara.runtime.api_service import (
    API_SERVICE_REGISTRY_VERSION,
    PlanExecutionService,
    ProviderSpec,
    ServiceProviderRegistry,
    TimerService,
    _specs_from_rows,
    build_api_service,
    get_server_engine,
)
from zara.runtime.frames import (
    DurationValue,
    FilledSlot,
    FrameStatus,
    IntentFrame,
    RefValue,
    SlotOrigin,
    TextValue,
)
from zara.runtime.plans import (
    ExecutionPlan,
    PlanArgument,
    PlanLocation,
    PlanOutcomeStatus,
    PlanSideEffect,
    PlanStatus,
)
from zara.prolog_engine import PrologEngine

REPO_ROOT = Path(__file__).resolve().parent.parent
SERVER_MAIN = REPO_ROOT / "server_main.pl"


@pytest.fixture(scope="module", autouse=True)
def isolated_user_config(tmp_path_factory):
    config_home = tmp_path_factory.mktemp("config")
    (config_home / "zarathushtra").mkdir(parents=True)
    with MonkeyPatch.context() as mp:
        mp.setenv("XDG_CONFIG_HOME", str(config_home))
        yield


def complete_frame(intent_ns: str, intent_name: str, *slots) -> IntentFrame:
    return IntentFrame(
        intent_ns=intent_ns,
        intent_name=intent_name,
        slots=tuple(slots),
        status=FrameStatus.COMPLETE,
    )


def text_slot(name: str, text: str) -> FilledSlot:
    return FilledSlot(name=name, value=TextValue(text=text), origin=SlotOrigin.UTTERANCE)


@pytest.fixture(scope="module")
def engine() -> PrologEngine:
    return PrologEngine(SERVER_MAIN)


def make_spec(provider_id: str = "search_server", timeout_seconds: float = 5.0) -> ProviderSpec:
    return ProviderSpec(
        provider_id=provider_id,
        kind="builtin",
        timeout_seconds=timeout_seconds,
    )


class TestProviderSpec:
    def test_rejects_empty_provider_id(self):
        with pytest.raises(ValueError):
            make_spec(provider_id="  ")

    def test_rejects_unknown_kind(self):
        with pytest.raises(ValueError):
            ProviderSpec(provider_id="x", kind="shell", timeout_seconds=1.0)

    @pytest.mark.parametrize("timeout_seconds", [0.0, -1.0, 61.0])
    def test_rejects_timeout_outside_bounds(self, timeout_seconds):
        with pytest.raises(ValueError):
            ProviderSpec(provider_id="x", kind="builtin", timeout_seconds=timeout_seconds)


class TestServiceProviderRegistry:
    def test_register_and_resolve(self):
        registry = ServiceProviderRegistry()
        adapter = lambda plan: "ok"
        registry.register(make_spec(), adapter)
        assert registry.provider_ids() == ("search_server",)
        assert registry.resolve_adapter("search_server") is adapter

    def test_register_rejects_duplicate_id(self):
        registry = ServiceProviderRegistry()
        registry.register(make_spec(), lambda plan: "ok")
        with pytest.raises(ValueError):
            registry.register(make_spec(), lambda plan: "other")

    def test_unregister_removes_adapter(self):
        registry = ServiceProviderRegistry()
        registry.register(make_spec(), lambda plan: "ok")
        registry.unregister("search_server")
        assert registry.provider_ids() == ()
        assert registry.resolve_adapter("search_server") is None

    def test_unregister_unknown_id_rejected(self):
        registry = ServiceProviderRegistry()
        with pytest.raises(ValueError):
            registry.unregister("missing")

    def test_timeout_for_provider(self):
        registry = ServiceProviderRegistry()
        registry.register(make_spec(timeout_seconds=2.5), lambda plan: "ok")
        assert registry.timeout_for("search_server") == 2.5
        assert registry.timeout_for("missing") is None

    def test_register_unregister_race_keeps_registry_consistent(self):
        registry = ServiceProviderRegistry()
        errors: list[Exception] = []

        def worker(n: int) -> None:
            try:
                for round_index in range(50):
                    spec = make_spec(provider_id=f"provider_{n}")
                    registry.register(spec, lambda plan: "ok")
                    registry.unregister(f"provider_{n}")
            except Exception as error:  # pragma: no cover - race evidence
                errors.append(error)

        threads = [threading.Thread(target=worker, args=(n,)) for n in range(8)]
        for thread in threads:
            thread.start()
        for thread in threads:
            thread.join()
        assert errors == []
        assert registry.provider_ids() == ()


class TestTimerService:
    def test_set_records_timer_with_virtual_clock(self):
        now = [100.0]
        timers = TimerService(clock=lambda: now[0])
        timer_id = timers.set(30, "eggs", "request-1")
        assert timers.pending() == ((timer_id, 30, "eggs", 130.0),)

    def test_advance_marks_due(self):
        now = [100.0]
        timers = TimerService(clock=lambda: now[0])
        timers.set(30, "eggs", "request-1")
        now[0] = 129.0
        assert timers.due() == ()
        now[0] = 130.0
        assert len(timers.due()) == 1

    @pytest.mark.parametrize("duration", [0, -5, 86401])
    def test_duration_bounds(self, duration):
        timers = TimerService(clock=lambda: 100.0)
        with pytest.raises(ValueError):
            timers.set(duration, "bad", "request-1")

    def test_timer_table_is_bounded(self):
        timers = TimerService(clock=lambda: 100.0)
        for index in range(64):
            timers.set(index + 1, f"t{index}", f"request-{index}")
        with pytest.raises(ValueError):
            timers.set(1, "over", "request-over")

    def test_cancel_removes_pending_timer(self):
        timers = TimerService(clock=lambda: 100.0)
        timer_id = timers.set(30, "eggs", "request-1")
        assert timers.cancel(timer_id) is True
        assert timers.pending() == ()
        assert timers.cancel(timer_id) is False

    def test_cancelled_timers_never_become_due(self):
        timers = TimerService(clock=lambda: 100.0)
        timer_id = timers.set(10, "eggs", "request-1")
        timers.cancel(timer_id)
        assert timers.due() == ()


class TestBuildApiService:
    def test_registers_builtin_server_providers(self, engine):
        service = build_api_service(
            {"enabled": True, "disabled_providers": []}, engine=engine
        )
        assert set(service.registry.provider_ids()) == {
            "search_server",
            "timer_server",
            "admin_restart",
        }

    def test_disabled_providers_are_not_registered(self, engine):
        service = build_api_service(
            {"enabled": True, "disabled_providers": ["timer_server"]}, engine=engine
        )
        assert "timer_server" not in service.registry.provider_ids()
        assert "search_server" in service.registry.provider_ids()

    def test_version_mismatch_fails_closed(self, engine, monkeypatch):
        monkeypatch.setattr(
            "zara.runtime.api_service.API_SERVICE_REGISTRY_VERSION",
            API_SERVICE_REGISTRY_VERSION + 1,
        )
        with pytest.raises(ValueError, match="registry version"):
            build_api_service({"enabled": True, "disabled_providers": []}, engine=engine)

    def test_malformed_rows_fail_closed(self):
        with pytest.raises(ValueError, match="duplicate"):
            _specs_from_rows(
                [
                    ("search_server", "builtin", 5),
                    ("search_server", "builtin", 5),
                ]
            )
        with pytest.raises(ValueError, match="kind"):
            _specs_from_rows([("mystery", "shell", 5)])
        with pytest.raises(ValueError, match="timeout"):
            _specs_from_rows([("slow", "builtin", 0)])
        with pytest.raises(ValueError, match="provider_id"):
            _specs_from_rows([("", "builtin", 5)])

    def test_missing_engine_file_fails(self, tmp_path):
        with pytest.raises(Exception):
            build_api_service(
                {"enabled": True, "disabled_providers": []},
                engine=PrologEngine(tmp_path / "missing.pl"),
            )


class TestPlanExecutionService:
    @pytest.fixture(autouse=True)
    def _service_engine(self, engine):
        self._engine = engine

    def service(self, **kwargs) -> PlanExecutionService:
        return build_api_service(
            {"enabled": True, "disabled_providers": []}, engine=self._engine, **kwargs
        )

    def test_search_plan_executes_and_returns_url(self):
        service = self.service()
        frame = complete_frame("web", "search", text_slot("query", "prolog test"))
        outcome = asyncio.run(
            service.execute(
                frame, principal="alice", auths=(), request_id="req-search-1"
            )
        )
        assert outcome.status is PlanOutcomeStatus.EXECUTED
        assert "search.brave.com" in outcome.response

    def test_open_app_plan_is_unavailable_on_headless_server(self):
        service = self.service()
        frame = complete_frame(
            "app", "open", FilledSlot(
                name="target",
                value=RefValue(kind="app_alias", id="firefox"),
                origin=SlotOrigin.UTTERANCE,
            )
        )
        outcome = asyncio.run(
            service.execute(frame, principal="alice", auths=(), request_id="req-open-1")
        )
        assert outcome.status is PlanOutcomeStatus.REFUSED
        assert outcome.detail == "plan_not_ready"
        assert "unavailable" in outcome.response

    def test_denied_without_admin_authorization(self):
        service = self.service()
        frame = complete_frame("skill", "admin.restart")
        outcome = asyncio.run(
            service.execute(frame, principal="alice", auths=(), request_id="req-admin-1")
        )
        assert outcome.status is PlanOutcomeStatus.REFUSED
        assert outcome.detail == "plan_not_ready"
        assert "denied" in outcome.response

    def test_admin_plan_executes_with_hook_and_authorization(self):
        restarts: list[str] = []
        service = self.service(admin_restart_hook=restarts.append)
        frame = complete_frame("skill", "admin.restart")
        outcome = asyncio.run(
            service.execute(
                frame,
                principal="alice",
                auths=("daemon.admin",),
                request_id="req-admin-2",
            )
        )
        assert outcome.status is PlanOutcomeStatus.EXECUTED
        assert restarts == ["skill.admin.restart"]

    def test_timer_plan_records_service_work(self):
        service = self.service()
        frame = complete_frame(
            "device",
            "timer.set",
            FilledSlot(
                name="duration",
                value=DurationValue(seconds=90),
                origin=SlotOrigin.UTTERANCE,
            ),
            FilledSlot(
                name="label",
                value=TextValue(text="tea"),
                origin=SlotOrigin.UTTERANCE,
            ),
        )
        outcome = asyncio.run(
            service.execute(frame, principal="alice", auths=(), request_id="req-timer-1")
        )
        assert outcome.status is PlanOutcomeStatus.EXECUTED
        assert len(service.timers.pending()) == 1

    def test_timer_retry_replays_without_duplicate_side_effect(self):
        service = self.service()
        frame = complete_frame(
            "device",
            "timer.set",
            FilledSlot(
                name="duration",
                value=DurationValue(seconds=90),
                origin=SlotOrigin.UTTERANCE,
            ),
        )
        first = asyncio.run(
            service.execute(frame, principal="alice", auths=(), request_id="req-timer-2")
        )
        second = asyncio.run(
            service.execute(frame, principal="alice", auths=(), request_id="req-timer-2")
        )
        assert first.status is PlanOutcomeStatus.EXECUTED
        assert second.status is PlanOutcomeStatus.REPLAYED
        assert len(service.timers.pending()) == 1

    def test_unregistered_provider_leaves_plan_unavailable(self):
        service = self.service()
        service.registry.unregister("timer_server")
        frame = complete_frame(
            "device",
            "timer.set",
            FilledSlot(
                name="duration",
                value=DurationValue(seconds=90),
                origin=SlotOrigin.UTTERANCE,
            ),
        )
        outcome = asyncio.run(
            service.execute(frame, principal="alice", auths=(), request_id="req-timer-3")
        )
        assert outcome.status is PlanOutcomeStatus.REFUSED
        assert outcome.detail == "plan_not_ready"

    def test_executor_refuses_hand_built_plan_for_unregistered_provider(self):
        service = self.service()
        plan = ExecutionPlan(
            intent_ns="app",
            intent_name="open",
            provider="open_app",
            location=PlanLocation.DEVICE,
            device="phone",
            side_effect=PlanSideEffect.EXTERNAL,
            status=PlanStatus.READY,
            arguments=(PlanArgument("app", RefValue(kind="app_alias", id="firefox")),),
        )
        outcome = asyncio.run(
            service.execute_plan(plan, principal="alice", request_id="req-open-2")
        )
        assert outcome.status is PlanOutcomeStatus.REFUSED
        assert outcome.detail == "unknown_provider"

    def test_open_frame_is_refused_at_selection_boundary(self):
        service = self.service()
        frame = IntentFrame(
            intent_ns="web",
            intent_name="search",
            slots=(),
            status=FrameStatus.MISSING,
            missing=("query",),
        )
        with pytest.raises(ValueError):
            asyncio.run(
                service.execute(
                    frame, principal="alice", auths=(), request_id="req-open-frame"
                )
            )

    def test_selection_failure_is_typed_refusal(self):
        class FakeEngine:
            def query_all(self, goal, max_solutions=-1):
                if "registry_version" in goal:
                    return [{"Version": 1}]
                return [
                    {"Id": "search_server", "Kind": "builtin", "Timeout": 5},
                    {"Id": "timer_server", "Kind": "builtin", "Timeout": 5},
                    {"Id": "admin_restart", "Kind": "builtin", "Timeout": 5},
                ]

            def plan_for_frame(self, frame, environment):
                return None

        service = build_api_service(
            {"enabled": True, "disabled_providers": []}, engine=FakeEngine()
        )
        frame = complete_frame("web", "search", text_slot("query", "anything"))
        outcome = asyncio.run(
            service.execute(frame, principal="alice", auths=(), request_id="req-none")
        )
        assert outcome.status is PlanOutcomeStatus.REFUSED
        assert outcome.detail == "selection_failed"

    def test_adapter_timeout_is_tracked_so_retry_replays(self):
        service = self.service()
        service.registry.unregister("search_server")
        service.registry.register(
            ProviderSpec(provider_id="search_server", kind="builtin", timeout_seconds=0.1),
            lambda plan: time.sleep(0.5) or "late",
        )
        frame = complete_frame("web", "search", text_slot("query", "slow"))
        outcome = asyncio.run(
            service.execute(frame, principal="alice", auths=(), request_id="req-slow")
        )
        assert outcome.status is PlanOutcomeStatus.REFUSED
        assert outcome.detail == "adapter_timeout"
        replay = asyncio.run(
            service.execute(frame, principal="alice", auths=(), request_id="req-slow")
        )
        assert replay.status is PlanOutcomeStatus.REPLAYED

    def test_admin_adapter_without_hook_refuses(self):
        service = self.service()
        frame = complete_frame("skill", "admin.restart")
        outcome = asyncio.run(
            service.execute(
                frame,
                principal="alice",
                auths=("daemon.admin",),
                request_id="req-admin-3",
            )
        )
        assert outcome.status is PlanOutcomeStatus.REFUSED
        assert outcome.detail == "adapter_failed"
