from __future__ import annotations

from dataclasses import replace

import pytest

from zara.expert_port import CanonicalExpertInvocationPort
from zara.runtime.frames import (
    BoolValue,
    DateTimeValue,
    DurationValue,
    NumberValue,
    RefValue,
    TextValue,
    validate_value,
)
from zara.runtime.package_profiles import (
    PACKAGE_PROFILE_SCHEMA,
    AppPackageProfile,
    PackageProfileError,
    activate_profile_package,
)
from zara.runtime.plans import (
    DeviceAdvertisement,
    ExecutionPlan,
    PlanArgument,
    PlanEnvironment,
    PlanExecutor,
    PlanLocation,
    PlanSideEffect,
    PlanStatus,
    PreferDevice,
    PreferLocation,
)
from zara.runtime.registry import (
    ControlOwner,
    RuntimeDescriptor,
    RuntimeHealth,
    RuntimeInvocationBinding,
    RuntimeLocality,
    RuntimeRegistry,
    RuntimeTransport,
    RuntimeUnavailable,
    ZARA_RUNTIME_PROTOCOL,
)
from zara.runtime.symbols import (
    ProgrammableSymbolRegistry,
    SymbolRegistrationError,
    SymbolSpec,
)


def _descriptor(**overrides) -> RuntimeDescriptor:
    fields = dict(
        id="zara-python",
        display_name="Zara Python",
        protocol=ZARA_RUNTIME_PROTOCOL,
        runtime_version="1",
        implementation_version="1",
        installed=True,
        available=True,
        health=RuntimeHealth.READY,
        locality=RuntimeLocality.EMBEDDED,
        transport=RuntimeTransport.IN_PROCESS,
        capabilities=("chat",),
        profiles=("default",),
        provider_control=ControlOwner.ZARA,
        model_control=ControlOwner.ZARA,
        supports_streaming=False,
        supports_cancel=True,
        supports_context_handles=True,
        supports_host_tools=False,
        provenance="core:zara-python",
    )
    fields.update(overrides)
    return RuntimeDescriptor(**fields)


@pytest.mark.parametrize(
    ("value", "reason"),
    [
        (TextValue(" "), "empty"),
        (NumberValue(float("nan")), "non_finite"),
        (NumberValue(True), "not_a_number"),
        (NumberValue("12"), "not_a_number"),
        (DurationValue(True), "not_an_integer"),
        (DurationValue(1.5), "not_an_integer"),
        (DurationValue(-1), "negative"),
        (DateTimeValue(2026, 1, 0, 0, 0, 0), "day_range"),
        (DateTimeValue(2026, 1, 1, 24, 0, 0), "hour_range"),
        (DateTimeValue(2026, 1, 1, 0, 60, 0), "minute_range"),
        (DateTimeValue(2026, 1, 1, 0, 0, 60), "second_range"),
        (RefValue("", "id"), "empty_kind"),
        (RefValue("contact", " "), "empty_id"),
        (BoolValue(1), "not_a_boolean"),
        (object(), "unknown_type"),
    ],
)
def test_symbolic_slot_values_fail_closed_on_malformed_typed_data(value, reason) -> None:
    assert validate_value(value) == reason


def test_plan_types_and_argument_names_fail_closed() -> None:
    with pytest.raises(ValueError, match="plan status"):
        ExecutionPlan("app", "open", status="ready")
    with pytest.raises(ValueError, match="side effect"):
        ExecutionPlan("app", "open", side_effect="none")
    with pytest.raises(ValueError, match="plan location"):
        ExecutionPlan("app", "open", location="server", reason="unavailable")
    with pytest.raises(ValueError, match="provider argument"):
        ExecutionPlan(
            "app",
            "open",
            provider="open",
            location=PlanLocation.SERVER,
            status=PlanStatus.READY,
            arguments=(PlanArgument(" ", TextValue("x")),),
        )


def test_plan_environment_rejects_malformed_authority_rows_and_bounds() -> None:
    with pytest.raises(ValueError, match="owner"):
        DeviceAdvertisement("phone", " ", ("app.open",))
    with pytest.raises(ValueError, match="capabilities"):
        DeviceAdvertisement("phone", "alice", ())
    with pytest.raises(ValueError, match="capabilities"):
        DeviceAdvertisement("phone", "alice", ("",))

    with pytest.raises(ValueError, match="auths"):
        PlanEnvironment("alice", auths=tuple(f"auth-{i}" for i in range(17)))
    with pytest.raises(ValueError, match="auths"):
        PlanEnvironment("alice", auths=("",))
    device = DeviceAdvertisement("phone", "alice", ("app.open",))
    with pytest.raises(ValueError, match="devices"):
        PlanEnvironment("alice", devices=(device,) * 65)
    with pytest.raises(ValueError, match="providers"):
        PlanEnvironment("alice", providers=("",))
    with pytest.raises(ValueError, match="alias"):
        PlanEnvironment("alice", aliases=(("", "alias"),))
    with pytest.raises(ValueError, match="PlanLocation"):
        PlanEnvironment("alice", policies=(PreferLocation("server"),))
    with pytest.raises(ValueError, match="prefer-device"):
        PlanEnvironment("alice", policies=(PreferDevice(1),))


def test_plan_executor_constructor_resolver_and_tracking_edges() -> None:
    with pytest.raises(ValueError, match="either adapters or adapter_resolver"):
        PlanExecutor()
    with pytest.raises(ValueError, match="either adapters or adapter_resolver"):
        PlanExecutor({}, adapter_resolver=lambda _provider: None)
    with pytest.raises(ValueError, match="max_tracked"):
        PlanExecutor({}, max_tracked=0)

    calls: list[str] = []

    def resolver(provider: str):
        calls.append(provider)
        if provider == "open":
            return lambda _plan: "ok"
        return None

    plan = ExecutionPlan(
        "app",
        "open",
        provider="open",
        location=PlanLocation.SERVER,
        side_effect=PlanSideEffect.NONE,
        status=PlanStatus.READY,
    )
    executor = PlanExecutor(adapter_resolver=resolver, max_tracked=1)
    assert executor.execute(plan, "req-1").response == "ok"
    executor.track("req-2")
    assert executor.execute(plan, "req-1").response == "ok"
    assert calls == ["open", "open"]


@pytest.mark.parametrize(
    "overrides",
    [
        {"display_name": 1},
        {"display_name": "x" * 129},
        {"display_name": ""},
        {"display_name": " Zara"},
        {"display_name": "Zara\n"},
        {"capabilities": tuple(f"cap{i}" for i in range(65))},
        {"capabilities": ("chat", "chat")},
        {"capabilities": ("bad token",)},
        {"profiles": ("default", "default")},
        {"installed": 1},
        {"health": "ready"},
        {"locality": "embedded"},
        {"transport": "in_process"},
        {"provider_control": "zara"},
        {"model_control": "zara"},
    ],
)
def test_runtime_descriptor_rejects_noncanonical_wire_shapes(overrides) -> None:
    with pytest.raises((TypeError, ValueError)):
        _descriptor(**overrides)


@pytest.mark.parametrize(
    "kwargs",
    [
        {"runtime_id": "Bad Runtime", "generation": 1, "context_ref": "ctx:turn"},
        {"runtime_id": "zara-python", "generation": True, "context_ref": "ctx:turn"},
        {"runtime_id": "zara-python", "generation": 0, "context_ref": "ctx:turn"},
        {"runtime_id": "zara-python", "generation": 1, "context_ref": 1},
        {
            "runtime_id": "zara-python",
            "generation": 1,
            "context_ref": "ctx:turn",
            "capability_refs": tuple(f"cap:c{i}" for i in range(65)),
        },
        {
            "runtime_id": "zara-python",
            "generation": 1,
            "context_ref": "ctx:turn",
            "capability_refs": ("cap:one", "cap:one"),
        },
    ],
)
def test_runtime_invocation_binding_rejects_forged_or_unbounded_refs(kwargs) -> None:
    with pytest.raises((TypeError, ValueError)):
        RuntimeInvocationBinding(**kwargs)


def test_runtime_registry_rejects_bad_discovery_lookup_and_binding_types() -> None:
    registry = RuntimeRegistry()
    with pytest.raises(TypeError, match="RuntimeDescriptor"):
        registry.refresh([object()])
    with pytest.raises(RuntimeUnavailable, match="not discovered"):
        registry.capabilities("missing")
    with pytest.raises(RuntimeUnavailable, match="not discovered"):
        registry.select("missing")

    registry.refresh([_descriptor()])
    selection = registry.select("zara-python")
    binding = registry.bind_invocation("ctx:turn")

    assert registry.accepts_binding(object(), context_ref="ctx:turn") is False
    assert registry.accepts_binding(binding, context_ref=1) is False
    assert registry.accepts_generation("zara-python", selection.generation)
    assert registry.discover()[0].id == "zara-python"


def test_package_profile_rejects_direct_and_wire_shape_bypasses() -> None:
    with pytest.raises(PackageProfileError, match="enabled_packages"):
        AppPackageProfile("app", "pkg")
    with pytest.raises(PackageProfileError, match="pins"):
        AppPackageProfile("app", ("pkg",), pins="1.0")
    with pytest.raises(PackageProfileError, match="pins"):
        AppPackageProfile("app", ("pkg",), pins=(("pkg", "1"),) * 257)
    with pytest.raises(PackageProfileError, match="pairs"):
        AppPackageProfile("app", ("pkg",), pins=(("pkg",),))
    with pytest.raises(PackageProfileError, match="duplicate package pin"):
        AppPackageProfile("app", ("pkg",), pins=(("pkg", "1"), ("pkg", "2")))
    with pytest.raises(PackageProfileError, match="mapping"):
        AppPackageProfile.from_mapping([])
    with pytest.raises(PackageProfileError, match="enabled_packages"):
        AppPackageProfile.from_mapping(
            {"schema": PACKAGE_PROFILE_SCHEMA, "app_id": "app", "enabled_packages": "pkg", "pins": {}}
        )
    with pytest.raises(PackageProfileError, match="pins"):
        AppPackageProfile.from_mapping(
            {"schema": PACKAGE_PROFILE_SCHEMA, "app_id": "app", "enabled_packages": [], "pins": []}
        )
    with pytest.raises(PackageProfileError, match="pins"):
        AppPackageProfile.from_mapping(
            {
                "schema": PACKAGE_PROFILE_SCHEMA,
                "app_id": "app",
                "enabled_packages": [f"pkg{i}" for i in range(256)],
                "pins": {f"pkg{i}": "1" for i in range(257)},
            }
        )


def test_package_profile_queries_and_activation_authority_are_bounded() -> None:
    profile = AppPackageProfile("app", ("pkg",), pins=(("pkg", "1.0"),))
    assert profile.enables("pkg")
    assert not profile.enables("other")
    assert profile.pinned_version("pkg") == "1.0"
    assert profile.pinned_version("other") is None
    assert profile.owner_for("pkg") == "app:app:package:pkg"
    with pytest.raises(PackageProfileError, match="not enabled"):
        profile.owner_for("other")
    with pytest.raises(PackageProfileError, match="symbol registry"):
        activate_profile_package(object(), profile, "pkg", ())
    with pytest.raises(PackageProfileError, match="AppPackageProfile"):
        activate_profile_package(ProgrammableSymbolRegistry(), object(), "pkg", ())


@pytest.mark.parametrize(
    "spec,owner,layer",
    [
        (object(), "plugin:test", "package"),
        (SymbolSpec("x", "command", 1), "plugin:test", "unknown"),
        (SymbolSpec("x", "command", 1, priority=True), "plugin:test", "package"),
        (SymbolSpec("x", "command", 1, priority=100001), "plugin:test", "package"),
        (SymbolSpec("x", "command", 1, docs=1), "plugin:test", "package"),
        (SymbolSpec("x", "command", 1, docs="x" * 4097), "plugin:test", "package"),
        (SymbolSpec("x", "command", 1, source=1), "plugin:test", "package"),
        (SymbolSpec("x", "command", 1, source="x" * 513), "plugin:test", "package"),
        (
            SymbolSpec("x", "command", 1, capabilities=tuple(f"cap{i}" for i in range(65))),
            "plugin:test",
            "package",
        ),
        (SymbolSpec("x", "command", 1, capabilities=("",)), "plugin:test", "package"),
        (SymbolSpec("x", "command", 1, capabilities=(1,)), "plugin:test", "package"),
        (SymbolSpec("x", "command", 1, capabilities=("x" * 129,)), "plugin:test", "package"),
        (SymbolSpec("x", "command", 1, capabilities=("cap", "cap")), "plugin:test", "package"),
    ],
)
def test_symbol_registry_rejects_malformed_generation_metadata(spec, owner, layer) -> None:
    registry = ProgrammableSymbolRegistry()
    with pytest.raises(SymbolRegistrationError):
        registry.replace_owner(owner, (spec,), layer=layer)
    assert registry.symbols() == ()


def test_symbol_registry_noop_and_empty_lookup_edges() -> None:
    registry = ProgrammableSymbolRegistry()
    assert registry.unregister(None) is False
    assert registry.unregister(999) is False
    assert registry.describe("unknown") == ()
    assert registry.clear_owner("plugin:none") == 0


def test_canonical_expert_port_refuses_noncanonical_registry_owner() -> None:
    with pytest.raises(TypeError, match="ExpertRegistry"):
        CanonicalExpertInvocationPort(object())
