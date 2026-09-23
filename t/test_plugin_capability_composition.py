import concurrent.futures

import pytest

from zara.plugins.api import CapabilityHandle, PluginRuntime, RuntimeStatus


def _future():
    return concurrent.futures.Future()


def _runtime(*, resolver, invoker):
    return PluginRuntime(
        plugin_name="consumer",
        configuration={},
        status_provider=lambda: RuntimeStatus("running", True, 1),
        dispatcher=lambda _command: _future(),
        subscriber=lambda **_kwargs: None,
        failure_callback=lambda _message: None,
        capability_resolver=resolver,
        capability_invoker=invoker,
    )


def test_resolve_returns_opaque_core_handle_without_raw_tool_escape():
    expected = CapabilityHandle(
        plugin_name="provider",
        capability="provider.read",
        generation=7,
        requires_approval=False,
    )
    runtime = _runtime(resolver=lambda name: expected, invoker=lambda *_args: None)

    handle = runtime.resolve_capability("provider.read")

    assert handle == expected
    assert not hasattr(handle, "tool")
    assert not hasattr(handle, "plugin")


def test_invoke_preserves_core_handle_and_structured_request():
    handle = CapabilityHandle("provider", "provider.read", 3, False)
    seen = []
    runtime = _runtime(
        resolver=lambda _name: handle,
        invoker=lambda caller, resolved, request: seen.append((caller, resolved, request))
        or {"status": "ok"},
    )

    result = runtime.invoke_capability(handle, {"query": "status"})

    assert result == {"status": "ok"}
    assert seen == [("consumer", handle, {"query": "status"})]


def test_plugin_cannot_supply_turn_correlation():
    handle = CapabilityHandle("provider", "provider.read", 3, False)
    runtime = _runtime(resolver=lambda _name: handle, invoker=lambda *_args: None)

    with pytest.raises(TypeError, match="unexpected keyword argument"):
        runtime.invoke_capability(handle, {}, turn_id="forged-turn")


def test_closed_runtime_cannot_resolve_or_invoke_capabilities():
    handle = CapabilityHandle("provider", "provider.read", 1, False)
    runtime = _runtime(resolver=lambda _name: handle, invoker=lambda *_args: {})
    runtime._shutdown()

    with pytest.raises(RuntimeError, match="plugin runtime is closed"):
        runtime.resolve_capability("provider.read")
    with pytest.raises(RuntimeError, match="plugin runtime is closed"):
        runtime.invoke_capability(handle, {})


def test_invalid_or_foreign_values_never_reach_invoker():
    calls = []
    runtime = _runtime(
        resolver=lambda _name: None,
        invoker=lambda *_args: calls.append(True),
    )

    with pytest.raises(TypeError, match="CapabilityHandle"):
        runtime.invoke_capability(object(), {})
    with pytest.raises(TypeError, match="mapping"):
        runtime.invoke_capability(CapabilityHandle("provider", "provider.read", 1, False), [])
    assert calls == []


def test_unavailable_capability_fails_explicitly():
    runtime = _runtime(resolver=lambda _name: None, invoker=lambda *_args: {})

    with pytest.raises(LookupError, match="capability is unavailable"):
        runtime.resolve_capability("missing.read")