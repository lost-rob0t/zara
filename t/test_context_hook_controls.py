import pytest

from zara.agent.hooks import AgentLoopAdviceRegistry


@pytest.mark.asyncio
async def test_core_hooks_run_when_customization_is_disabled_and_keep_order():
    events = []
    registry = AgentLoopAdviceRegistry(enabled=False, allow_override=False)
    registry.register("before", "plugin:off", 0, lambda: events.append("plugin"))
    first = registry.register_core(
        "before", "core:memory-recall", -20, lambda: events.append("memory")
    )
    registry.register_core(
        "before", "core:context", -10, lambda: events.append("context")
    )
    registry.register_core(
        "after", "core:memory-capture", 20, lambda result: events.append(f"capture:{result}")
    )

    async def base():
        events.append("base")
        return "done"

    assert await registry.invoke(base) == "done"
    assert events == ["memory", "context", "base", "capture:done"]
    assert registry.set_enabled(first, False)

    events.clear()
    assert await registry.invoke(base) == "done"
    assert events == ["context", "base", "capture:done"]


@pytest.mark.asyncio
async def test_per_hook_toggle_does_not_change_sibling_registration():
    events = []
    registry = AgentLoopAdviceRegistry(enabled=True, allow_override=False)
    first = registry.register("before", "plugin:a", 0, lambda: events.append("a"))
    second = registry.register("before", "plugin:b", 0, lambda: events.append("b"))

    assert registry.set_enabled(first, False)
    assert registry.set_enabled(999999, False) is False
    await registry.invoke(lambda: events.append("base"))

    assert events == ["b", "base"]
    diagnostics = {item.registration_id: item for item in registry.diagnostics()}
    assert diagnostics[first].enabled is False
    assert diagnostics[second].enabled is True
    assert diagnostics[first].sequence < diagnostics[second].sequence


def test_runtime_customization_toggle_is_distinct_from_core_hook_state():
    registry = AgentLoopAdviceRegistry(enabled=True, allow_override=False)
    plugin = registry.register("before", "plugin:a", 0, lambda: None)
    core = registry.register_core("before", "core:memory", 0, lambda: None)

    registry.set_customization_enabled(False)

    diagnostics = {item.registration_id: item for item in registry.diagnostics()}
    assert registry.enabled is False
    assert diagnostics[plugin].policy_gated is True
    assert diagnostics[core].policy_gated is False
    assert diagnostics[plugin].enabled is True
    assert diagnostics[core].enabled is True
