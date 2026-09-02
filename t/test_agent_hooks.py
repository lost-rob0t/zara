import pytest

from zara.agent.hooks import (
    AgentLoopAdviceRegistry,
    HookInvocationError,
    HookRegistrationError,
)


@pytest.mark.asyncio
async def test_before_and_after_advice_are_deterministic():
    events = []
    registry = AgentLoopAdviceRegistry(enabled=True, allow_override=False)

    registry.register("before", "plugin-b", 20, lambda: events.append("before-20"))
    registry.register("before", "plugin-a", 10, lambda: events.append("before-10"))
    registry.register("after", "plugin-a", 10, lambda result: events.append(f"after:{result}"))

    async def base():
        events.append("base")
        return "done"

    result = await registry.invoke(base)

    assert result == "done"
    assert events == ["before-10", "before-20", "base", "after:done"]


@pytest.mark.asyncio
async def test_disabled_registry_invokes_only_base():
    events = []
    registry = AgentLoopAdviceRegistry(enabled=False, allow_override=True)
    registry.register("before", "user", 0, lambda: events.append("hook"))

    async def base():
        events.append("base")
        return 7

    assert await registry.invoke(base) == 7
    assert events == ["base"]


@pytest.mark.parametrize("kind", ["around", "override"])
def test_override_capable_advice_requires_both_policy_gates(kind):
    callback = lambda *args, **kwargs: None

    with pytest.raises(HookRegistrationError):
        AgentLoopAdviceRegistry(enabled=True, allow_override=False).register(
            kind, "user", 0, callback
        )

    with pytest.raises(HookRegistrationError):
        AgentLoopAdviceRegistry(enabled=False, allow_override=True).register(
            kind, "user", 0, callback
        )


@pytest.mark.asyncio
async def test_clear_owner_does_not_remove_other_owners():
    events = []
    registry = AgentLoopAdviceRegistry(enabled=True, allow_override=False)
    registry.register("before", "user", 0, lambda: events.append("user"))
    registry.register("before", "plugin", 0, lambda: events.append("plugin"))

    registry.clear_owner("user")

    await registry.invoke(lambda: events.append("base"))
    assert events == ["plugin", "base"]


@pytest.mark.asyncio
async def test_dispatch_uses_snapshot_when_callback_unregisters_itself():
    events = []
    registry = AgentLoopAdviceRegistry(enabled=True, allow_override=False)
    registration_id = None

    def first():
        events.append("first")
        registry.unregister(registration_id)

    registration_id = registry.register("before", "user", 0, first)
    registry.register("before", "user", 1, lambda: events.append("second"))

    await registry.invoke(lambda: events.append("base"))
    await registry.invoke(lambda: events.append("base-2"))

    assert events == ["first", "second", "base", "second", "base-2"]


@pytest.mark.asyncio
async def test_multiple_overrides_fail_before_callback_side_effects():
    events = []
    registry = AgentLoopAdviceRegistry(enabled=True, allow_override=True)
    registry.register("override", "user-a", 0, lambda: events.append("a"))
    registry.register("override", "user-b", 1, lambda: events.append("b"))

    with pytest.raises(HookInvocationError):
        await registry.invoke(lambda: events.append("base"))

    assert events == []


def test_invalid_registration_metadata_is_rejected():
    registry = AgentLoopAdviceRegistry(enabled=True, allow_override=False)

    with pytest.raises(HookRegistrationError):
        registry.register("unknown", "user", 0, lambda: None)
    with pytest.raises(HookRegistrationError):
        registry.register("before", "", 0, lambda: None)
    with pytest.raises(HookRegistrationError):
        registry.register("before", "user", 100001, lambda: None)
    with pytest.raises(HookRegistrationError):
        registry.register("before", "user", 0, None)
