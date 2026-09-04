import pytest

from zara.runtime.clarification import ClarificationCoordinator
from zara.runtime.frames import DurationValue, FrameStatus, SlotOrigin, TextValue
from zara.user_command_compiler import (
    CapabilityContract,
    CommandCompileError,
    CompiledCommandRegistry,
    UserCommandCompiler,
)
from zara.user_command_runtime import (
    UserCommandResolutionKind,
    UserCommandResolver,
)
from zara.user_commands import CommandSlot, SemanticAction, UserCommandDefinition


def _catalog():
    return {
        "open_app": CapabilityContract(
            action_id="open_app",
            intent_ns="app",
            intent_name="open",
            arguments={"app": "text"},
            locations=frozenset({"device"}),
        ),
        "timer.set": CapabilityContract(
            action_id="timer.set",
            intent_ns="device",
            intent_name="timer.set",
            arguments={"duration": "duration", "label": "text"},
            locations=frozenset({"server", "device"}),
        ),
    }


def _compiler(*, protected=frozenset({"open", "search", "timer"})):
    return UserCommandCompiler(_catalog(), protected_triggers=protected)


def _focus_timer(command_id="focus-timer", *, alias="focus timer"):
    return UserCommandDefinition(
        command_id=command_id,
        trigger="focus timer for {duration}",
        aliases=(alias,),
        slots=(
            CommandSlot("duration", "duration", required=True),
            CommandSlot("label", "text", required=False, default={"text": "focus"}),
        ),
        actions=(
            SemanticAction(
                capability="timer.set",
                arguments={"duration_slot": "duration", "label_slot": "label"},
                location="server",
            ),
        ),
    )


def _resolver(*definitions, availability=None):
    registry = CompiledCommandRegistry(_compiler())
    registry.replace_all(tuple(definitions))
    return registry, UserCommandResolver(registry, capability_availability=availability)


def test_parameterized_trigger_uses_canonical_duration_parser_and_fills_frame():
    _, resolver = _resolver(_focus_timer())

    result = resolver.resolve("focus timer for thirty five minutes")

    assert result.kind is UserCommandResolutionKind.MATCHED
    assert result.command.command_id == "focus-timer"
    assert result.frame.status is FrameStatus.COMPLETE
    assert result.frame.slot_value("duration") == DurationValue(35 * 60)
    assert result.frame.origin_of("duration") is SlotOrigin.UTTERANCE
    assert result.frame.slot_value("label") == TextValue("focus")
    assert result.frame.origin_of("label") is SlotOrigin.DEFAULT


def test_bare_trigger_returns_missing_frame_and_opens_canonical_clarification():
    _, resolver = _resolver(_focus_timer())
    result = resolver.resolve("focus timer")

    assert result.kind is UserCommandResolutionKind.MATCHED
    assert result.frame.status is FrameStatus.MISSING
    assert result.frame.missing == ("duration",)

    coordinator = ClarificationCoordinator()
    opened = resolver.open_clarification(
        result,
        coordinator,
        principal="operator-a",
        conversation_id="conv-1",
    )
    assert opened.kind == "opened"
    assert opened.question == "How long?"

    follow_up = coordinator.submit_follow_up(
        "thirty five minutes",
        principal="operator-a",
        conversation_id="conv-1",
    )
    assert follow_up.kind == "complete"
    assert follow_up.frame.slot_value("duration") == DurationValue(35 * 60)
    assert follow_up.frame.slot_value("label") == TextValue("focus")


def test_invalid_parameter_value_does_not_match_or_guess():
    _, resolver = _resolver(_focus_timer())

    result = resolver.resolve("focus timer for eventually")

    assert result.kind is UserCommandResolutionKind.NO_MATCH
    assert result.frame is None


def test_overlapping_parameterized_commands_are_ambiguous_not_first_match():
    first = _focus_timer("focus-timer")
    second = UserCommandDefinition(
        command_id="focus-session",
        trigger="{label} timer for {duration}",
        aliases=("begin focus",),
        slots=(
            CommandSlot("label", "text"),
            CommandSlot("duration", "duration"),
        ),
        actions=(
            SemanticAction(
                capability="timer.set",
                arguments={"duration_slot": "duration", "label_slot": "label"},
                location="server",
            ),
        ),
    )
    registry = CompiledCommandRegistry(_compiler())
    registry.replace_all((first, second))
    resolver = UserCommandResolver(registry)

    result = resolver.resolve("focus timer for 20 minutes")

    assert result.kind is UserCommandResolutionKind.AMBIGUOUS
    assert result.frame.status is FrameStatus.AMBIGUOUS
    assert result.frame.alternatives == ("focus-session", "focus-timer")
    assert result.command is None


def test_template_placeholder_must_name_declared_slot():
    definition = UserCommandDefinition(
        command_id="broken-template",
        trigger="focus timer for {missing}",
        slots=(CommandSlot("duration", "duration"),),
        actions=(
            SemanticAction(
                capability="timer.set",
                arguments={"duration_slot": "duration", "label": "focus"},
                location="server",
            ),
        ),
    )

    with pytest.raises(CommandCompileError) as caught:
        _compiler().compile(definition)

    assert caught.value.field == "trigger"
    assert caught.value.code == "unknown_slot_placeholder"


def test_capability_revocation_is_checked_at_resolution_without_rewriting_definition():
    definition = UserCommandDefinition(
        command_id="work-mode",
        trigger="work mode",
        actions=(
            SemanticAction(
                capability="open_app",
                arguments={"app": "emacs"},
                location="device",
            ),
        ),
    )
    state = {"open_app": "available"}
    _, resolver = _resolver(definition, availability=lambda capability: state[capability])

    assert resolver.resolve("work mode").kind is UserCommandResolutionKind.MATCHED

    state["open_app"] = "denied"
    denied = resolver.resolve("work mode")
    assert denied.kind is UserCommandResolutionKind.DENIED
    assert denied.reason == "open_app"

    state["open_app"] = "unavailable"
    unavailable = resolver.resolve("work mode")
    assert unavailable.kind is UserCommandResolutionKind.UNAVAILABLE
    assert unavailable.reason == "open_app"


def test_resolution_reads_one_immutable_snapshot_while_reload_publishes_new_snapshot():
    old = UserCommandDefinition(
        command_id="work-mode",
        trigger="work mode",
        actions=(
            SemanticAction(
                capability="open_app",
                arguments={"app": "emacs"},
                location="device",
            ),
        ),
    )
    new = old.with_updates(
        actions=(
            SemanticAction(
                capability="open_app",
                arguments={"app": "firefox"},
                location="device",
            ),
        )
    )
    registry = CompiledCommandRegistry(_compiler())
    old_snapshot = registry.replace_all((old,))
    resolver = UserCommandResolver(registry)

    registry.replace_all((new,))

    assert old_snapshot["work-mode"].actions[0].arguments["app"] == TextValue("emacs")
    current = resolver.resolve("work mode")
    assert current.command.actions[0].arguments["app"] == TextValue("firefox")


def test_failed_reload_keeps_previous_snapshot_resolvable():
    definition = UserCommandDefinition(
        command_id="work-mode",
        trigger="work mode",
        actions=(
            SemanticAction(
                capability="open_app",
                arguments={"app": "emacs"},
                location="device",
            ),
        ),
    )
    registry = CompiledCommandRegistry(_compiler())
    first = registry.replace_all((definition,))
    resolver = UserCommandResolver(registry)

    registry.set_compiler(
        UserCommandCompiler(
            {"timer.set": _catalog()["timer.set"]},
            protected_triggers=frozenset({"open", "search", "timer"}),
        )
    )
    with pytest.raises(CommandCompileError):
        registry.replace_all((definition,))

    assert registry.snapshot() is first
    assert resolver.resolve("work mode").kind is UserCommandResolutionKind.MATCHED
