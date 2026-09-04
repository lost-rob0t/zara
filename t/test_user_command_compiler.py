import pytest

from zara.runtime.frames import DurationValue, FrameStatus, SlotOrigin, TextValue
from zara.user_commands import CommandSlot, SemanticAction, UserCommandDefinition
from zara.user_command_compiler import (
    CapabilityContract,
    CommandCompileError,
    CompiledCommandRegistry,
    UserCommandCompiler,
)


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


def _compiler():
    return UserCommandCompiler(
        _catalog(),
        protected_triggers=frozenset({"open", "search", "timer"}),
    )


def test_work_mode_compiles_to_two_registered_open_app_actions():
    definition = UserCommandDefinition(
        command_id="work-mode",
        trigger="work mode",
        aliases=("start work",),
        actions=(
            SemanticAction(
                capability="open_app",
                arguments={"app": "emacs"},
                location="device",
                target_policy="initiating_device",
            ),
            SemanticAction(
                capability="open_app",
                arguments={"app": "firefox"},
                location="device",
                target_policy="initiating_device",
            ),
        ),
    )

    compiled = _compiler().compile(definition)

    assert compiled.command_id == "work-mode"
    assert compiled.trigger == "work mode"
    assert compiled.aliases == ("start work",)
    assert [action.contract.action_id for action in compiled.actions] == [
        "open_app",
        "open_app",
    ]
    assert compiled.actions[0].arguments == {"app": TextValue("emacs")}
    assert compiled.actions[1].arguments == {"app": TextValue("firefox")}
    assert compiled.actions[0].target_policy == "initiating_device"


def test_parameterized_timer_compiles_required_slot_and_typed_default():
    definition = UserCommandDefinition(
        command_id="focus-timer",
        trigger="focus timer",
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

    compiled = _compiler().compile(definition)
    frame = compiled.frame_template()

    assert frame.intent_ns == "user"
    assert frame.intent_name == "focus-timer"
    assert frame.status is FrameStatus.MISSING
    assert frame.missing == ("duration",)
    assert frame.slot_value("label") == TextValue("focus")
    assert frame.origin_of("label") is SlotOrigin.DEFAULT
    assert compiled.actions[0].slot_bindings == {
        "duration": "duration",
        "label": "label",
    }


def test_unknown_capability_and_invalid_location_fail_with_source_field():
    unknown = UserCommandDefinition(
        command_id="bad",
        trigger="bad command",
        actions=(SemanticAction(capability="not_registered", arguments={}),),
    )
    with pytest.raises(CommandCompileError) as caught:
        _compiler().compile(unknown)
    assert caught.value.command_id == "bad"
    assert caught.value.field == "actions[0].capability"
    assert caught.value.code == "unknown_capability"

    wrong_location = UserCommandDefinition(
        command_id="wrong-location",
        trigger="wrong location",
        actions=(
            SemanticAction(
                capability="open_app",
                arguments={"app": "emacs"},
                location="server",
            ),
        ),
    )
    with pytest.raises(CommandCompileError) as caught:
        _compiler().compile(wrong_location)
    assert caught.value.field == "actions[0].location"
    assert caught.value.code == "location_not_allowed"


def test_action_arguments_are_checked_against_registered_schema():
    extra = UserCommandDefinition(
        command_id="extra",
        trigger="extra",
        actions=(
            SemanticAction(
                capability="open_app",
                arguments={"app": "emacs", "shell": "echo nope"},
                location="device",
            ),
        ),
    )
    with pytest.raises(CommandCompileError) as caught:
        _compiler().compile(extra)
    assert caught.value.code == "unknown_argument"

    wrong_type = UserCommandDefinition(
        command_id="wrong-type",
        trigger="wrong type",
        actions=(
            SemanticAction(
                capability="timer.set",
                arguments={"duration": "five"},
                location="server",
            ),
        ),
    )
    with pytest.raises(CommandCompileError) as caught:
        _compiler().compile(wrong_type)
    assert caught.value.code == "argument_type"


def test_typed_literal_arguments_compile_without_execution():
    definition = UserCommandDefinition(
        command_id="timer-30",
        trigger="focus thirty",
        actions=(
            SemanticAction(
                capability="timer.set",
                arguments={"duration": {"duration": 30}, "label": "focus"},
                location="server",
            ),
        ),
    )

    compiled = _compiler().compile(definition)

    assert compiled.actions[0].arguments == {
        "duration": DurationValue(30),
        "label": TextValue("focus"),
    }


def test_builtin_trigger_collision_is_rejected_without_shadowing():
    definition = UserCommandDefinition(
        command_id="shadow-open",
        trigger="OPEN",
        actions=(
            SemanticAction(
                capability="open_app",
                arguments={"app": "emacs"},
                location="device",
            ),
        ),
    )

    with pytest.raises(CommandCompileError) as caught:
        _compiler().compile(definition)

    assert caught.value.field == "trigger"
    assert caught.value.code == "protected_trigger"


def test_registry_replace_is_atomic_when_one_definition_is_invalid():
    registry = CompiledCommandRegistry(_compiler())
    valid = UserCommandDefinition(
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
    first = registry.replace_all((valid,))

    invalid = UserCommandDefinition(
        command_id="broken",
        trigger="broken",
        actions=(SemanticAction(capability="not_registered", arguments={}),),
    )
    with pytest.raises(CommandCompileError):
        registry.replace_all((valid, invalid))

    assert registry.snapshot() is first
    assert tuple(registry.snapshot()) == ("work-mode",)


def test_compilation_is_deterministic_and_provider_removal_fails_next_reload():
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
    compiler = _compiler()

    assert compiler.compile(definition) == compiler.compile(definition)

    registry = CompiledCommandRegistry(compiler)
    registry.replace_all((definition,))
    compiler_without_open = UserCommandCompiler(
        {"timer.set": _catalog()["timer.set"]},
        protected_triggers=frozenset({"open", "search", "timer"}),
    )
    registry.set_compiler(compiler_without_open)
    with pytest.raises(CommandCompileError) as caught:
        registry.replace_all((definition,))
    assert caught.value.code == "unknown_capability"
