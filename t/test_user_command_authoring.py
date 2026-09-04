import pytest

from zara.database import DatabaseManager
from zara.principals import PrincipalContext
from zara.runtime.clarification import ClarificationCoordinator
from zara.user_command_authoring import (
    UserCommandAuthoringDialogue,
    UserCommandAuthoringService,
)
from zara.user_command_compiler import (
    CapabilityContract,
    CommandCompileError,
    CompiledCommandRegistry,
    UserCommandCompiler,
)
from zara.user_commands import SemanticAction, UserCommandDefinition, UserCommandStore


def _compiler():
    return UserCommandCompiler(
        {
            "open_app": CapabilityContract(
                action_id="open_app",
                intent_ns="app",
                intent_name="open",
                arguments={"app": "text"},
                locations=frozenset({"device"}),
            )
        },
        protected_triggers=frozenset({"set a timer"}),
    )


def _definition(*, app="emacs", trigger="work mode"):
    return UserCommandDefinition(
        command_id="work-mode",
        trigger=trigger,
        actions=(
            SemanticAction(
                capability="open_app",
                arguments={"app": app},
                location="device",
                target_policy="initiating_device",
            ),
        ),
    )


def _service(tmp_path, principal_id="operator-a"):
    database = DatabaseManager(tmp_path / f"{principal_id}.db")
    principal = PrincipalContext(principal_id)
    store = UserCommandStore(database, principal=principal)
    compiler = _compiler()
    registry = CompiledCommandRegistry(compiler)
    return UserCommandAuthoringService(store, compiler=compiler, registry=registry), store, registry


def _dialogue(tmp_path, principal_id="operator-a"):
    service, store, registry = _service(tmp_path, principal_id)
    dialogue = UserCommandAuthoringDialogue(
        service,
        clarifications=ClarificationCoordinator(),
        action_resolver=_resolve_actions,
        principal_id=principal_id,
        conversation_id="voice",
    )
    return dialogue, service, store, registry


def _resolve_actions(text):
    normalized = " ".join(text.casefold().split())
    if normalized == "open emacs and firefox":
        apps = ("emacs", "firefox")
    elif normalized == "open firefox":
        apps = ("firefox",)
    else:
        raise ValueError("ambiguous action request")
    return tuple(
        SemanticAction(
            capability="open_app",
            arguments={"app": app},
            location="device",
            target_policy="initiating_device",
        )
        for app in apps
    )


def test_preview_is_side_effect_free(tmp_path):
    service, store, registry = _service(tmp_path)

    preview = service.preview(_definition())

    assert preview.command_id == "work-mode"
    assert preview.trigger == "work mode"
    assert preview.actions[0].contract.action_id == "open_app"
    assert preview.actions[0].location == "device"
    assert store.list() == []
    assert registry.snapshot() == {}


def test_create_persists_and_hot_publishes_immediately(tmp_path):
    service, store, registry = _service(tmp_path)

    saved = service.create(_definition())

    assert saved.revision == 1
    assert store.get("work-mode") == saved
    assert registry.snapshot()["work-mode"].trigger == "work mode"


def test_invalid_create_preserves_durable_and_active_state(tmp_path):
    service, store, registry = _service(tmp_path)
    first = service.create(_definition())
    before = registry.snapshot()
    invalid = UserCommandDefinition(
        command_id="bad-command",
        trigger="set a timer",
        actions=first.actions,
    )

    with pytest.raises(CommandCompileError):
        service.create(invalid)

    assert store.get("bad-command") is None
    assert registry.snapshot() is before
    assert tuple(registry.snapshot()) == ("work-mode",)


def test_list_and_describe_are_principal_scoped(tmp_path):
    service_a, _, _ = _service(tmp_path, "operator-a")
    service_b, _, _ = _service(tmp_path, "operator-b")
    service_a.create(_definition())

    assert [item.command_id for item in service_a.list_commands()] == ["work-mode"]
    assert service_a.describe("work-mode").trigger == "work mode"
    assert service_b.list_commands() == []
    assert service_b.describe("work-mode") is None


def test_edit_requires_revision_and_republishes(tmp_path):
    service, store, registry = _service(tmp_path)
    saved = service.create(_definition())

    updated = service.edit(
        saved.with_updates(
            actions=(
                SemanticAction(
                    capability="open_app",
                    arguments={"app": "firefox"},
                    location="device",
                    target_policy="initiating_device",
                ),
            )
        ),
        expected_revision=saved.revision,
    )

    assert updated.revision == 2
    assert store.get("work-mode") == updated
    assert registry.snapshot()["work-mode"].actions[0].arguments["app"].text == "firefox"


def test_delete_and_undo_restore_definition_without_executing(tmp_path):
    service, store, registry = _service(tmp_path)
    saved = service.create(_definition())

    service.delete("work-mode", expected_revision=saved.revision)
    assert store.get("work-mode") is None
    assert registry.snapshot() == {}

    restored = service.undo()
    assert restored is not None
    assert restored.command_id == "work-mode"
    assert store.get("work-mode") == restored
    assert "work-mode" in registry.snapshot()
    assert service.undo() is None


def test_undo_create_removes_only_latest_mutation(tmp_path):
    service, store, registry = _service(tmp_path)
    saved = service.create(_definition())

    result = service.undo()

    assert result is None
    assert store.get(saved.command_id) is None
    assert registry.snapshot() == {}
    assert service.undo() is None


def test_create_dialogue_uses_shared_clarification_and_requires_confirmation(tmp_path):
    service, store, registry = _service(tmp_path)
    clarifications = ClarificationCoordinator()
    dialogue = UserCommandAuthoringDialogue(
        service,
        clarifications=clarifications,
        action_resolver=_resolve_actions,
        principal_id="operator-a",
        conversation_id="voice",
    )

    started = dialogue.start_create()
    assert started.kind == "question"
    assert started.message == "What should you say?"
    assert clarifications.session_for("operator-a", "voice") is not None

    trigger = dialogue.submit("work mode")
    assert trigger.kind == "question"
    assert trigger.message == "What should work mode do?"

    preview = dialogue.submit("open Emacs and Firefox")
    assert preview.kind == "preview"
    assert "open_app(emacs)" in preview.message
    assert "open_app(firefox)" in preview.message
    assert preview.message.endswith("Save it?")
    assert store.list() == []
    assert registry.snapshot() == {}

    created = dialogue.submit("yes")
    assert created.kind == "created"
    assert created.definition is not None
    assert created.definition.trigger == "work mode"
    assert len(created.definition.actions) == 2
    assert store.get(created.definition.command_id) == created.definition
    assert created.definition.command_id in registry.snapshot()
    assert clarifications.active_question("operator-a", "voice") is None


def test_create_dialogue_no_confirmation_is_side_effect_free(tmp_path):
    dialogue, _, store, registry = _dialogue(tmp_path)

    dialogue.start_create()
    dialogue.submit("work mode")
    dialogue.submit("open Emacs and Firefox")
    cancelled = dialogue.submit("no")

    assert cancelled.kind == "cancelled"
    assert cancelled.message == "Cancelled."
    assert store.list() == []
    assert registry.snapshot() == {}


def test_dialogue_lists_describes_and_dry_runs_without_mutation(tmp_path):
    dialogue, service, store, registry = _dialogue(tmp_path)
    saved = service.create(_definition())
    before = registry.snapshot()

    listed = dialogue.list_commands()
    described = dialogue.describe("work-mode")
    tested = dialogue.test("work-mode")

    assert listed.kind == "list"
    assert "work mode" in listed.message
    assert described.kind == "description"
    assert "open_app(emacs)" in described.message
    assert tested.kind == "preview"
    assert tested.message.endswith("Dry run only; nothing executed.")
    assert store.get("work-mode") == saved
    assert registry.snapshot() is before


def test_edit_dialogue_previews_then_confirms_revisioned_mutation(tmp_path):
    dialogue, service, store, registry = _dialogue(tmp_path)
    saved = service.create(_definition())

    started = dialogue.start_edit("work-mode")
    assert started.kind == "question"
    assert started.message == "What should work mode do instead?"

    preview = dialogue.submit("open Firefox")
    assert preview.kind == "preview"
    assert "open_app(firefox)" in preview.message
    assert preview.message.endswith("Save this edit?")
    assert store.get("work-mode") == saved

    edited = dialogue.submit("yes")
    assert edited.kind == "edited"
    assert edited.definition is not None
    assert edited.definition.revision == 2
    assert store.get("work-mode") == edited.definition
    assert registry.snapshot()["work-mode"].actions[0].arguments["app"].text == "firefox"


def test_delete_dialogue_requires_confirmation_and_can_be_undone(tmp_path):
    dialogue, service, store, registry = _dialogue(tmp_path)
    saved = service.create(_definition())

    started = dialogue.start_delete("work-mode")
    assert started.kind == "question"
    assert started.message == 'Delete command "work mode"?'
    cancelled = dialogue.submit("no")
    assert cancelled.kind == "cancelled"
    assert store.get("work-mode") == saved

    dialogue.start_delete("work-mode")
    deleted = dialogue.submit("yes")
    assert deleted.kind == "deleted"
    assert store.get("work-mode") is None
    assert registry.snapshot() == {}

    undone = dialogue.undo_last()
    assert undone.kind == "undone"
    assert undone.definition is not None
    assert undone.definition.command_id == "work-mode"
    assert store.get("work-mode") == undone.definition
    assert "work-mode" in registry.snapshot()


def test_dialogue_unknown_command_is_indistinguishable_and_side_effect_free(tmp_path):
    dialogue, _, store, registry = _dialogue(tmp_path)

    described = dialogue.describe("foreign-or-missing")
    tested = dialogue.test("foreign-or-missing")
    edited = dialogue.start_edit("foreign-or-missing")
    deleted = dialogue.start_delete("foreign-or-missing")

    assert {described.kind, tested.kind, edited.kind, deleted.kind} == {"not_found"}
    assert len({described.message, tested.message, edited.message, deleted.message}) == 1
    assert store.list() == []
    assert registry.snapshot() == {}
