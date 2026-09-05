import pytest

from zara.database import DatabaseManager
from zara.principals import PrincipalContext
from zara.runtime.clarification import ClarificationCoordinator
from zara.user_command_authoring import UserCommandAuthoringDialogue, UserCommandAuthoringService
from zara.user_command_compiler import CapabilityContract, CompiledCommandRegistry, UserCommandCompiler
from zara.user_commands import SemanticAction, UserCommandStore


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
        }
    )


def _action(app):
    return SemanticAction(
        capability="open_app",
        arguments={"app": app},
        location="device",
        target_policy="initiating_device",
    )


def _service(tmp_path, principal_id):
    database = DatabaseManager(tmp_path / f"{principal_id}.db")
    store = UserCommandStore(database, principal=PrincipalContext(principal_id))
    compiler = _compiler()
    registry = CompiledCommandRegistry(compiler)
    return UserCommandAuthoringService(store, compiler=compiler, registry=registry), store, registry


def _resolver(text):
    normalized = " ".join(text.casefold().split())
    if normalized == "open emacs":
        return (_action("emacs"),)
    raise ValueError("ambiguous action request")


def _dialogue(tmp_path, principal_id, *, clarifications=None, resolver=_resolver):
    service, store, registry = _service(tmp_path, principal_id)
    dialogue = UserCommandAuthoringDialogue(
        service,
        clarifications=clarifications or ClarificationCoordinator(),
        action_resolver=resolver,
        principal_id=principal_id,
        conversation_id="voice",
    )
    return dialogue, service, store, registry


def test_create_can_cancel_before_trigger_without_writing(tmp_path):
    dialogue, _, store, registry = _dialogue(tmp_path, "operator-a")

    dialogue.start_create()
    result = dialogue.submit("never mind")

    assert result.kind == "cancelled"
    assert store.list() == []
    assert registry.snapshot() == {}


def test_create_can_cancel_while_collecting_actions_without_writing(tmp_path):
    dialogue, _, store, registry = _dialogue(tmp_path, "operator-a")

    dialogue.start_create()
    dialogue.submit("work mode")
    result = dialogue.submit("cancel that")

    assert result.kind == "cancelled"
    assert store.list() == []
    assert registry.snapshot() == {}


def test_ambiguous_action_reasks_and_does_not_commit_partial_definition(tmp_path):
    dialogue, _, store, registry = _dialogue(tmp_path, "operator-a")

    dialogue.start_create()
    dialogue.submit("work mode")
    ambiguous = dialogue.submit("do my stuff")

    assert ambiguous.kind == "question"
    assert store.list() == []
    assert registry.snapshot() == {}

    preview = dialogue.submit("open emacs")
    assert preview.kind == "preview"
    assert store.list() == []
    assert registry.snapshot() == {}


def test_action_resolver_failure_cannot_write_partial_definition(tmp_path):
    def failing_resolver(_text):
        raise RuntimeError("resolver unavailable")

    dialogue, _, store, registry = _dialogue(
        tmp_path,
        "operator-a",
        resolver=failing_resolver,
    )

    dialogue.start_create()
    dialogue.submit("work mode")
    with pytest.raises(RuntimeError, match="resolver unavailable"):
        dialogue.submit("open emacs")

    assert store.list() == []
    assert registry.snapshot() == {}


def test_two_simultaneous_authoring_dialogues_are_principal_isolated(tmp_path):
    clarifications = ClarificationCoordinator()
    dialogue_a, _, store_a, registry_a = _dialogue(
        tmp_path,
        "operator-a",
        clarifications=clarifications,
    )
    dialogue_b, _, store_b, registry_b = _dialogue(
        tmp_path,
        "operator-b",
        clarifications=clarifications,
    )

    dialogue_a.start_create()
    dialogue_b.start_create()
    dialogue_a.submit("work mode")
    dialogue_b.submit("browse mode")
    dialogue_a.submit("open emacs")
    dialogue_b.submit("open emacs")

    created_a = dialogue_a.submit("yes")
    created_b = dialogue_b.submit("yes")

    assert created_a.kind == "created"
    assert created_b.kind == "created"
    assert [item.trigger for item in store_a.list()] == ["work mode"]
    assert [item.trigger for item in store_b.list()] == ["browse mode"]
    assert tuple(registry_a.snapshot()) == (created_a.definition.command_id,)
    assert tuple(registry_b.snapshot()) == (created_b.definition.command_id,)


def test_undo_is_bound_to_the_owning_principal_service(tmp_path):
    dialogue_a, service_a, store_a, registry_a = _dialogue(tmp_path, "operator-a")
    dialogue_b, service_b, store_b, registry_b = _dialogue(tmp_path, "operator-b")

    for dialogue, trigger in ((dialogue_a, "work mode"), (dialogue_b, "browse mode")):
        dialogue.start_create()
        dialogue.submit(trigger)
        dialogue.submit("open emacs")
        dialogue.submit("yes")

    service_a.undo()

    assert store_a.list() == []
    assert registry_a.snapshot() == {}
    assert [item.trigger for item in store_b.list()] == ["browse mode"]
    assert len(registry_b.snapshot()) == 1
    assert service_b.undo() is None
