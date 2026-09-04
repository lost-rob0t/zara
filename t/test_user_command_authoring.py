import pytest

from zara.database import DatabaseManager
from zara.principals import PrincipalContext
from zara.user_command_authoring import UserCommandAuthoringService
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


def test_preview_is_side_effect_free(tmp_path):
    service, store, registry = _service(tmp_path)

    preview = service.preview(_definition())

    assert preview.command_id == "work-mode"
    assert preview.trigger == "work mode"
    assert preview.actions[0].capability == "open_app"
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
