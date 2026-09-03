import pytest

from zara.database import DatabaseManager
from zara.principals import PrincipalContext
from zara.user_commands import (
    CommandConflictError,
    SemanticAction,
    UserCommandDefinition,
    UserCommandStore,
)


def _command(command_id="work-mode", trigger="work mode", *, aliases=()):
    return UserCommandDefinition(
        command_id=command_id,
        trigger=trigger,
        aliases=aliases,
        actions=(
            SemanticAction(capability="open_app", arguments={"app": "emacs"}),
            SemanticAction(capability="open_app", arguments={"app": "firefox"}),
        ),
    )


def test_minimal_semantic_command_survives_restart(tmp_path):
    database = DatabaseManager(tmp_path / "commands.db")
    principal = PrincipalContext("operator-a")
    store = UserCommandStore(database, principal=principal)

    saved = store.create(_command())
    database.close()

    reopened = UserCommandStore(
        DatabaseManager(tmp_path / "commands.db"),
        principal=principal,
    )
    loaded = reopened.get("work-mode")

    assert loaded == saved
    assert loaded is not None
    assert loaded.owner_principal_id == "operator-a"
    assert loaded.schema_version == 1
    assert loaded.revision == 1


def test_principal_cannot_read_update_or_delete_another_principals_command(tmp_path):
    database = DatabaseManager(tmp_path / "commands.db")
    owner = UserCommandStore(database, principal=PrincipalContext("operator-a"))
    stranger = UserCommandStore(database, principal=PrincipalContext("operator-b"))
    saved = owner.create(_command())

    assert stranger.get(saved.command_id) is None
    assert stranger.list() == []
    with pytest.raises(KeyError):
        stranger.update(saved, expected_revision=saved.revision)
    with pytest.raises(KeyError):
        stranger.delete(saved.command_id, expected_revision=saved.revision)
    assert owner.get(saved.command_id) == saved


def test_stale_revision_update_fails_without_overwriting_latest_value(tmp_path):
    store = UserCommandStore(
        DatabaseManager(tmp_path / "commands.db"),
        principal=PrincipalContext("operator-a"),
    )
    original = store.create(_command())
    first_edit = original.with_updates(trigger="focused work")
    saved = store.update(first_edit, expected_revision=original.revision)

    with pytest.raises(CommandConflictError):
        store.update(original.with_updates(trigger="stale work"), expected_revision=original.revision)

    assert store.get(original.command_id) == saved
    assert saved.trigger == "focused work"
    assert saved.revision == 2


def test_trigger_and_alias_collisions_are_principal_scoped(tmp_path):
    database = DatabaseManager(tmp_path / "commands.db")
    owner = UserCommandStore(database, principal=PrincipalContext("operator-a"))
    other = UserCommandStore(database, principal=PrincipalContext("operator-b"))
    owner.create(_command(aliases=("focus",)))

    with pytest.raises(CommandConflictError):
        owner.create(_command("duplicate-trigger", "WORK MODE"))
    with pytest.raises(CommandConflictError):
        owner.create(_command("duplicate-alias", "another mode", aliases=(" Focus ",)))

    assert other.create(_command()).owner_principal_id == "operator-b"


def test_durable_commands_fail_closed_for_ephemeral_principals(tmp_path):
    store = UserCommandStore(
        DatabaseManager(tmp_path / "commands.db"),
        principal=PrincipalContext("guest-1", kind="guest"),
    )

    with pytest.raises(PermissionError):
        store.create(_command())


def test_executable_capabilities_and_non_json_arguments_are_rejected():
    for capability in ("shell", "exec", "eval", "python", "prolog_goal", "dynamic_import"):
        with pytest.raises(ValueError):
            SemanticAction(capability=capability, arguments={"value": "echo nope"})

    with pytest.raises(ValueError):
        SemanticAction(capability="open_app", arguments={"value": object()})


def test_command_identity_and_trigger_are_normalized_and_validated():
    command = _command(command_id=" Work-Mode ", trigger="  Work   Mode  ", aliases=(" Focus ",))

    assert command.command_id == "work-mode"
    assert command.trigger == "Work Mode"
    assert command.aliases == ("Focus",)

    with pytest.raises(ValueError):
        _command(command_id="../escape")
    with pytest.raises(ValueError):
        _command(trigger="   ")
