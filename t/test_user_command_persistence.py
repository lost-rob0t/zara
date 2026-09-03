import json

import pytest

from zara.database import DatabaseManager
from zara.principals import PrincipalContext
from zara.user_commands import (
    CommandConflictError,
    CommandSlot,
    SemanticAction,
    UserCommandDefinition,
    UserCommandStore,
)


def _command(command_id="work-mode", trigger="work mode", *, aliases=(), slots=()):
    return UserCommandDefinition(
        command_id=command_id,
        trigger=trigger,
        aliases=aliases,
        slots=slots,
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


def test_typed_slots_defaults_target_policy_and_migration_metadata_round_trip(tmp_path):
    definition = UserCommandDefinition(
        command_id="focus-timer",
        trigger="focus timer",
        slots=(
            CommandSlot(name="duration", value_type="duration", required=True),
            CommandSlot(
                name="label",
                value_type="text",
                required=False,
                default={"text": "focus"},
            ),
        ),
        actions=(
            SemanticAction(
                capability="timer.set",
                arguments={"duration_slot": "duration", "label_slot": "label"},
                location="server",
            ),
        ),
        migration_metadata={"created_with_schema": 1},
    )
    database = DatabaseManager(tmp_path / "commands.db")
    principal = PrincipalContext("operator-a")
    saved = UserCommandStore(database, principal=principal).create(definition)
    database.close()

    loaded = UserCommandStore(
        DatabaseManager(tmp_path / "commands.db"),
        principal=principal,
    ).get("focus-timer")

    assert loaded == saved
    assert loaded.slots[0].name == "duration"
    assert loaded.slots[1].default == {"text": "focus"}
    assert loaded.actions[0].location == "server"
    assert loaded.actions[0].target_policy is None
    assert loaded.migration_metadata == {"created_with_schema": 1}


def test_unknown_slot_type_and_invalid_action_location_fail_closed():
    with pytest.raises(ValueError):
        CommandSlot(name="payload", value_type="shell", required=True)
    with pytest.raises(ValueError):
        CommandSlot(name="duration", value_type="duration", default={"text": "wrong"})
    with pytest.raises(ValueError):
        SemanticAction(capability="open_app", arguments={}, location="somewhere")
    with pytest.raises(ValueError):
        SemanticAction(
            capability="open_app",
            arguments={},
            location="server",
            target_policy="initiating_device",
        )


def test_principal_cannot_read_search_export_update_or_delete_another_principals_command(tmp_path):
    database = DatabaseManager(tmp_path / "commands.db")
    owner = UserCommandStore(database, principal=PrincipalContext("operator-a"))
    stranger = UserCommandStore(database, principal=PrincipalContext("operator-b"))
    saved = owner.create(_command())

    assert stranger.get(saved.command_id) is None
    assert stranger.list() == []
    assert stranger.search("work") == []
    assert stranger.export() == []
    with pytest.raises(KeyError):
        stranger.update(saved, expected_revision=saved.revision)
    with pytest.raises(KeyError):
        stranger.delete(saved.command_id, expected_revision=saved.revision)
    assert owner.get(saved.command_id) == saved
    assert owner.search("WORK")[0] == saved
    assert owner.export()[0] == saved


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


def test_collision_during_update_rolls_back_definition_and_trigger_keys(tmp_path):
    store = UserCommandStore(
        DatabaseManager(tmp_path / "commands.db"),
        principal=PrincipalContext("operator-a"),
    )
    first = store.create(_command("first", "first mode"))
    store.create(_command("second", "second mode", aliases=("occupied",)))

    with pytest.raises(CommandConflictError):
        store.update(
            first.with_updates(trigger="occupied"),
            expected_revision=first.revision,
        )

    assert store.get("first") == first
    assert store.search("first mode") == [first]


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


def test_unsupported_stored_schema_is_rejected_without_rewriting_row(tmp_path):
    database = DatabaseManager(tmp_path / "commands.db")
    store = UserCommandStore(database, principal=PrincipalContext("operator-a"))
    saved = store.create(_command())
    row = database.fetch_one(
        "SELECT definition_json FROM user_commands WHERE principal_id = ? AND command_id = ?",
        ("operator-a", saved.command_id),
    )
    original_json = str(row["definition_json"])
    database.execute(
        "UPDATE user_commands SET schema_version = 999 WHERE principal_id = ? AND command_id = ?",
        ("operator-a", saved.command_id),
    )

    with pytest.raises(ValueError, match="unsupported user command schema version"):
        store.get(saved.command_id)

    after = database.fetch_one(
        "SELECT schema_version, definition_json FROM user_commands WHERE principal_id = ? AND command_id = ?",
        ("operator-a", saved.command_id),
    )
    assert int(after["schema_version"]) == 999
    assert str(after["definition_json"]) == original_json


def test_malformed_hot_load_payload_fails_closed(tmp_path):
    database = DatabaseManager(tmp_path / "commands.db")
    store = UserCommandStore(database, principal=PrincipalContext("operator-a"))
    saved = store.create(_command())
    raw = json.loads(
        str(
            database.fetch_one(
                "SELECT definition_json FROM user_commands WHERE principal_id = ? AND command_id = ?",
                ("operator-a", saved.command_id),
            )["definition_json"]
        )
    )
    raw["actions"][0]["capability"] = "shell"
    database.execute(
        "UPDATE user_commands SET definition_json = ? WHERE principal_id = ? AND command_id = ?",
        (json.dumps(raw), "operator-a", saved.command_id),
    )

    with pytest.raises(ValueError):
        store.get(saved.command_id)
