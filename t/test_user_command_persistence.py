from zara.database import DatabaseManager
from zara.principals import PrincipalContext
from zara.user_commands import SemanticAction, UserCommandDefinition, UserCommandStore


def test_minimal_semantic_command_survives_restart(tmp_path):
    database = DatabaseManager(tmp_path / "commands.db")
    principal = PrincipalContext("operator-a")
    store = UserCommandStore(database, principal=principal)
    command = UserCommandDefinition(
        command_id="work-mode",
        trigger="work mode",
        actions=(
            SemanticAction(capability="open_app", arguments={"app": "emacs"}),
            SemanticAction(capability="open_app", arguments={"app": "firefox"}),
        ),
    )

    saved = store.create(command)
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
