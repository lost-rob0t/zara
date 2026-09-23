from __future__ import annotations

from zara.conversation_schema import PORTABLE_LOCAL_PRINCIPAL_ID
from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationStore, SymbolicConversationProjection
from zara.principals import PrincipalContext


def test_numeric_uid_projection_is_claimed_with_canonical_local_history_on_reopen(tmp_path) -> None:
    path = tmp_path / "legacy-symbolic-owner.db"
    current_owner = PrincipalContext("uid:9001", kind="local-owner")
    database = DatabaseManager(path)
    store = ConversationStore(database, principal=current_owner)
    conversation = store.create_conversation(
        "Legacy symbolic owner",
        conversation_id="legacy-symbolic-owner",
    )
    stored = store.save_symbolic_projection(
        SymbolicConversationProjection(
            conversation_id=conversation.id,
            projection_generation=1,
            runtime_generation=3,
            turn_id="turn-legacy-symbolic-owner",
            outcome="pending",
            dialogue_act="clarify",
            dialogue_state={"intent": "timer", "slot": "duration"},
            unresolved_questions=[{"slot": "duration"}],
            renderer_provenance="symbolic-dcg/v1",
            providers_enabled=False,
            max_model_calls=0,
            provider_calls=0,
            model_calls=0,
        ),
        expected_generation=0,
    )
    stored.assert_pure_symbolic()

    previous_uid = "uid:4242"
    database.execute(
        "UPDATE desktop_conversations SET principal_id = ? WHERE id = ?",
        (previous_uid, conversation.id),
    )
    database.execute(
        "UPDATE desktop_symbolic_projections SET principal_id = ? WHERE conversation_id = ?",
        (previous_uid, conversation.id),
    )
    database.close()

    reopened_database = DatabaseManager(path)
    reopened = ConversationStore(reopened_database, principal=current_owner)
    try:
        recovered = reopened.load_symbolic_projection(conversation.id)
        assert recovered is not None
        recovered.assert_pure_symbolic()
        assert recovered.projection_generation == 1
        assert recovered.runtime_generation == 3
        assert recovered.turn_id == "turn-legacy-symbolic-owner"
        assert recovered.dialogue_act == "clarify"
        assert recovered.dialogue_state == {"intent": "timer", "slot": "duration"}
        assert recovered.unresolved_questions == [{"slot": "duration"}]
        assert recovered.provider_calls == 0
        assert recovered.model_calls == 0

        conversation_owner = reopened_database.fetch_one(
            "SELECT principal_id FROM desktop_conversations WHERE id = ?",
            (conversation.id,),
        )
        projection_owner = reopened_database.fetch_one(
            "SELECT principal_id FROM desktop_symbolic_projections WHERE conversation_id = ?",
            (conversation.id,),
        )
        assert conversation_owner["principal_id"] == PORTABLE_LOCAL_PRINCIPAL_ID
        assert projection_owner["principal_id"] == PORTABLE_LOCAL_PRINCIPAL_ID
    finally:
        reopened_database.close()
