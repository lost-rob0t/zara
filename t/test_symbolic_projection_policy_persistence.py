from __future__ import annotations

from pathlib import Path

import pytest

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationStore, SymbolicConversationProjection


def _projection(conversation_id: str, **overrides) -> SymbolicConversationProjection:
    values = {
        "conversation_id": conversation_id,
        "projection_generation": 1,
        "runtime_generation": 1,
        "turn_id": "turn-policy-1",
        "outcome": "pending",
        "dialogue_act": "clarify",
        "dialogue_state": {"slot": "target"},
        "renderer_provenance": "symbolic-dcg/v1",
        "providers_enabled": False,
        "max_model_calls": 0,
        "provider_calls": 0,
        "model_calls": 0,
    }
    values.update(overrides)
    return SymbolicConversationProjection(**values)


def test_pure_symbolic_policy_round_trips_through_canonical_store(tmp_path):
    path = tmp_path / "zara.db"
    database = DatabaseManager(path)
    store = ConversationStore(database)
    conversation = store.create_conversation("Policy persistence", conversation_id="conv-policy")

    stored = store.save_symbolic_projection(_projection(conversation.id), expected_generation=0)
    stored.assert_pure_symbolic()
    database.close()

    reopened_database = DatabaseManager(path)
    reopened = ConversationStore(reopened_database)
    recovered = reopened.load_symbolic_projection(conversation.id)
    assert recovered is not None
    assert recovered.providers_enabled is False
    assert recovered.max_model_calls == 0
    assert recovered.provider_calls == 0
    assert recovered.model_calls == 0
    recovered.assert_pure_symbolic()
    reopened_database.close()


def test_pure_symbolic_assertion_fails_closed_on_policy_not_only_usage():
    with pytest.raises(AssertionError, match="providers"):
        _projection("conv-policy", providers_enabled=True).assert_pure_symbolic()
    with pytest.raises(AssertionError, match="max_model_calls"):
        _projection("conv-policy", max_model_calls=1).assert_pure_symbolic()


def test_zero_model_policy_cannot_widen_in_later_generation(tmp_path):
    database = DatabaseManager(tmp_path / "zara.db")
    store = ConversationStore(database)
    conversation = store.create_conversation("Policy fence", conversation_id="conv-policy-fence")
    store.save_symbolic_projection(_projection(conversation.id), expected_generation=0)

    with pytest.raises(RuntimeError, match="provider policy widening"):
        store.save_symbolic_projection(
            _projection(
                conversation.id,
                projection_generation=2,
                runtime_generation=2,
                turn_id="turn-policy-2",
                providers_enabled=True,
            ),
            expected_generation=1,
        )
    with pytest.raises(RuntimeError, match="model-call budget widening"):
        store.save_symbolic_projection(
            _projection(
                conversation.id,
                projection_generation=2,
                runtime_generation=2,
                turn_id="turn-policy-2",
                max_model_calls=1,
            ),
            expected_generation=1,
        )

    recovered = store.load_symbolic_projection(conversation.id)
    assert recovered is not None
    recovered.assert_pure_symbolic()
    assert recovered.projection_generation == 1
    database.close()


def test_portable_schema_and_android_projection_persist_same_policy_contract():
    schema = Path("zara/conversation_schema.sql").read_text(encoding="utf-8")
    android = Path(
        "android/app/src/main/java/ai/zara/app/history/SymbolicConversationProjection.kt"
    ).read_text(encoding="utf-8")

    assert "providers_enabled INTEGER NOT NULL" in schema
    assert "max_model_calls INTEGER NOT NULL" in schema
    assert "val providersEnabled: Boolean" in android
    assert "val maxModelCalls: Long" in android
    assert 'put("providers_enabled"' in android
    assert 'put("max_model_calls"' in android
    assert "providersEnabled" in android and "maxModelCalls" in android


def test_migrated_unknown_policy_must_not_be_manufactured_as_pure_symbolic():
    schema = Path("zara/conversation_schema.sql").read_text(encoding="utf-8")
    # Existing projection rows without durable policy evidence must not be
    # retroactively labelled pure-symbolic. Defaults therefore fail closed
    # until a fresh authoritative projection records disabled providers and a
    # zero model-call budget explicitly.
    assert "providers_enabled INTEGER NOT NULL DEFAULT 1" in schema
    assert "max_model_calls INTEGER NOT NULL DEFAULT 1" in schema
