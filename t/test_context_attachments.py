from __future__ import annotations

import pytest

from zara.context import (
    ContextAttachmentError, ContextAttachmentNotFound, ContextAttachmentScope,
    ContextAttachmentStore, ContextSensitivity,
)
from zara.principals import PrincipalContext


def store(**kwargs):
    return ContextAttachmentStore(principal=PrincipalContext("owner"), **kwargs)


def acquire(owner, text="reference", **kwargs):
    return owner.acquire_text(text, conversation_id="chat", turn_id="turn", **kwargs)


def render(owner, ids, **kwargs):
    return owner.render(ids, conversation_id="chat", turn_id="turn", **kwargs)


def test_scoped_text_is_untrusted_input_with_no_implicit_path_read(tmp_path):
    path = tmp_path / "secret.txt"
    path.write_text("hidden bytes")
    owner = store()
    item = acquire(owner, str(path), display_name="reference")
    output = render(owner, [item.id, item.id])
    assert "untrusted input" in output
    assert "not authorization" in output
    assert str(path) in output and "hidden bytes" not in output
    assert output.count(str(path)) == 1
    assert not hasattr(owner, "add_path")


@pytest.mark.parametrize("conversation_id,turn_id", [("other", "turn"), ("chat", "other")])
def test_turn_scope_rejects_wrong_identity_without_leaking_payload(conversation_id, turn_id):
    owner = store()
    item = acquire(owner, "private payload")
    with pytest.raises(ContextAttachmentNotFound) as caught:
        owner.render([item.id], conversation_id=conversation_id, turn_id=turn_id)
    assert "private payload" not in str(caught.value) and item.id not in str(caught.value)
    assert "private payload" in render(owner, [item.id])


def test_stores_do_not_share_principal_data_and_missing_ids_fail_uniformly():
    alice = store()
    bob = ContextAttachmentStore(principal=PrincipalContext("bob"))
    item = acquire(alice)
    for target, ref in [(bob, item.id), (alice, "ctx-guessed")]:
        with pytest.raises(ContextAttachmentNotFound, match="context reference unavailable"):
            render(target, [ref])


def test_expiry_revocation_and_turn_cleanup_release_capacity():
    now = [100.0]
    owner = store(clock=lambda: now[0], ttl_seconds=2, max_items=2)
    turn = acquire(owner)
    conversation = acquire(owner, scope=ContextAttachmentScope.CONVERSATION)
    assert owner.release_turn(conversation_id="chat", turn_id="other") == 0
    assert owner.release_turn(conversation_id="chat", turn_id="turn") == 1
    assert render(owner, [conversation.id], max_chars=1000)
    with pytest.raises(ContextAttachmentNotFound):
        render(owner, [turn.id])
    now[0] = 102
    with pytest.raises(ContextAttachmentNotFound):
        render(owner, [conversation.id])
    fresh = acquire(owner)
    assert owner.revoke(fresh.id, conversation_id="other", turn_id="turn") is False
    assert owner.revoke(fresh.id, conversation_id="chat", turn_id="turn") is True
    assert owner.diagnostics().item_count == 0


def test_count_and_utf8_byte_limits_fail_atomically_and_recover():
    owner = store(max_items=2, max_item_bytes=4, max_total_bytes=6)
    first = acquire(owner, "éé")
    with pytest.raises(ContextAttachmentError):
        acquire(owner, "abc")
    second = acquire(owner, "ab")
    with pytest.raises(ContextAttachmentError):
        acquire(owner, "a")
    with pytest.raises(ValueError):
        acquire(owner, "")
    assert owner.diagnostics().item_count == 2
    assert owner.diagnostics().text_bytes == 6
    owner.revoke(first.id, conversation_id="chat", turn_id="turn")
    assert acquire(owner, "1234")
    owner.clear()
    assert owner.diagnostics().item_count == owner.diagnostics().text_bytes == 0
    with pytest.raises(ContextAttachmentNotFound):
        render(owner, [second.id])


def test_secret_text_withheld_and_diagnostics_do_not_expose_content_or_names():
    owner = store()
    item = acquire(owner, "credential payload", display_name="private name",
                   sensitivity=ContextSensitivity.SECRET)
    assert "credential payload" not in render(owner, [item.id])
    assert "withheld" in render(owner, [item.id])
    assert "private name" not in repr(owner.diagnostics())
    assert "credential payload" not in repr(owner.diagnostics())


def test_render_overflow_and_bad_batch_are_atomic():
    owner = store(max_render_chars=200)
    item = acquire(owner, "x" * 150)
    with pytest.raises(ContextAttachmentError):
        render(owner, [item.id])
    with pytest.raises(ContextAttachmentNotFound):
        render(owner, [item.id, "ctx-missing"])
    with pytest.raises(ContextAttachmentError):
        render(owner, [item.id] * 33)
    assert owner.diagnostics().item_count == 1


@pytest.mark.parametrize("kwargs", [
    {"ttl_seconds": 0}, {"ttl_seconds": float("nan")}, {"ttl_seconds": 3601},
    {"max_items": True}, {"max_total_bytes": -1}, {"max_item_bytes": 0},
    {"max_render_chars": 0}, {"principal": "owner"},
])
def test_invalid_store_limits_and_identity_rejected(kwargs):
    options = {"principal": PrincipalContext("owner"), **kwargs}
    with pytest.raises((ValueError, TypeError)):
        ContextAttachmentStore(**options)


@pytest.mark.parametrize("kwargs", [
    {"conversation_id": ""}, {"turn_id": ""}, {"scope": "turn"},
    {"scope": "persistent"}, {"display_name": "x" * 257},
    {"source": "x" * 129}, {"sensitivity": "private"},
])
def test_invalid_acquisition_metadata_rejected_without_retention(kwargs):
    owner = store()
    options = {"conversation_id": "chat", "turn_id": "turn", **kwargs}
    with pytest.raises((ValueError, TypeError)):
        owner.acquire_text("text", **options)
    assert owner.diagnostics().item_count == 0


def test_concurrent_acquisition_cannot_overrun_retention_budget():
    from concurrent.futures import ThreadPoolExecutor
    import threading

    owner = store(max_items=1)
    gate = threading.Barrier(8)

    def attempt(_index):
        gate.wait()
        try:
            return acquire(owner)
        except ContextAttachmentError:
            return None

    with ThreadPoolExecutor(max_workers=8) as executor:
        results = list(executor.map(attempt, range(8)))
    assert sum(item is not None for item in results) == 1
    assert owner.diagnostics().item_count == 1


@pytest.mark.parametrize("text", [None, 123, "", "  ", "\ud800", "x" * 5])
def test_invalid_or_oversize_text_never_retained(text):
    owner = store(max_item_bytes=4)
    with pytest.raises((ValueError, ContextAttachmentError)):
        acquire(owner, text)
    assert owner.diagnostics().item_count == 0


@pytest.mark.parametrize("key", [None, [], 123, "x" * 129, "ctx-missing"])
def test_malformed_references_fail_without_retention_mutation(key):
    owner = store()
    acquire(owner)
    with pytest.raises(ContextAttachmentNotFound, match="context reference unavailable"):
        render(owner, [key])
    assert owner.diagnostics().item_count == 1


def test_empty_render_is_noop_and_render_override_cannot_expand_store_budget():
    owner = store(max_render_chars=100)
    assert render(owner, []) == ""
    item = acquire(owner, "x" * 100)
    with pytest.raises(ContextAttachmentError):
        render(owner, [item.id], max_chars=1000)
    with pytest.raises(ValueError):
        render(owner, [item.id], max_chars=0)


def test_invalid_clock_fails_closed_and_expiry_reclaims_byte_quota():
    now = [1.0]
    owner = store(clock=lambda: now[0], ttl_seconds=1, max_total_bytes=4, max_item_bytes=4)
    acquire(owner, "four")
    now[0] = float("nan")
    with pytest.raises(ValueError):
        acquire(owner, "new")
    now[0] = 2.0
    assert acquire(owner, "new")
    assert owner.diagnostics().text_bytes == 3


def test_conversation_scope_cannot_be_released_by_turn_cleanup():
    owner = store()
    item = acquire(owner, scope=ContextAttachmentScope.CONVERSATION)
    assert owner.release_turn(conversation_id="chat", turn_id="turn") == 0
    assert "reference" in owner.render([item.id], conversation_id="chat", turn_id="next")


def test_turn_leases_are_bounded_and_cannot_be_forged_or_reused_after_clear():
    from dataclasses import replace

    owner = store(max_items=1)
    item = acquire(owner)
    lease = owner.begin_turn(conversation_id="chat", turn_id="turn")
    with pytest.raises(ContextAttachmentError, match="active"):
        owner.begin_turn(conversation_id="chat", turn_id="turn")
    with pytest.raises(ContextAttachmentError, match="limit"):
        owner.begin_turn(conversation_id="chat", turn_id="other")
    owner.finish_turn(replace(lease))
    assert "reference" in render(owner, [item.id])
    owner.clear()
    fresh = acquire(owner, "fresh")
    current = owner.begin_turn(conversation_id="chat", turn_id="turn")
    owner.finish_turn(lease)
    assert "fresh" in render(owner, [fresh.id])
    owner.finish_turn(current)
    owner.finish_turn(current)
    assert owner.diagnostics().item_count == 0


def test_finishing_expired_or_explicitly_revoked_turn_is_safe():
    now = [1.0]
    owner = store(clock=lambda: now[0], ttl_seconds=1)
    item = acquire(owner)
    lease = owner.begin_turn(conversation_id="chat", turn_id="turn")
    assert owner.revoke(item.id, conversation_id="chat", turn_id="turn")
    now[0] = 2.0
    owner.finish_turn(lease)
    assert owner.diagnostics().item_count == 0
    assert acquire(owner)


def test_invalid_clock_callable_and_missing_turn_rejected():
    with pytest.raises(TypeError, match="clock"):
        store(clock=None)
    owner = store()
    with pytest.raises(ValueError, match="turn identity"):
        owner.acquire_text("text", conversation_id="chat")
    with pytest.raises(TypeError, match="lease"):
        owner.finish_turn(None)
    with pytest.raises(ValueError, match="reference"):
        owner.revoke(None, conversation_id="chat", turn_id="turn")
    assert owner.diagnostics().item_count == 0


def test_runtime_binding_is_exclusive_and_foreign_unbind_cannot_clear_data():
    owner = store()
    token, foreign = object(), object()
    item = acquire(owner)
    with pytest.raises(ValueError):
        owner.bind_runtime(None)
    owner.bind_runtime(token)
    owner.bind_runtime(token)
    with pytest.raises(ContextAttachmentError, match="bound"):
        owner.bind_runtime(foreign)
    owner.unbind_runtime(foreign)
    owner.unbind_runtime(None)
    assert render(owner, [item.id])
    owner.unbind_runtime(token)
    assert owner.diagnostics().item_count == 0
    owner.bind_runtime(foreign)
