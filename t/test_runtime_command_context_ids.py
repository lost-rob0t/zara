from __future__ import annotations

import pytest

from zara.runtime.commands import MAX_CONTEXT_IDS, SubmitTurn


def test_submit_turn_normalizes_context_ids_before_runtime_dispatch():
    command = SubmitTurn(
        text="continue from these references",
        context_ids=["  dotfiles:flake  ", "turn:prior-result"],
    )

    assert command.context_ids == ("dotfiles:flake", "turn:prior-result")
    assert isinstance(command.context_ids, tuple)


@pytest.mark.parametrize(
    "context_ids",
    [
        [""],
        ["   "],
        ["bad\x00context"],
        ["x" * 129],
        [object()],
    ],
)
def test_submit_turn_rejects_malformed_context_ids(context_ids):
    with pytest.raises(ValueError):
        SubmitTurn(text="hello", context_ids=context_ids)


def test_submit_turn_rejects_unbounded_context_reference_sets():
    with pytest.raises(ValueError, match="maximum count"):
        SubmitTurn(
            text="hello",
            context_ids=[f"ctx-{index}" for index in range(MAX_CONTEXT_IDS + 1)],
        )


def test_submit_turn_preserves_empty_context_set_for_existing_callers():
    command = SubmitTurn(text="hello")

    assert command.context_ids == ()
