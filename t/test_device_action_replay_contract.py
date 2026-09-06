from __future__ import annotations

import pytest

from zara.device_action_replay import (
    DEVICE_ACTION_REPLAY_WINDOW,
    ReplayDisposition,
    classify_action_seq,
)


def test_device_action_replay_window_is_protocol_owned_and_bounded():
    assert DEVICE_ACTION_REPLAY_WINDOW == 256


@pytest.mark.parametrize(
    ("action_seq", "high_water", "expected"),
    [
        (1, 0, ReplayDisposition.FRESH),
        (2, 1, ReplayDisposition.FRESH),
        (257, 1, ReplayDisposition.FRESH),
        (257, 257, ReplayDisposition.DUPLICATE),
        (2, 257, ReplayDisposition.DUPLICATE),
        (1, 257, ReplayDisposition.EXPIRED),
    ],
)
def test_classify_action_seq_fails_closed_across_replay_horizon(
    action_seq,
    high_water,
    expected,
):
    assert classify_action_seq(action_seq, high_water) is expected


def test_forward_gap_does_not_reopen_lower_sequences():
    assert classify_action_seq(500, 5) is ReplayDisposition.FRESH
    assert classify_action_seq(499, 500) is ReplayDisposition.DUPLICATE
    assert classify_action_seq(244, 500) is ReplayDisposition.EXPIRED


@pytest.mark.parametrize("action_seq", [0, -1, True, 1.5, "1", None])
def test_action_sequence_evidence_must_be_positive_integer(action_seq):
    with pytest.raises(ValueError):
        classify_action_seq(action_seq, 0)


@pytest.mark.parametrize("high_water", [-1, True, 1.5, "1", None])
def test_high_water_must_be_nonnegative_integer(high_water):
    with pytest.raises(ValueError):
        classify_action_seq(1, high_water)


@pytest.mark.parametrize("window", [0, -1, True, 1.5, "256", None])
def test_replay_window_must_be_positive_integer(window):
    with pytest.raises(ValueError):
        classify_action_seq(1, 0, window=window)
