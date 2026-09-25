from __future__ import annotations

import importlib.util
from pathlib import Path
import sys

import pytest


ROOT = Path(__file__).resolve().parents[1]
INTEGRATION = ROOT / "android" / "integration"
ACCEPTANCE = INTEGRATION / "device_pure_symbolic_acceptance.py"


def load_acceptance_module():
    sys.path.insert(0, str(INTEGRATION))
    try:
        spec = importlib.util.spec_from_file_location("device_pure_symbolic_acceptance", ACCEPTANCE)
        assert spec is not None and spec.loader is not None
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        return module
    finally:
        sys.path.remove(str(INTEGRATION))


def expert_checkpoint(stage: str, turn_id: str) -> dict[str, object]:
    return {
        "stage": stage,
        "conversation_id": "conversation-1",
        "turn_id": turn_id,
        "expert_evidence": [{"ref": "expert:diagnosis:1"}],
        "providers_enabled": False,
        "max_model_calls": 0,
        "provider_calls": 0,
        "model_calls": 0,
    }


def final_projection(turn_id: str) -> dict[str, object]:
    return {
        "conversation_id": "conversation-1",
        "turn_id": turn_id,
        "expert_evidence": [{"ref": "expert:diagnosis:1"}],
        "providers_enabled": False,
        "max_model_calls": 0,
        "provider_calls": 0,
        "model_calls": 0,
    }


def test_checkpoint_continuity_accepts_distinct_committed_turns_and_terminal_snapshot() -> None:
    module = load_acceptance_module()
    checkpoints = [
        expert_checkpoint("expert-answer", "turn-1"),
        expert_checkpoint("expert-follow-up-after-restart", "turn-2"),
    ]

    module.assert_checkpoint_continuity(checkpoints, final_projection("turn-2"))


def test_checkpoint_continuity_rejects_stale_turn_reuse_after_recreation() -> None:
    module = load_acceptance_module()
    checkpoints = [
        expert_checkpoint("expert-answer", "turn-1"),
        expert_checkpoint("expert-follow-up-after-restart", "turn-1"),
    ]

    with pytest.raises(AssertionError, match="reused stale canonical turn_id"):
        module.assert_checkpoint_continuity(checkpoints, final_projection("turn-1"))


def test_checkpoint_continuity_rejects_final_snapshot_from_pre_restart_turn() -> None:
    module = load_acceptance_module()
    checkpoints = [
        expert_checkpoint("expert-answer", "turn-1"),
        expert_checkpoint("expert-follow-up-after-restart", "turn-2"),
    ]

    with pytest.raises(AssertionError, match=r"terminal restart-safe `why\?` turn"):
        module.assert_checkpoint_continuity(checkpoints, final_projection("turn-1"))


def test_checkpoint_continuity_rejects_conversation_identity_swap_after_recreation() -> None:
    module = load_acceptance_module()
    checkpoints = [
        expert_checkpoint("expert-answer", "turn-1"),
        expert_checkpoint("expert-follow-up-after-restart", "turn-2"),
    ]
    checkpoints[1]["conversation_id"] = "conversation-2"

    with pytest.raises(AssertionError, match="replaced canonical conversation identity"):
        module.assert_checkpoint_continuity(checkpoints, final_projection("turn-2"))


def test_checkpoint_continuity_rejects_expert_evidence_swap_after_recreation() -> None:
    module = load_acceptance_module()
    checkpoints = [
        expert_checkpoint("expert-answer", "turn-1"),
        expert_checkpoint("expert-follow-up-after-restart", "turn-2"),
    ]
    checkpoints[1]["expert_evidence"] = [{"ref": "expert:diagnosis:2"}]

    with pytest.raises(AssertionError, match="replaced the admitted canonical expert evidence ref"):
        module.assert_checkpoint_continuity(checkpoints, final_projection("turn-2"))


def test_checkpoint_continuity_rejects_final_snapshot_evidence_swap() -> None:
    module = load_acceptance_module()
    checkpoints = [
        expert_checkpoint("expert-answer", "turn-1"),
        expert_checkpoint("expert-follow-up-after-restart", "turn-2"),
    ]
    projection = final_projection("turn-2")
    projection["expert_evidence"] = [{"ref": "expert:diagnosis:2"}]

    with pytest.raises(AssertionError, match="replaced the admitted canonical expert evidence ref"):
        module.assert_checkpoint_continuity(checkpoints, projection)
