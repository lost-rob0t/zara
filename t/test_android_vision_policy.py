from __future__ import annotations

import subprocess
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


def test_android_vision_policy_requires_approval_and_fails_closed() -> None:
    goal = ",".join(
        [
            "use_module('kb/android_control.pl')",
            "kb_android_control:android_vision_action_decision(tap(12,34),require_approval)",
            "\\+ kb_android_control:android_vision_action_decision(shell(id),_)",
            "retractall(kb_android_control:android_vision_policy(_))",
            "assertz(kb_android_control:android_vision_policy(observe_only))",
            "kb_android_control:android_vision_action_decision(tap(12,34),deny)",
            "\\+ kb_android_control:android_vision_action_decision(tap(12,34),require_approval)",
            "halt(0)",
        ]
    )
    completed = subprocess.run(
        ["swipl", "-q", "-g", goal, "-t", "halt(1)"],
        cwd=ROOT,
        capture_output=True,
        text=True,
        timeout=10,
        check=False,
    )

    assert completed.returncode == 0, completed.stderr or completed.stdout
