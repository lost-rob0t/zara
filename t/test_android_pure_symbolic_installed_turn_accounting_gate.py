from __future__ import annotations

import ast
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
INSTALLED_ACCEPTANCE = ROOT / "android" / "integration" / "device_pure_symbolic_acceptance.py"


def top_level_functions(source: str) -> dict[str, ast.FunctionDef | ast.AsyncFunctionDef]:
    tree = ast.parse(source, filename=str(INSTALLED_ACCEPTANCE))
    return {
        node.name: node
        for node in tree.body
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef))
    }


def source_of(
    source: str,
    function: ast.FunctionDef | ast.AsyncFunctionDef,
) -> str:
    return ast.get_source_segment(source, function) or ""


def test_installed_transcript_takes_hard_zero_checkpoint_after_every_ui_visible_turn() -> None:
    source = INSTALLED_ACCEPTANCE.read_text(encoding="utf-8")
    functions = top_level_functions(source)
    exercise = functions.get("exercise_pure_symbolic_dialogue")
    assert exercise is not None, "installed pure-symbolic acceptance entry point disappeared"
    exercise_source = source_of(source, exercise)

    expected_stages = (
        "clarification",
        "follow-up-after-restart",
        "social-follow-up",
        "unsupported-no-fallback",
        "expert-answer",
        "expert-follow-up-after-restart",
    )
    for stage in expected_stages:
        assert f'stage="{stage}"' in exercise_source, (
            "Every UI-visible pure-symbolic turn must be followed by a durable hard-zero "
            f"accounting checkpoint; missing stage {stage!r}."
        )

    assert exercise_source.count("inspect_hard_zero_accounting(") == len(expected_stages), (
        "Installed acceptance must checkpoint every exercised dialogue turn exactly once so a "
        "later zero-usage turn cannot hide earlier provider/model use."
    )


def test_hard_zero_checkpoint_rejects_provider_or_model_usage_and_provider_selection() -> None:
    source = INSTALLED_ACCEPTANCE.read_text(encoding="utf-8")
    functions = top_level_functions(source)
    checkpoint = functions.get("inspect_hard_zero_accounting")
    assert checkpoint is not None, "durable hard-zero checkpoint helper disappeared"
    checkpoint_source = source_of(source, checkpoint)

    required_zero_fields = (
        'projection["providers_enabled"] != 0',
        'projection["max_model_calls"] != 0',
        'projection["provider_calls"] != 0',
        'projection["model_calls"] != 0',
    )
    for fragment in required_zero_fields:
        assert fragment in checkpoint_source, (
            "Pure-symbolic checkpoint must fail closed on any provider/model accounting drift; "
            f"missing {fragment}."
        )

    assert 'SELECT id, provider, model' in checkpoint_source
    assert 'if conversation["provider"] or conversation["model"]' in checkpoint_source, (
        "A zero counter is insufficient if the canonical conversation silently persisted a "
        "provider/model selection."
    )


def test_installed_transcript_asserts_offline_before_first_turn_and_restores_device_state() -> None:
    source = INSTALLED_ACCEPTANCE.read_text(encoding="utf-8")
    functions = top_level_functions(source)
    exercise = functions.get("exercise_pure_symbolic_dialogue")
    assert exercise is not None
    exercise_source = source_of(source, exercise)

    offline_enable = exercise_source.index("set_airplane_mode(device, True)")
    offline_assertion = exercise_source.index("if not airplane_mode_enabled(device)")
    first_turn = exercise_source.index('send_chat(device, "/symbolic on"')
    assert offline_enable < offline_assertion < first_turn, (
        "Installed pure-symbolic acceptance must prove the device is offline before the first "
        "conversation turn, not infer offline behavior from zero counters afterward."
    )

    assert "finally:" in exercise_source
    assert "if airplane_mode_enabled(device) != original_airplane_mode:" in exercise_source
    assert "set_airplane_mode(device, original_airplane_mode)" in exercise_source, (
        "Installed acceptance must restore the emulator's original airplane-mode state even when "
        "a symbolic turn fails."
    )


def test_final_snapshot_and_continuity_check_happen_after_restart_safe_expert_follow_up() -> None:
    source = INSTALLED_ACCEPTANCE.read_text(encoding="utf-8")
    functions = top_level_functions(source)
    exercise = functions.get("exercise_pure_symbolic_dialogue")
    assert exercise is not None
    exercise_source = source_of(source, exercise)

    expert_follow_up = exercise_source.index('stage="expert-follow-up-after-restart"')
    final_snapshot = exercise_source.index("projection = inspect_pure_symbolic_database(device, output)")
    continuity = exercise_source.index("assert_checkpoint_continuity(accounting_checkpoints, projection)")
    assert expert_follow_up < final_snapshot < continuity, (
        "Process-recreation acceptance must compare the restart-safe expert follow-up against a "
        "fresh durable database snapshot before declaring continuity."
    )
