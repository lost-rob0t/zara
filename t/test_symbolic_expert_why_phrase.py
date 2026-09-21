from __future__ import annotations

import os
import pathlib
import subprocess


ROOT = pathlib.Path(__file__).resolve().parents[1]
_PROVIDER_CREDENTIALS = (
    "OPENAI_API_KEY",
    "ANTHROPIC_API_KEY",
    "OPENROUTER_API_KEY",
    "GOOGLE_API_KEY",
    "GEMINI_API_KEY",
    "GROQ_API_KEY",
    "ZAI_API_KEY",
    "AZURE_OPENAI_API_KEY",
    "HF_TOKEN",
)


def _provider_free_env() -> dict[str, str]:
    env = os.environ.copy()
    for name in _PROVIDER_CREDENTIALS:
        env.pop(name, None)
    return env


def _run_swipl(module: pathlib.Path, goal: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [
            "swipl",
            "-q",
            "-s",
            str(module),
            "-g",
            goal,
            "-t",
            "halt(1)",
        ],
        cwd=ROOT,
        env=_provider_free_env(),
        text=True,
        capture_output=True,
        check=False,
    )


def test_normalizer_expands_standalone_u_without_corrupting_you_or_u_words() -> None:
    goal = (
        'normalizer:normalize_string("why did you do that?", Why), '
        "Why = [why,did,you,do,that], "
        'normalizer:normalize_string("u check ubuntu menu", Tokens), '
        "Tokens = [you,check,ubuntu,menu], halt(0)"
    )

    completed = _run_swipl(ROOT / "modules" / "normalizer.pl", goal)

    assert completed.returncode == 0, completed.stdout + completed.stderr


def test_natural_expert_why_phrase_preserves_evidence_with_zero_model_calls() -> None:
    goal = (
        'Previous = answer(expert, "Kotlin inspection complete.", '
        "evidence('expert:kotlin/invocation-4')), "
        'symbolic_dialogue:symbolic_follow_up("why did you do that?", '
        "Previous, Text, Evidence), "
        'Text = "I answered from evidence expert:kotlin/invocation-4.", '
        "Evidence = evidence(renderer('symbolic-dcg/v1'), "
        "provider_calls(0), model_calls(0)), halt(0)"
    )

    completed = _run_swipl(ROOT / "modules" / "symbolic_dialogue.pl", goal)

    assert completed.returncode == 0, completed.stdout + completed.stderr
