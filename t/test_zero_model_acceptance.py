from __future__ import annotations

import os
import pathlib
import subprocess


ROOT = pathlib.Path(__file__).resolve().parents[1]
PROVIDER_ENV_KEYS = (
    "ANTHROPIC_API_KEY",
    "GOOGLE_API_KEY",
    "GROQ_API_KEY",
    "MISTRAL_API_KEY",
    "OPENAI_API_KEY",
    "OPENROUTER_API_KEY",
    "TOGETHER_API_KEY",
    "XAI_API_KEY",
    "ZARA_PROLOG_RLM_ROOT",
)


def pure_symbolic_env() -> dict[str, str]:
    env = os.environ.copy()
    for key in PROVIDER_ENV_KEYS:
        env.pop(key, None)
    env["ZARA_PROVIDERS_ENABLED"] = "0"
    env["ZARA_MAX_MODEL_CALLS"] = "0"
    return env


def test_zero_model_headless_acceptance_transcript() -> None:
    completed = subprocess.run(
        [
            "swipl",
            "-q",
            "-g",
            "run_tests(zero_model_acceptance),halt(0)",
            "-t",
            "halt(1)",
            "-s",
            str(ROOT / "t" / "zero_model_acceptance.pl"),
        ],
        cwd=ROOT,
        env=pure_symbolic_env(),
        text=True,
        capture_output=True,
        check=False,
    )

    assert completed.returncode == 0, completed.stdout + completed.stderr
