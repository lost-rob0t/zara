from __future__ import annotations

import pathlib
import subprocess


ROOT = pathlib.Path(__file__).resolve().parents[1]


def test_symbolic_dialogue_prolog_contract() -> None:
    completed = subprocess.run(
        [
            "swipl",
            "-q",
            "-g",
            "run_tests(symbolic_dialogue),halt(0)",
            "-t",
            "halt(1)",
            "-s",
            str(ROOT / "t" / "symbolic_dialogue.pl"),
        ],
        cwd=ROOT,
        text=True,
        capture_output=True,
        check=False,
    )

    assert completed.returncode == 0, completed.stdout + completed.stderr
