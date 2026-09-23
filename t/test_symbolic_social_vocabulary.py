from __future__ import annotations

import pathlib
import subprocess


ROOT = pathlib.Path(__file__).resolve().parents[1]


def test_symbolic_social_vocabulary_prolog_contract() -> None:
    completed = subprocess.run(
        [
            "swipl",
            "-q",
            "-g",
            "run_tests(symbolic_social_vocabulary),halt(0)",
            "-t",
            "halt(1)",
            "-s",
            str(ROOT / "t" / "symbolic_social_vocabulary.pl"),
        ],
        cwd=ROOT,
        text=True,
        capture_output=True,
        check=False,
    )

    assert completed.returncode == 0, completed.stdout + completed.stderr
