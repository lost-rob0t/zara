from __future__ import annotations

import subprocess
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[1]


def test_prolog_hook_registry_contract():
    completed = subprocess.run(
        [
            "swipl",
            "-q",
            "-g",
            (
                "load_files('t/zara_hooks_registry.pl', [silent(true)]), "
                "run_tests(zara_hooks_registry), halt"
            ),
            "-t",
            "halt(1)",
        ],
        cwd=REPO_ROOT,
        check=False,
        capture_output=True,
        text=True,
        timeout=15,
    )

    assert completed.returncode == 0, completed.stdout + completed.stderr
