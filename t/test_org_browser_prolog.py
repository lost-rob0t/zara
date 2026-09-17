from __future__ import annotations

import subprocess
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[1]


def test_org_browser_prolog_contracts():
    completed = subprocess.run(
        [
            "swipl",
            "-q",
            "-g",
            (
                "load_files('t/org_browser_config.pl', [silent(true)]), "
                "load_files('t/org_roam_memory_projection.pl', [silent(true)]), "
                "run_tests([org_browser_config, org_roam_memory_projection]), halt"
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
