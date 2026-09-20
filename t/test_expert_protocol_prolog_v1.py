"""Execute the real symbolic contract in the normal Nix/pytest gate."""

from pathlib import Path
import shutil
import subprocess

ROOT = Path(__file__).resolve().parents[1]


def test_real_prolog_contract():
    swipl = shutil.which("swipl")
    assert swipl is not None, "SWI-Prolog is required; run this gate with nix develop"
    result = subprocess.run(
        [swipl, "-q", "-s", "t/expert_protocol_v1.pl", "-g",
         "(run_tests(expert_protocol_v1) -> halt(0); halt(1))", "-t", "halt(1)"],
        cwd=ROOT, capture_output=True, text=True, timeout=30, check=False,
    )
    assert result.returncode == 0, result.stdout + result.stderr
