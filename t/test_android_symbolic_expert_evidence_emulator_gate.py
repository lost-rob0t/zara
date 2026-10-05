from __future__ import annotations

from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[1]
EMULATOR_GATE = REPO_ROOT / "scripts" / "test-android-emulator-install.sh"
INSTRUMENTATION_TEST = (
    "ai.zara.app.history.SymbolicExpertEvidenceTrustEnvelopeInstrumentedTest"
)


def test_expert_evidence_restart_trust_fence_runs_on_real_android_emulator() -> None:
    script = EMULATOR_GATE.read_text(encoding="utf-8")

    assert INSTRUMENTATION_TEST in script, (
        "pure-symbolic expert evidence restart acceptance must execute on the "
        "real installed-emulator gate, not merely compile in androidTest"
    )
