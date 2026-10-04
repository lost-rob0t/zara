from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
EMULATOR_GATE = ROOT / "scripts" / "test-android-emulator-install.sh"
INSTRUMENTED_TEST = (
    ROOT
    / "android"
    / "app"
    / "src"
    / "androidTest"
    / "java"
    / "ai"
    / "zara"
    / "app"
    / "ui"
    / "LocalEmbeddingPreferenceStoreInstrumentedTest.kt"
)


def test_embedding_preference_multiprocess_regression_runs_on_real_emulator() -> None:
    gate = EMULATOR_GATE.read_text(encoding="utf-8")
    test = INSTRUMENTED_TEST.read_text(encoding="utf-8")

    assert ":app:connectedDebugAndroidTest" in gate
    assert "ai.zara.app.ui.LocalEmbeddingPreferenceStoreInstrumentedTest" in gate
    assert "voiceProcessColdStartMigratesLegacyPreferenceWithoutCrash" in test
    assert "mainThreadStorageFailureIsTypedAndRecoversAfterRepair" in test
    assert 'assertEquals("$packageName:voice", mainActivity.processName)' in test
    assert "LocalEmbeddingPreferenceStore.create(context)" in test
    assert "runOnMainSync" in test
