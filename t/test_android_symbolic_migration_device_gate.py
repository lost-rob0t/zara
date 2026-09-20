from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
EMULATOR_GATE = ROOT / "scripts" / "test-android-emulator-install.sh"
MIGRATION_TEST = (
    ROOT
    / "android"
    / "app"
    / "src"
    / "androidTest"
    / "java"
    / "ai"
    / "zara"
    / "app"
    / "history"
    / "PortableConversationMigrationInstrumentedTest.kt"
)


def test_android_emulator_gate_executes_real_v2_to_v3_sqlite_migration() -> None:
    gate = EMULATOR_GATE.read_text(encoding="utf-8")
    test_source = MIGRATION_TEST.read_text(encoding="utf-8")

    assert ":app:connectedDebugAndroidTest" in gate
    assert (
        "android.testInstrumentationRunnerArguments.class="
        "ai.zara.app.history.PortableConversationMigrationInstrumentedTest"
    ) in gate
    assert "db.version = 2" in test_source
    assert "assertEquals(3, first.readableDatabase.version)" in test_source
    assert "idx_desktop_symbolic_project" in test_source
    assert "idx_desktop_symbolic_turn" in test_source
    assert "first.close()" in test_source
    assert "val reopened = PortableConversationStore(context)" in test_source
    assert "projection.assertPureSymbolic()" in test_source
    assert "assertEquals(0L, projection.providerCalls)" in test_source
    assert "assertEquals(0L, projection.modelCalls)" in test_source
