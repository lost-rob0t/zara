from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
EMULATOR_GATE = ROOT / "scripts" / "test-android-emulator-install.sh"
ANDROID_BUILD = ROOT / "android" / "build.gradle.kts"
V2_MIGRATION_TEST = (
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
V3_MIGRATION_TEST = (
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
    / "PortableConversationV3MigrationInstrumentedTest.kt"
)


def test_android_emulator_gate_executes_real_v2_and_v3_to_v4_sqlite_migrations() -> None:
    gate = EMULATOR_GATE.read_text(encoding="utf-8")
    android_build = ANDROID_BUILD.read_text(encoding="utf-8")
    v2_source = V2_MIGRATION_TEST.read_text(encoding="utf-8")
    v3_source = V3_MIGRATION_TEST.read_text(encoding="utf-8")

    assert ":app:connectedDebugAndroidTest" in gate
    assert (
        "android.testInstrumentationRunnerArguments.class="
        "ai.zara.app.history.PortableConversationMigrationInstrumentedTest,"
        "ai.zara.app.history.PortableConversationV3MigrationInstrumentedTest"
    ) in gate
    assert 'testInstrumentationRunner = "androidx.test.runner.AndroidJUnitRunner"' in android_build

    assert "db.version = 2" in v2_source
    assert "assertEquals(4, first.readableDatabase.version)" in v2_source
    assert "idx_desktop_symbolic_project" in v2_source
    assert "idx_desktop_symbolic_turn" in v2_source
    assert '"providers_enabled"' in v2_source
    assert '"max_model_calls"' in v2_source
    assert "first.close()" in v2_source
    assert "val reopened = PortableConversationStore(context)" in v2_source
    assert "projection.assertPureSymbolic()" in v2_source
    assert "assertFalse(projection.providersEnabled)" in v2_source
    assert "assertEquals(0L, projection.maxModelCalls)" in v2_source
    assert "assertEquals(0L, projection.providerCalls)" in v2_source
    assert "assertEquals(0L, projection.modelCalls)" in v2_source

    assert "db.version = 3" in v3_source
    assert "version3ProjectionUpgradesFailClosedAndSurvivesHelperRecreation" in v3_source
    assert "assertEquals(4, first.readableDatabase.version)" in v3_source
    assert "assertTrue(migrated.providersEnabled)" in v3_source
    assert "assertEquals(1L, migrated.maxModelCalls)" in v3_source
    assert "authoritative.assertPureSymbolic()" in v3_source
    assert "val reopened = PortableConversationStore(context)" in v3_source
    assert "recovered.assertPureSymbolic()" in v3_source
    assert "persistedRealCounterCannotBecomeExactZeroAfterReopen" in v3_source
