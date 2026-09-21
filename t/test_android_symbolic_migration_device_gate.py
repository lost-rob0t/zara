from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
EMULATOR_GATE = ROOT / "scripts" / "test-android-emulator-install.sh"
ANDROID_BUILD = ROOT / "android" / "build.gradle.kts"
STOCK_SERVER_FIXTURE = ROOT / "android" / "integration" / "stock_zara_server_fixture.py"
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
RESTART_FENCE_TEST = (
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
    / "PortableConversationRestartFenceInstrumentedTest.kt"
)
VERIFIED_V2_RESTART_TEST = (
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
    / "SymbolicVerifiedOutcomeV2RestartInstrumentedTest.kt"
)


def test_android_emulator_gate_executes_real_v2_and_v3_to_v4_sqlite_migrations() -> None:
    gate = EMULATOR_GATE.read_text(encoding="utf-8")
    android_build = ANDROID_BUILD.read_text(encoding="utf-8")
    test_source = MIGRATION_TEST.read_text(encoding="utf-8")
    v3_test_source = V3_MIGRATION_TEST.read_text(encoding="utf-8")
    restart_test_source = RESTART_FENCE_TEST.read_text(encoding="utf-8")
    verified_v2_restart_source = VERIFIED_V2_RESTART_TEST.read_text(encoding="utf-8")

    assert ":app:connectedDebugAndroidTest" in gate
    assert (
        "android.testInstrumentationRunnerArguments.class="
        "ai.zara.app.history.PortableConversationMigrationInstrumentedTest,"
        "ai.zara.app.history.PortableConversationV3MigrationInstrumentedTest,"
        "ai.zara.app.history.PortableConversationRestartFenceInstrumentedTest,"
        "ai.zara.app.history.PortableConversationLegacyPrincipalInstrumentedTest,"
        "ai.zara.app.history.SymbolicVerifiedOutcomeV2RestartInstrumentedTest"
    ) in gate
    assert 'testInstrumentationRunner = "androidx.test.runner.AndroidJUnitRunner"' in android_build

    assert "db.version = 2" in test_source
    assert "assertEquals(4, first.readableDatabase.version)" in test_source
    assert "idx_desktop_symbolic_project" in test_source
    assert "idx_desktop_symbolic_turn" in test_source
    assert '"providers_enabled"' in test_source
    assert '"max_model_calls"' in test_source
    assert "first.close()" in test_source
    assert "val reopened = PortableConversationStore(context)" in test_source
    assert "projection.assertPureSymbolic()" in test_source
    assert "assertFalse(projection.providersEnabled)" in test_source
    assert "assertEquals(0L, projection.maxModelCalls)" in test_source
    assert "assertEquals(0L, projection.providerCalls)" in test_source
    assert "assertEquals(0L, projection.modelCalls)" in test_source

    assert "db.version = 3" in v3_test_source
    assert "assertEquals(4, first.readableDatabase.version)" in v3_test_source
    assert "migrated.assertPureSymbolic()" in v3_test_source
    assert "authoritative.assertPureSymbolic()" in v3_test_source
    assert "recovered.assertPureSymbolic()" in v3_test_source
    assert "persistedRealCounterCannotBecomeExactZeroAfterReopen" in v3_test_source
    assert 'assertEquals("real", storageClass)' in v3_test_source
    assert "persistedTextCounterCannotBecomeExactZeroAfterReopen" in v3_test_source
    assert 'assertEquals("text", storageClass)' in v3_test_source

    assert "processRecreationInterruptsPendingProjectionAndRejectsLateCompletion" in restart_test_source
    assert "recovered.assertPureSymbolic()" in restart_test_source
    assert 'assertEquals("interrupted", recovered.outcome)' in restart_test_source
    assert "late same-turn completion must be rejected after restart interruption" in restart_test_source

    assert "eightyVerifiedTurnsStayBoundedAndRejectRetiredReplayAfterProcessRecreation" in verified_v2_restart_source
    assert "for (runtimeGeneration in 2L..80L)" in verified_v2_restart_source
    assert "first.close()" in verified_v2_restart_source
    assert "val reopened = PortableConversationStore(context)" in verified_v2_restart_source
    assert "recovered.assertPureSymbolic()" in verified_v2_restart_source
    assert '"retired verified outcome replay rejected"' in verified_v2_restart_source
    assert '"stale symbolic projection write"' in verified_v2_restart_source
    assert "assertEquals(0L, recovered.maxModelCalls)" in verified_v2_restart_source
    assert "assertEquals(0L, recovered.providerCalls)" in verified_v2_restart_source
    assert "assertEquals(0L, recovered.modelCalls)" in verified_v2_restart_source


def test_symbolic_emulator_gate_preserves_current_master_device_acceptance() -> None:
    gate = EMULATOR_GATE.read_text(encoding="utf-8")
    fixture = STOCK_SERVER_FIXTURE.read_text(encoding="utf-8")

    # Reconciliation must not trade the current master Android acceptance path
    # for the symbolic migration/restart suite. Keep both fail-closed gates.
    assert "com.google.android.apps.nexuslauncher" in gate
    assert "android/integration/device_acceptance.py" in gate
    assert "android/integration/stock_zara_server_fixture.py" in gate
    assert "android/integration/device_remote_acceptance.py" in gate
    assert 'data.get("passed") is not True' in gate
    assert 'data.get("app_diagnostics_failure")' in gate
    assert 'data.get("logcat_failure")' in gate
    assert "fatal_log_markers" in gate

    # The installed-APK remote acceptance needs the stock server's canonical
    # security admin socket while the branch readiness probe proves ZARA/1 is
    # authenticated before publishing the fixture.
    assert '"security_admin_path": os.fspath(state.control_socket_path)' in fixture
    assert "_wait_for_transport_ready(" in fixture
    assert "ZmqZaraClient(" in fixture
