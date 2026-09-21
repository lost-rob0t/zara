from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
EMULATOR_GATE = ROOT / "scripts" / "test-android-emulator-install.sh"
ANDROID_BUILD = ROOT / "android" / "build.gradle.kts"
STOCK_SERVER_FIXTURE = ROOT / "android" / "integration" / "stock_zara_server_fixture.py"
PURE_SYMBOLIC_INSTALLED_ACCEPTANCE = (
    ROOT / "android" / "integration" / "device_pure_symbolic_acceptance.py"
)
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
PURE_SYMBOLIC_E2E_TEST = (
    ROOT
    / "android"
    / "app"
    / "src"
    / "androidTest"
    / "java"
    / "ai"
    / "zara"
    / "app"
    / "prolog"
    / "AndroidPureSymbolicConversationInstrumentedTest.kt"
)


def test_android_emulator_gate_executes_real_v2_and_v3_to_v4_sqlite_migrations() -> None:
    gate = EMULATOR_GATE.read_text(encoding="utf-8")
    android_build = ANDROID_BUILD.read_text(encoding="utf-8")
    test_source = MIGRATION_TEST.read_text(encoding="utf-8")
    v3_test_source = V3_MIGRATION_TEST.read_text(encoding="utf-8")
    restart_test_source = RESTART_FENCE_TEST.read_text(encoding="utf-8")

    assert ":app:connectedDebugAndroidTest" in gate
    assert (
        "android.testInstrumentationRunnerArguments.class="
        "ai.zara.app.history.PortableConversationMigrationInstrumentedTest,"
        "ai.zara.app.history.PortableConversationV3MigrationInstrumentedTest,"
        "ai.zara.app.history.PortableConversationRestartFenceInstrumentedTest"
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


def test_emulator_gate_runs_real_native_pure_symbolic_multiturn_continuity() -> None:
    gate = EMULATOR_GATE.read_text(encoding="utf-8")
    e2e = PURE_SYMBOLIC_E2E_TEST.read_text(encoding="utf-8")

    assert "ai.zara.app.prolog.AndroidPureSymbolicConversationInstrumentedTest" in gate
    assert "clarificationFollowUpAndProcessRecreationStayPureSymbolicAndDurable" in e2e
    assert 'runNaturalTurn(history, "timer")' in e2e
    assert 'runNaturalTurn(history, "5 minutes")' in e2e
    assert 'runNaturalTurn(history, "thanks")' in e2e
    assert "history = reopenHistory(createConversation = false)" in e2e
    assert "assertZeroModel(clarification)" in e2e
    assert "assertZeroModel(followUp)" in e2e
    assert "assertZeroModel(acknowledgement)" in e2e
    assert "finalProjection.assertPureSymbolic()" in e2e
    assert "assertFalse(finalProjection.providersEnabled)" in e2e
    assert "assertEquals(0L, finalProjection.providerCalls)" in e2e
    assert "assertEquals(0L, finalProjection.modelCalls)" in e2e
    assert "capability-checked execution" in e2e


def test_emulator_gate_runs_installed_pure_symbolic_process_recreation() -> None:
    gate = EMULATOR_GATE.read_text(encoding="utf-8")
    installed = PURE_SYMBOLIC_INSTALLED_ACCEPTANCE.read_text(encoding="utf-8")

    compile(installed, str(PURE_SYMBOLIC_INSTALLED_ACCEPTANCE), "exec")
    assert "android/integration/device_pure_symbolic_acceptance.py" in gate
    assert 'device.adb("shell", "pm", "clear", APP_PACKAGE)' in installed
    assert 'send_chat(device, "/symbolic on", "Pure symbolic mode enabled")' in installed
    assert 'send_chat(device, "timer", "How long should I set the timer for?")' in installed
    assert '"5 minutes"' in installed
    assert 'send_chat(device, "thanks", "welcome")' in installed
    assert installed.count("device.recreate()") >= 3
    assert "capability-checked execution" in installed
    assert 'projection["providers_enabled"] != 0' in installed
    assert 'projection["max_model_calls"] != 0' in installed
    assert 'projection["provider_calls"] != 0' in installed
    assert 'projection["model_calls"] != 0' in installed
    assert 'projection["renderer_provenance"] != "symbolic-dcg/v1"' in installed
    assert 'context_term.startswith("completed_frame(")' in installed
    assert 'projection["conversation_id"]' in installed
    assert "Projection turn id does not match canonical terminal history" in installed


def test_symbolic_emulator_gate_preserves_current_master_device_acceptance() -> None:
    gate = EMULATOR_GATE.read_text(encoding="utf-8")
    fixture = STOCK_SERVER_FIXTURE.read_text(encoding="utf-8")

    # Reconciliation must not trade the current master Android acceptance path
    # for the symbolic migration/restart suite. Keep both fail-closed gates.
    assert "com.google.android.apps.nexuslauncher" in gate
    assert "android/integration/device_acceptance.py" in gate
    assert "android/integration/device_pure_symbolic_acceptance.py" in gate
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


def test_emulator_gate_executes_every_pure_symbolic_android_contract() -> None:
    gate = EMULATOR_GATE.read_text(encoding="utf-8")

    # connectedDebugAndroidTest is run with an explicit class filter. Keep every
    # pure-symbolic acceptance class in that filter so a new androidTest cannot
    # silently compile without ever executing on the emulator.
    required_classes = (
        "ai.zara.app.prolog.NativeTreallaResultBindingInstrumentedTest",
        "ai.zara.app.prolog.AndroidPureSymbolicContextRoundTripInstrumentedTest",
        "ai.zara.app.prolog.AndroidPureSymbolicPersistenceBoundaryInstrumentedTest",
        "ai.zara.app.conversations.CanonicalConversationProjectSwitchFenceInstrumentedTest",
        "ai.zara.app.prolog.AndroidPureSymbolicConversationInstrumentedTest",
    )
    for class_name in required_classes:
        assert class_name in gate
