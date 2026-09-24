from pathlib import Path


SESSION = Path("android/app/src/main/java/ai/zara/app/AndroidAppSession.kt")
LOCAL_SERVER = Path("android/app/src/main/java/ai/zara/app/runtime/LocalZaraServer.kt")
SYMBOLIC_FACTORY = Path(
    "android/app/src/main/java/ai/zara/app/prolog/AndroidPureSymbolicConversationFactory.kt"
)
INSTALLED_ACCEPTANCE = Path("android/integration/device_remote_acceptance.py")

CANONICAL_RESOLVER = "zara_portable_semantic_core:resolve_frames"


def _text(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def test_android_local_symbolic_entry_points_use_the_loaded_portable_core_module():
    local_server = _text(LOCAL_SERVER)
    factory = _text(SYMBOLIC_FACTORY)

    assert CANONICAL_RESOLVER in local_server
    assert CANONICAL_RESOLVER in factory
    assert 'val query = "resolve_frames(' not in local_server
    assert 'return "resolve_frames(' not in factory


def test_strict_local_natural_language_cannot_route_to_remote_transport():
    session = _text(SESSION)
    submit_text = session.split("fun submitText(", 1)[1].split(
        "private fun submitAutoRemoteFirst", 1
    )[0]
    local_turn = session.split("internal fun submitLocalText(", 1)[1].split(
        "private fun generateLocalModelTurn", 1
    )[0]

    assert "RuntimeMode.Local -> return submitLocalText(text, localConversationId)" in submit_text
    assert "localServer.resolve(query)" in local_turn
    assert "submitRemoteText(" not in local_turn
    assert "controller.submitText(" not in local_turn
    assert "controller.connect(" not in local_turn


def test_installed_local_gate_proves_readiness_then_canonical_natural_language_before_network():
    acceptance = _text(INSTALLED_ACCEPTANCE)
    exercise = acceptance.split("def exercise_remote_connection", 1)[1]

    readiness = exercise.index('type_printable_ascii(device, "?- Result = zara_ready.")')
    natural = exercise.index('type_printable_ascii(device, "set a timer for 2 hours")')
    symbolic_result = exercise.index('device.await_contains("timer.set"')
    diagnostics = exercise.index("local_diagnostics = read_app_diagnostics(device)")
    no_model = exercise.index('if "local_model.generate.begin" in local_diagnostics:')
    enrollment = exercise.index("enroll_live_server(fixture, client_public)")
    connect = exercise.index('device.tap("Connect")')

    assert readiness < natural < symbolic_result < diagnostics < no_model < enrollment < connect
    assert 'device.tap("Local")' in exercise[:natural]
    assert 'device.await_contains("LOCAL", timeout=5.0)' in exercise[:enrollment]
    assert 'if "local_model.generate.complete" in local_diagnostics:' in exercise
    assert '"local_model_fallback_observed": False' in exercise


def test_installed_local_gate_cannot_be_satisfied_by_remote_or_provider_setup():
    acceptance = _text(INSTALLED_ACCEPTANCE)
    exercise = acceptance.split("def exercise_remote_connection", 1)[1]
    local_gate = exercise.split("# Then enroll the same installed app", 1)[0]

    assert "enroll_live_server(" not in local_gate
    assert 'device.tap("Connect")' not in local_gate
    assert 'device.tap("Remote")' not in local_gate
    assert "server_public" not in local_gate
    assert "SecurityAdminClient(" not in local_gate
