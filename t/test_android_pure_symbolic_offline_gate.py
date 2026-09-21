from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
INSTALLED_ACCEPTANCE = ROOT / "android" / "integration" / "device_pure_symbolic_acceptance.py"
EMULATOR_GATE = ROOT / "scripts" / "test-android-emulator-install.sh"


def test_installed_pure_symbolic_transcript_is_forced_offline_and_restored() -> None:
    source = INSTALLED_ACCEPTANCE.read_text(encoding="utf-8")

    compile(source, str(INSTALLED_ACCEPTANCE), "exec")
    assert '"cmd", "connectivity", "airplane-mode", verb' in source
    assert '"settings", "get", "global", "airplane_mode_on"' in source
    assert "original_airplane_mode = airplane_mode_enabled(device)" in source
    assert "set_airplane_mode(device, True)" in source
    assert "Pure-symbolic acceptance requires verified offline execution" in source
    assert 'projection["offline_verified"] = True' in source
    assert '"offline_required": True' in source
    assert "finally:" in source
    assert "set_airplane_mode(device, original_airplane_mode)" in source

    # The app must be put offline before the first launch/natural turn, rather
    # than merely recording an offline-looking manifest after the conversation.
    assert source.index("set_airplane_mode(device, True)") < source.index("device.start()")


def test_emulator_gate_executes_stale_and_preflight_zero_model_fences() -> None:
    source = EMULATOR_GATE.read_text(encoding="utf-8")

    required_classes = (
        "ai.zara.app.conversations.CanonicalConversationStaleUiCompletionFenceInstrumentedTest",
        "ai.zara.app.prolog.AndroidPureSymbolicPreflightFailureInstrumentedTest",
    )
    for class_name in required_classes:
        assert class_name in source, (
            f"Android pure-symbolic emulator acceptance must execute {class_name}; "
            "compiling androidTest without selecting the class is a false green"
        )
