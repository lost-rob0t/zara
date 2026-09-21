from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
INSTALLED_ACCEPTANCE = ROOT / "android" / "integration" / "device_pure_symbolic_acceptance.py"
EMULATOR_GATE = ROOT / "scripts" / "test-android-emulator-install.sh"
SEMANTIC_PARITY_GATE = ROOT / "scripts" / "test-android-semantic-parity.sh"


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


def test_installed_pure_symbolic_transcript_checks_hard_zero_after_each_turn() -> None:
    source = INSTALLED_ACCEPTANCE.read_text(encoding="utf-8")

    assert "def inspect_hard_zero_accounting(" in source
    assert '"providers_enabled"' in source
    assert '"max_model_calls"' in source
    assert '"provider_calls"' in source
    assert '"model_calls"' in source
    assert '"accounting_checkpoints"' in source

    required_stages = (
        "clarification",
        "follow-up-after-restart",
        "social-follow-up",
        "unsupported-no-fallback",
    )
    for stage in required_stages:
        marker = f'inspect_hard_zero_accounting(device, stage="{stage}")'
        assert marker in source, (
            f"Installed pure-symbolic acceptance must snapshot hard-zero accounting at {stage}; "
            "checking only the final projection can hide a transient provider/model call because "
            "the next symbolic turn resets per-turn counters to zero"
        )

    # Every natural turn checkpoint must happen before the following recreation,
    # while that turn's counters are still the current durable projection.
    timer_send = source.index('send_chat(device, "timer"')
    timer_checkpoint = source.index(
        'inspect_hard_zero_accounting(device, stage="clarification")',
        timer_send,
    )
    timer_recreate = source.index("device.recreate()", timer_checkpoint)
    assert timer_send < timer_checkpoint < timer_recreate


def test_emulator_gate_executes_zero_model_and_verified_receipt_fences() -> None:
    source = EMULATOR_GATE.read_text(encoding="utf-8")

    required_classes = (
        "ai.zara.app.conversations.CanonicalConversationStaleUiCompletionFenceInstrumentedTest",
        "ai.zara.app.prolog.AndroidPureSymbolicPreflightFailureInstrumentedTest",
        "ai.zara.app.conversations.CanonicalConversationProjectSwitchVerifiedReceiptEdgeInstrumentedTest",
    )
    for class_name in required_classes:
        assert class_name in source, (
            f"Android pure-symbolic emulator acceptance must execute {class_name}; "
            "compiling androidTest without selecting the class is a false green"
        )


def test_semantic_parity_gate_uses_pinned_trealla_context_wire_contract() -> None:
    source = SEMANTIC_PARITY_GATE.read_text(encoding="utf-8")

    assert 'string_codes("[]", Context0Codes)' in source
    assert "atom_codes(Context0Atom, Context0Codes)" in source
    assert "read_term_from_atom(Context0Atom, Context0, [])" in source
    assert "with_output_to(atom(ContextAtom), write_term(Context1, [quoted(true)]))" in source
    assert "write_term_to_atom(" not in source
    assert "atom_concat('__zara_context__:', ContextAtom, ContextTagged)" in source
    assert "atom_codes(ContextTagged, ContextWireCodes)" in source
    assert "string_codes(ContextWire, ContextWireCodes)" in source
    assert "Results = [Rendered, ContextWire]" in source
    assert "append(PrefixCodes, ContextAtomCodes, ContextWireCodes)" in source
    assert "read_term_from_atom(ContextAtom, Context, [])" in source
    assert "symbolic_dialogue_turn:valid_dialogue_context(Context)" in source
    assert "atom_string(" not in source
    assert 'read_term_from_atom("[]", Context0, [])' not in source
