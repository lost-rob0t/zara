from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
FACTORY = ROOT / "android/app/src/main/java/ai/zara/app/prolog/AndroidPureSymbolicConversationFactory.kt"


def test_terminal_symbolic_projection_does_not_publish_pending_conversation_act() -> None:
    """Terminal edge truth must carry the canonical dialogue act, not the pending placeholder."""
    source = FACTORY.read_text(encoding="utf-8")

    pending = source.split("private fun pendingProjection(", 1)[1].split(
        "private fun terminalProjection(", 1
    )[0]
    terminal = source.split("private fun terminalProjection(", 1)[1].split(
        "private fun splitDialogueEnvelope", 1
    )[0]
    envelope = source.split("internal fun dialogueTurnEnvelopeQuery(", 1)[1].split(
        "private fun dialogueTurnPrelude", 1
    )[0]

    assert 'dialogueAct = "conversation"' in pending
    assert "dialogueAct =" in terminal
    assert "dialogueAct = pending.dialogueAct" not in terminal
    assert "ActWire" in envelope
    assert "DIALOGUE_ACT_WIRE_PREFIX" in envelope
    assert 'private const val DIALOGUE_ACT_WIRE_PREFIX = "__zara_act__:"' in source
    assert "Result = ActWire" in envelope


def test_terminal_error_and_cancel_acts_are_explicitly_terminal() -> None:
    """Error/cancel terminal projections must never retain the non-edge pending act."""
    source = FACTORY.read_text(encoding="utf-8")
    terminal = source.split("private fun terminalProjection(", 1)[1].split(
        "private fun splitDialogueEnvelope", 1
    )[0]

    assert '"error" -> "error"' in terminal
    assert '"cancelled" -> "cancelled"' in terminal
