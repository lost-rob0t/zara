from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
CANONICAL_STORE = ROOT / (
    "android/app/src/main/java/ai/zara/app/conversations/CanonicalConversationStore.kt"
)


def test_canonical_ui_metadata_malformed_utf8_fails_closed() -> None:
    """Malformed sidecar bytes must not decode through Unicode replacement characters."""
    source = CANONICAL_STORE.read_text(encoding="utf-8")

    assert "String(bytes, StandardCharsets.UTF_8)" not in source, (
        "String(byte[], UTF_8) silently replaces malformed byte sequences with U+FFFD; "
        "conversation-ui corruption can therefore be accepted as a different project or "
        "conversation identity instead of surfacing ConversationState.loadFailure"
    )
    assert ".newDecoder()" in source, (
        "CanonicalConversationStore must decode persisted UI metadata with a strict UTF-8 decoder"
    )
    assert "CodingErrorAction.REPORT" in source, (
        "Malformed/unmappable UTF-8 must throw so loadMetadata() enters the existing fail-closed "
        "metadataLoadFailure path"
    )
