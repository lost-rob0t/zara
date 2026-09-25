from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
CANONICAL_STORE = ROOT / (
    "android/app/src/main/java/ai/zara/app/conversations/CanonicalConversationStore.kt"
)


def test_persisted_ui_metadata_identifiers_are_revalidated_after_decode() -> None:
    """Persisted project/selection/remote ids must re-enter the canonical id validator."""
    source = CANONICAL_STORE.read_text(encoding="utf-8")
    load_block = source.split("private fun loadMetadata(): ConversationUiMetadataState", 1)[1].split(
        "private fun persistMetadata()", 1
    )[0]

    assert (
        'val selected = normalizeOptionalId(\n'
        '                    input.readBoundedString(MAX_ID_CHARS),\n'
        '                    MAX_ID_CHARS,\n'
        '                    "Selected conversation id",\n'
        '                )'
        in load_block
    ), (
        "conversation-ui.bin selectedConversationId must be revalidated after UTF-8 decoding; "
        "otherwise control-character corruption can be accepted as durable UI identity"
    )
    assert (
        'val projectId = normalizeOptionalId(\n'
        '                            input.readBoundedString(MAX_PROJECT_ID_CHARS),\n'
        '                            MAX_PROJECT_ID_CHARS,\n'
        '                            "Project id",\n'
        '                        )'
        in load_block
    ), (
        "persisted projectId must cross the same bounded/control-character validator as live "
        "project moves before restart recovery trusts project scope"
    )
    assert (
        'val remoteId = normalizeOptionalId(\n'
        '                            input.readBoundedString(MAX_ID_CHARS),\n'
        '                            MAX_ID_CHARS,\n'
        '                            "Remote conversation id",\n'
        '                        )'
        in load_block
    ), (
        "persisted remote conversation identity must be revalidated instead of accepting any "
        "well-formed UTF-8 byte sequence"
    )
