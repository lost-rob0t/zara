import re
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
CANONICAL_STORE = ROOT / (
    "android/app/src/main/java/ai/zara/app/conversations/CanonicalConversationStore.kt"
)
LEGACY_STORE = ROOT / (
    "android/app/src/main/java/ai/zara/app/conversations/ConversationStore.kt"
)


def test_corrupt_canonical_ui_metadata_fails_closed_instead_of_erasing_project_scope() -> None:
    """A corrupt sidecar must not silently turn durable project-scoped chats into unscoped chats."""
    canonical = CANONICAL_STORE.read_text(encoding="utf-8")
    legacy = LEGACY_STORE.read_text(encoding="utf-8")

    # The previous store already exposes corruption through ConversationState.loadFailure.
    assert 'loadFailure = "Conversation history is corrupt or unsupported"' in legacy

    # CanonicalConversationStore currently owns project binding/selection metadata used by the
    # pure-symbolic project fence. Corruption must therefore be observable and fail closed; an
    # empty metadata state is indistinguishable from a legitimate unscoped conversation and can
    # silently reset project identity after process recreation.
    assert "metadataLoadFailure" in canonical, (
        "CanonicalConversationStore must retain UI metadata load failure instead of silently "
        "replacing corrupt metadata with an empty project scope"
    )
    assert "Conversation UI metadata is corrupt or unsupported" in canonical
    assert "ConversationState(loadFailure = metadataLoadFailure)" in canonical.replace("\n", " ").replace(
        "    ", " "
    ), (
        "snapshot/state must surface corrupt UI metadata through the existing ConversationState "
        "loadFailure contract before any project-scoped conversation can be routed"
    )

    silent_fallback = "catch (_: Exception) {\n            ConversationUiMetadataState()\n        }"
    assert silent_fallback not in canonical, (
        "corrupt UI metadata must not silently erase selected/project/remote metadata"
    )


def test_corrupt_ui_metadata_blocks_interrupted_turn_recovery_mutation() -> None:
    """Corrupt project metadata must fence startup writes to canonical zara.db too."""
    canonical = CANONICAL_STORE.read_text(encoding="utf-8")
    init_start = canonical.index("    init {")
    init_end = canonical.index("\n    @Synchronized", init_start)
    init_block = canonical[init_start:init_end]

    # loadState() performs interrupted-turn recovery in the canonical history owner. Running it
    # while project/selection metadata is corrupt mutates zara.db before the facade has a healthy
    # project scope, violating the fail-closed restart fence. Migration, recovery, and pruning must
    # all stay behind the same metadata-health gate.
    guarded_startup = re.compile(
        r"if \(metadataLoadFailure == null\) \{\s*"
        r"migrateLegacyIfNeeded\(legacyFile\)\s*"
        r"recoverInterruptedTurns\(\)\s*"
        r"pruneMetadata\(\)\s*"
        r"\}",
        re.MULTILINE,
    )
    assert guarded_startup.search(init_block), (
        "corrupt conversation-ui metadata must prevent recoverInterruptedTurns() from mutating "
        "canonical zara.db during process recreation"
    )
