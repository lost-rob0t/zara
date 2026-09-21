import re
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
CANONICAL_STORE = ROOT / (
    "android/app/src/main/java/ai/zara/app/conversations/CanonicalConversationStore.kt"
)
SYMBOLIC_SCOPE = ROOT / (
    "android/app/src/main/java/ai/zara/app/history/SymbolicProjectScope.kt"
)


def _constant(source: str, name: str) -> int:
    match = re.search(rf"private const val {name} = (\d+)", source)
    assert match is not None, f"missing {name}"
    return int(match.group(1))


def test_canonical_ui_metadata_does_not_narrow_symbolic_project_identity() -> None:
    """UI metadata must accept every project id accepted by the symbolic scope contract."""
    canonical = CANONICAL_STORE.read_text(encoding="utf-8")
    symbolic = SYMBOLIC_SCOPE.read_text(encoding="utf-8")

    canonical_limit = _constant(canonical, "MAX_PROJECT_ID_CHARS")
    symbolic_limit = _constant(symbolic, "MAX_SYMBOLIC_PROJECT_ID_CHARS")

    assert symbolic_limit == 512
    assert canonical_limit == symbolic_limit, (
        "CanonicalConversationStore narrows project ids before pure-symbolic routing: "
        f"ui={canonical_limit}, symbolic={symbolic_limit}"
    )
    assert "normalizeOptionalId(projectId, MAX_PROJECT_ID_CHARS, \"Project id\")" in canonical
    assert "writeBoundedString(row.projectId.orEmpty(), MAX_PROJECT_ID_CHARS)" in canonical
    assert "readBoundedString(MAX_PROJECT_ID_CHARS).ifEmpty { null }" in canonical
