from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
MAIN_ACTIVITY = ROOT / "android/org-notebook/src/main/java/ai/zara/org/notebook/MainActivity.kt"


def _applied_result_block() -> str:
    source = MAIN_ACTIVITY.read_text()
    marker = "is OrgResultApplyResult.Applied -> {"
    assert marker in source, "Org Notebook applied-result branch is missing"
    tail = source.split(marker, 1)[1]
    return tail.split("is OrgResultApplyResult.Stale", 1)[0]


def test_notebook_persists_result_before_advancing_in_memory_revision() -> None:
    block = _applied_result_block()

    write = block.find("write(file, applied.source)")
    source_advance = block.find("source = applied.source")
    revision_advance = block.find("sourceRevision = applied.nextRevision")

    assert write >= 0, "successful notebook results must persist to canonical Org storage"
    assert source_advance >= 0 and revision_advance >= 0, "successful results must advance UI state"
    assert write < source_advance < revision_advance, (
        "canonical Org write must succeed before Notebook advances in-memory source/revision"
    )


def test_notebook_result_write_failure_has_an_explicit_failure_path() -> None:
    block = _applied_result_block()

    assert "runCatching" in block, "canonical result persistence must catch repository/SAF write failures"
    assert ".onFailure" in block, "failed canonical result writes must surface a failure path"
    assert "Result saved" in block, "successful result persistence should retain explicit success status"
