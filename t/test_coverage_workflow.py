from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
CI_WORKFLOW = ROOT / ".github" / "workflows" / "ci.yml"
COVERAGE_WORKFLOW = ROOT / ".github" / "workflows" / "coverage.yml"


def _read(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def _step_block(text: str, name: str, next_name: str) -> str:
    start = text.index(f"- name: {name}")
    end = text.index(f"- name: {next_name}", start)
    return text[start:end]


def _assert_step_local_coverage_base(text: str, consumer: str, next_step: str) -> None:
    assert "id: coverage-base" in text
    assert 'echo "base_ref=$base_ref" >> "$GITHUB_OUTPUT"' in text
    assert 'ZARA_COVERAGE_BASE_REF=$base_ref" >> "$GITHUB_ENV"' not in text

    consumer_block = _step_block(text, consumer, next_step)
    assert "ZARA_COVERAGE_BASE_REF: ${{ steps.coverage-base.outputs.base_ref }}" in consumer_block


def test_full_ci_scopes_trusted_coverage_base_to_test_suite() -> None:
    text = _read(CI_WORKFLOW)
    _assert_step_local_coverage_base(text, "Run test suite", "Run formal AGENTIC-15 verification")

    formal_and_later = text[text.index("- name: Run formal AGENTIC-15 verification") :]
    assert "ZARA_COVERAGE_BASE_REF:" not in formal_and_later


def test_standalone_coverage_scopes_trusted_base_to_coverage_gate() -> None:
    text = _read(COVERAGE_WORKFLOW)
    _assert_step_local_coverage_base(text, "Run coverage gate", "Upload coverage artifact")

    upload_and_later = text[text.index("- name: Upload coverage artifact") :]
    assert "ZARA_COVERAGE_BASE_REF:" not in upload_and_later
