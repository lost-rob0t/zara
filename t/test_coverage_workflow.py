from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
CI_WORKFLOW = ROOT / ".github" / "workflows" / "ci.yml"
COVERAGE_WORKFLOW = ROOT / ".github" / "workflows" / "coverage.yml"
COVERAGE_SCRIPT = ROOT / "scripts" / "test-coverage.sh"


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
    _assert_step_local_coverage_base(text, "Run all tests", "Run formal AGENTIC-15 verification")

    formal_and_later = text[text.index("- name: Run formal AGENTIC-15 verification") :]
    assert "ZARA_COVERAGE_BASE_REF:" not in formal_and_later


def test_standalone_coverage_scopes_trusted_base_to_coverage_gate() -> None:
    text = _read(COVERAGE_WORKFLOW)
    _assert_step_local_coverage_base(text, "Run branch-aware coverage gate", "Upload coverage reports")

    upload_and_later = text[text.index("- name: Upload coverage reports") :]
    assert "ZARA_COVERAGE_BASE_REF:" not in upload_and_later


def test_coverage_script_emits_junit_in_uploaded_artifact_dir() -> None:
    text = _read(COVERAGE_SCRIPT)
    assert '--junit-xml="$ARTIFACT_DIR/junit.xml"' in text


def test_coverage_script_generates_required_audio_fixtures_before_pytest() -> None:
    text = _read(COVERAGE_SCRIPT)
    fixture_dir = 'fixture_dir="$repo_root/t/fixtures/audio"'
    generator = 'python "$repo_root/scripts/generate-audio-fixtures.py" "$fixture_dir"'
    pytest = "python -m pytest"

    assert fixture_dir in text
    assert generator in text
    assert text.index(generator) < text.index(pytest)
    assert 'trap \'rm -rf "$fixture_dir"\' EXIT' in text
