from __future__ import annotations

from pathlib import Path


WORKFLOW = Path(__file__).resolve().parents[1] / ".github" / "workflows" / "ci.yml"


def test_ci_renders_deterministic_copilot_artifacts_before_upload() -> None:
    workflow = WORKFLOW.read_text(encoding="utf-8")

    render_step = "Render deterministic Copilot screenshots"
    render_command = "scripts/desktop_ui_evidence.py"
    output_argument = '--output "$ARTIFACT_DIR/ui"'

    assert render_step in workflow
    assert render_command in workflow
    assert output_argument in workflow
    assert workflow.index(render_step) < workflow.index("Upload test artifacts")


def test_ci_stamps_reviewed_source_head_not_pull_request_merge_commit() -> None:
    workflow = WORKFLOW.read_text(encoding="utf-8")

    assert "github.event.pull_request.head.sha || github.sha" in workflow
    assert "ZARA_SOURCE_COMMIT" in workflow
    assert '--source-sha "$ZARA_SOURCE_COMMIT"' in workflow
