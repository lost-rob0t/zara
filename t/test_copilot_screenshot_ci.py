from __future__ import annotations

from pathlib import Path


WORKFLOW = Path(__file__).resolve().parents[1] / ".github" / "workflows" / "ci.yml"


def test_ci_renders_deterministic_copilot_artifacts_before_upload() -> None:
    workflow = WORKFLOW.read_text(encoding="utf-8")

    render_step = "Render deterministic Copilot screenshots"
    render_call = "render_copilot_fixtures"
    output_path = 'Path(os.environ["ARTIFACT_DIR"]) / "ui"'

    assert render_step in workflow
    assert render_call in workflow
    assert output_path in workflow
    assert "GITHUB_SHA" in workflow
    assert workflow.index(render_step) < workflow.index("Upload test artifacts")
