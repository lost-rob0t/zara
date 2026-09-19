from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
CI_WORKFLOW = (ROOT / ".github" / "workflows" / "ci.yml").read_text()


REQUIRED_ORG_ARTIFACTS = (
    "zara-org-debug-${{ github.event.pull_request.head.sha || github.sha }}",
    "zara-org-todo-debug-${{ github.event.pull_request.head.sha || github.sha }}",
    "zara-org-sync-debug-${{ github.event.pull_request.head.sha || github.sha }}",
    "zara-org-notebook-debug-${{ github.event.pull_request.head.sha || github.sha }}",
)


def test_required_org_apk_uploads_fail_closed_when_artifact_is_missing() -> None:
    for artifact_name in REQUIRED_ORG_ARTIFACTS:
        marker = f"name: {artifact_name}"
        assert marker in CI_WORKFLOW
        _, remainder = CI_WORKFLOW.split(marker, 1)
        upload_block = remainder.split("\n      - name:", 1)[0]
        assert "if-no-files-found: error" in upload_block
