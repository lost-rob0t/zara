from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
ANDROID_GATE = (ROOT / "scripts" / "test-android.sh").read_text()
CI_WORKFLOW = (ROOT / ".github" / "workflows" / "ci.yml").read_text()
LATEST_WORKFLOW = (ROOT / ".github" / "workflows" / "android-latest.yml").read_text()


FOCUSED_ORG_APKS = {
    "org-app": "org-app-debug.apk",
    "org-todo": "org-todo-debug.apk",
    "org-sync": "org-sync-debug.apk",
    "org-roam": "org-roam-debug.apk",
}


def test_android_gate_builds_and_verifies_every_dependency_ready_org_apk() -> None:
    for module, apk_name in FOCUSED_ORG_APKS.items():
        assert f":{module}:assembleDebug" in ANDROID_GATE
        assert f"{module}/build/outputs/apk/debug/{apk_name}" in ANDROID_GATE


def test_ci_uploads_exact_head_org_roam_apk() -> None:
    assert "name: zara-org-roam-debug-${{ github.event.pull_request.head.sha || github.sha }}" in CI_WORKFLOW
    assert "android/org-roam/build/outputs/apk/debug/org-roam-debug.apk" in CI_WORKFLOW


def test_android_latest_publishes_org_roam_with_checksum_and_manifest_entry() -> None:
    expected_fragments = (
        "name: zara-org-roam-debug-${{ github.event.workflow_run.head_sha }}",
        "zara-org-roam-latest.apk",
        "zara-org-roam-latest.apk.sha256",
        "org_roam_apk=zara-org-roam-latest.apk",
        "org_roam_sha256=${org_roam_sha}",
    )
    for fragment in expected_fragments:
        assert fragment in LATEST_WORKFLOW
