from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


def test_ci_names_phone_apk_artifact_with_exact_source_sha():
    workflow = (ROOT / ".github/workflows/ci.yml").read_text()

    assert "zara-android-debug-${{ github.event.pull_request.head.sha || github.sha }}" in workflow
    assert "zara-android-debug-${SOURCE_SHA}.apk" in workflow
    assert "source_sha=${SOURCE_SHA}" in workflow


def test_android_release_candidate_apk_uploads_require_green_gate():
    workflow = (ROOT / ".github/workflows/ci.yml").read_text()
    android_job = workflow.split("\n  android:\n", 1)[1]

    for upload_name in (
        "Upload exact-SHA phone debug APK",
        "Upload Wear debug APK",
    ):
        step = android_job.split(f"      - name: {upload_name}\n", 1)[1].split("\n      - name:", 1)[0]
        assert "if: success()" in step
        assert "if: always()" not in step


def test_each_master_push_builds_and_rolls_direct_latest_apks():
    workflow = (ROOT / ".github/workflows/android-latest.yml").read_text()

    assert "push:" in workflow
    assert "branches:\n      - master" in workflow
    assert "workflow_run:" not in workflow
    assert "github.event.workflow_run" not in workflow
    assert "ref: ${{ github.sha }}" in workflow
    assert "ZARA_ANDROID_DEBUG_KEYSTORE_B64" in workflow
    assert "ZARA_ANDROID_DEBUG_KEYSTORE=$keystore" in workflow
    assert "scripts/test-android.sh" in workflow
    assert "android-master-${{ github.sha }}" in workflow
    assert "group: android-latest-publish" in workflow
    assert "cancel-in-progress: false" in workflow
    assert "zara-latest.apk" in workflow
    assert "zara-wear-latest.apk" in workflow
    assert "android-latest" in workflow
    assert "mutable=true" in workflow
    assert "version_name=${VERSION}" in workflow
    assert "version_code=${VERSION_CODE}" in workflow


def test_release_assets_use_semver_name_and_record_provenance():
    workflow = (ROOT / ".github/workflows/release.yml").read_text()

    assert "zara-android-${VERSION}.apk" in workflow
    assert "zara-android-${VERSION}.manifest.txt" in workflow
    assert "source_sha=${GITHUB_SHA}" in workflow
    assert "version_name=${VERSION}" in workflow
    assert "version_code=${VERSION_CODE}" in workflow


def test_repo_documents_the_semver_release_skill():
    agents = (ROOT / "AGENTS.md").read_text()
    skill = (ROOT / "skills/zara-android-release/SKILL.md").read_text()

    assert "$zara-android-release" in agents
    assert "exact candidate SHA" in skill
    assert "versionCode" in skill
    assert "Never move or overwrite a release tag" in skill
    assert "android-latest" in skill
    assert "downloaded GitHub artifact" in skill
