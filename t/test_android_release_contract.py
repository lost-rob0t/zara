from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


def test_ci_names_phone_apk_artifact_with_exact_source_sha():
    workflow = (ROOT / ".github/workflows/ci.yml").read_text()

    assert "zara-android-debug-${{ github.event.pull_request.head.sha || github.sha }}" in workflow
    assert "zara-android-debug-${SOURCE_SHA}.apk" in workflow
    assert "source_sha=${SOURCE_SHA}" in workflow
    assert "zara-agenda-debug-${{ github.event.pull_request.head.sha || github.sha }}" in workflow


def test_green_master_uses_update_compatible_signer_and_publishes_direct_latest_apks():
    ci = (ROOT / ".github/workflows/ci.yml").read_text()
    workflow = (ROOT / ".github/workflows/android-latest.yml").read_text()

    assert "ZARA_ANDROID_DEBUG_KEYSTORE_B64" in ci
    assert "github.event_name == 'push' && github.ref == 'refs/heads/master'" in ci
    assert "ZARA_ANDROID_DEBUG_KEYSTORE=$keystore" in ci
    assert "workflow_run:" in workflow
    assert "github.event.workflow_run.conclusion == 'success'" in workflow
    assert "github.event.workflow_run.event == 'push'" in workflow
    assert "github.event.workflow_run.head_branch == 'master'" in workflow
    assert "zara-latest.apk" in workflow
    assert "zara-wear-latest.apk" in workflow
    assert "zara-agenda-latest.apk" in workflow
    assert "zara-agenda-latest.apk.sha256" in workflow
    assert "android-latest" in workflow
    assert "mutable=true" in workflow


def test_release_assets_use_semver_name_and_record_provenance():
    workflow = (ROOT / ".github/workflows/release.yml").read_text()

    assert "zara-android-${VERSION}.apk" in workflow
    assert "zara-agenda-${VERSION}.apk" in workflow
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
