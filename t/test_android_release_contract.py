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
        "Upload Code Editor debug APK",
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
    assert "zara-code-editor-latest.apk" in workflow
    assert "code_editor_apk=zara-code-editor-latest.apk" in workflow
    assert "code_editor_sha256=${code_editor_sha}" in workflow
    assert "zara-wear-latest.apk" in workflow
    assert "android-latest" in workflow
    assert "mutable=true" in workflow
    assert "version_name=${VERSION}" in workflow
    assert "version_code=${VERSION_CODE}" in workflow


def test_release_assets_use_semver_name_and_record_provenance():
    workflow = (ROOT / ".github/workflows/release.yml").read_text()

    assert "zara-android-${VERSION}.apk" in workflow
    assert "zara-android-${VERSION}.manifest.txt" in workflow
    assert "zara-code-editor-${VERSION}.apk" in workflow
    assert "zara-code-editor-${VERSION}.manifest.txt" in workflow
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


def test_code_editor_release_uses_update_compatible_signing_key():
    build = (ROOT / "android/code-editor/build.gradle.kts").read_text()

    assert 'providers.environmentVariable("ZARA_ANDROID_DEBUG_KEYSTORE")' in build
    assert 'signingConfigs' in build
    assert 'storePassword = "android"' in build
    assert 'keyAlias = "androiddebugkey"' in build


def test_android_gate_validates_code_editor_apk_structure_and_signature():
    gate = (ROOT / "scripts/test-android.sh").read_text()

    assert 'check-android-apk-installable.sh" "$code_apk" "ai.zara.code.editor"' in gate


def test_ci_installs_and_launches_code_editor_on_emulator():
    workflow = (ROOT / ".github/workflows/ci.yml").read_text()
    smoke = (ROOT / "scripts/test-android-emulator-install.sh").read_text()

    assert 'bash scripts/test-android-emulator-install.sh "$serial" "$SOURCE_SHA"' in workflow
    assert 'adb -s "$serial" install -r "$code_apk"' in smoke
    assert 'cmd package path ai.zara.code.editor' in smoke
    assert 'am start -W -n ai.zara.code.editor/.MainActivity' in smoke
    assert 'pidof ai.zara.code.editor' in smoke


def test_android_latest_verifies_code_editor_stable_signer_before_publish():
    workflow = (ROOT / ".github/workflows/android-latest.yml").read_text()

    assert 'scripts/check-android-apk-signer.sh "$code_editor_apk"' in workflow
