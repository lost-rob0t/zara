from pathlib import Path
import re


ROOT = Path(__file__).resolve().parents[1]


def _version_name(path: Path) -> str:
    match = re.search(r'versionName\s*=\s*"([^"]+)"', path.read_text())
    assert match is not None, f"missing versionName in {path}"
    return match.group(1)


def test_store_uses_update_compatible_zara_signer():
    gradle = (ROOT / "android/apps/zara-store/build.gradle.kts").read_text()

    assert 'providers.environmentVariable("ZARA_ANDROID_DEBUG_KEYSTORE")' in gradle
    assert "signingConfigs" in gradle
    assert "storeFile = keyFile" in gradle
    assert 'keyAlias = "androiddebugkey"' in gradle


def test_store_and_phone_versions_are_not_forced_to_match():
    phone_version = _version_name(ROOT / "android/app/build.gradle.kts")
    store_version = _version_name(ROOT / "android/apps/zara-store/build.gradle.kts")
    workflow = (ROOT / ".github/workflows/release.yml").read_text()

    assert phone_version
    assert store_version
    assert "Store/version mismatch" not in workflow
    assert 'echo "store_version=$store_version" >> "$GITHUB_OUTPUT"' in workflow


def test_versioned_release_publishes_store_with_own_version_and_provenance():
    workflow = (ROOT / ".github/workflows/release.yml").read_text()

    assert "android/apps/zara-store/build.gradle.kts" in workflow
    assert "Zara Store versionName is not SemVer" in workflow
    assert 'zara-store-${STORE_VERSION}.apk' in workflow
    assert 'zara-store-${STORE_VERSION}.manifest.txt' in workflow
    assert 'echo "version_name=${STORE_VERSION}"' in workflow
    assert "STORE_APK" in workflow
    assert "STORE_MANIFEST" in workflow
    assert workflow.count("scripts/check-android-apk-signer.sh") >= 2


def test_android_release_evidence_includes_store_apk_hash():
    gate = (ROOT / "scripts/test-android-release.sh").read_text()

    assert "apps/zara-store/build/outputs/apk/debug/zara-store-debug.apk" in gate
    assert "store_apk_sha256=" in gate
