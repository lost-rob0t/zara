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


def test_store_cannot_define_parallel_plugin_runtime_authority():
    source_root = ROOT / "android/apps/zara-store/src/main/java"
    forbidden_types = {
        "CapabilityRegistry",
        "PermissionRegistry",
        "PluginHealthRegistry",
        "PluginManager",
        "PluginRegistry",
        "PluginRuntimeState",
    }
    declaration = re.compile(
        r"\b(?:data\s+class|sealed\s+class|class|interface|object|enum\s+class)\s+"
        r"(" + "|".join(sorted(forbidden_types)) + r")\b"
    )
    offenders = []

    for path in sorted(source_root.rglob("*.kt")):
        for match in declaration.finditer(path.read_text()):
            offenders.append(f"{path.relative_to(ROOT)}:{match.group(1)}")

    assert not offenders, (
        "Zara Store owns package artifact lifecycle only; plugin runtime/trust/permission "
        "authority must remain in the canonical plugin host/registry: " + ", ".join(offenders)
    )


def test_store_package_state_contains_artifact_lifecycle_only():
    contract = (
        ROOT
        / "android/apps/zara-store/src/main/java/ai/zara/store/catalog/ZaraCatalogContract.kt"
    ).read_text()
    state = re.search(r"data class StorePackageState\((.*?)\)\s*\{", contract, re.DOTALL)

    assert state is not None
    fields = state.group(1).lower()
    for forbidden in ("enabled", "trusted", "permission", "capability", "health", "ready", "runtime"):
        assert forbidden not in fields, (
            f"StorePackageState must not persist canonical plugin host field {forbidden!r}"
        )
