"""Org app fleet packaging contract (#1000, #1056, #1177).

Every Org app system ships as its own installable APK with a distinct
applicationId, consumes only the shared Org libraries, and must keep working
when none of the other fleet apps (and not the main Zara phone app) are
installed. Release CI publishes exactly one APK per fleet app with checksums
and provenance manifests.
"""

from __future__ import annotations

import pathlib
import re


ROOT = pathlib.Path(__file__).resolve().parents[1]
ANDROID = ROOT / "android"

FLEET = {
    "org-app": "ai.zara.org.app",
    "org-editor": "ai.zara.org.editor",
    "org-todo": "ai.zara.org.todo",
    "org-reminder": "ai.zara.org.reminder",
    "org-timer": "ai.zara.org.timer",
    "org-roam": "ai.zara.org.roam",
    "org-graph": "ai.zara.org.graph",
    "org-home": "ai.zara.org.home",
}
SHARED_ORG_LIBS = {":org-core", ":org-storage", ":org-sync-core", ":org-surfaces"}


def _gradle(module: str) -> str:
    return (ANDROID / module / "build.gradle.kts").read_text()


def _manifest(module: str) -> str:
    return (ANDROID / module / "src/main/AndroidManifest.xml").read_text()


def test_fleet_modules_are_registered_in_the_shared_gradle_build():
    settings = (ANDROID / "settings.gradle.kts").read_text()
    registered = set(re.findall(r'include\(":(org-[a-z-]+)"\)', settings))
    missing = set(FLEET) - registered
    assert not missing, f"fleet modules missing from android/settings.gradle.kts: {sorted(missing)}"
    assert "org-surfaces" in registered, "shared Org surface library missing from build"


def test_each_fleet_module_is_a_separate_installable_app():
    for module, application_id in FLEET.items():
        gradle = _gradle(module)
        assert f'applicationId = "{application_id}"' in gradle, module
        assert 'namespace = "' in gradle, module
        manifest = _manifest(module)
        assert '<category android:name="android.intent.category.LAUNCHER" />' in manifest, module
        assert "android.intent.action.MAIN" in manifest, module


def test_fleet_application_ids_are_distinct():
    ids = list(FLEET.values())
    assert len(ids) == len(set(ids)), "duplicate fleet applicationIds"


def test_every_fleet_app_consumes_canonical_version_context():
    for module in FLEET:
        gradle = _gradle(module)
        assert "version.properties" in gradle, module
        assert 'getProperty("zara.version")' in gradle, module
        assert 'getProperty("android.versionCode")' in gradle, module


def test_fleet_apps_depend_only_on_shared_org_libraries():
    """No fleet app may depend on the Zara phone app or on another fleet app."""
    project_deps = re.compile(r'(api|implementation)\(project\("([^"]+)"\)\)')
    for module in FLEET:
        deps = {match.group(2) for match in project_deps.finditer(_gradle(module))}
        app_deps = deps - SHARED_ORG_LIBS
        assert not app_deps, (
            f"{module} depends on non-shared modules {sorted(app_deps)}; "
            "fleet apps must work standalone over the shared Org libraries only"
        )


def test_fleet_apps_keep_the_optional_shared_workspace_queryable():
    for module in FLEET:
        manifest = _manifest(module)
        assert 'android:name="ai.zara.org.permission.ORG_HOME"' in manifest, module
        assert 'android:authorities="ai.zara.org.sync.home"' in manifest, module


def test_android_gate_builds_and_tests_every_fleet_app():
    gate = (ROOT / "scripts/test-android.sh").read_text()
    for module in FLEET:
        assert f":{module}:testDebugUnitTest" in gate, module
        assert f":{module}:assembleDebug" in gate, module
    for module, application_id in FLEET.items():
        var = module.replace("-", "_")
        assert (
            f'check-android-apk-installable.sh" "$org_{var}_apk" "{application_id}"' in gate
        ), module


def test_release_workflow_publishes_one_apk_per_fleet_app():
    release = (ROOT / ".github/workflows/release.yml").read_text()
    for module, application_id in FLEET.items():
        assert f'"{module}"' in release, f"{module} missing from release build matrix"
        assert f'"{application_id}"' in release, f"{application_id} missing from release installable checks"
    assert "zara-org-" in release, "release assets must use the zara-org-<app> namespace"
    assert "--prerelease" not in release, (
        "the versioned Org fleet release must be a full release so it becomes the latest release page"
    )


def test_release_workflow_triggers_on_fleet_build_changes():
    release = (ROOT / ".github/workflows/release.yml").read_text()
    for module in FLEET:
        assert f"android/{module}/build.gradle.kts" in release, module


def test_android_latest_channel_carries_every_fleet_app():
    latest = (ROOT / ".github/workflows/android-latest.yml").read_text()
    for module in FLEET:
        assert f'"{module}"' in latest, f"{module} missing from android-latest rolling payload"
    assert "org_${module}_sha256=" in latest, "fleet manifest entries missing from android-latest"
    assert "zara-${module}-latest.apk" in latest, "fleet payload naming loop missing"


def test_version_context_is_promoted_for_the_org_fleet_release():
    properties = (ROOT / "version.properties").read_text()
    values = dict(
        line.split("=", 1)
        for line in properties.splitlines()
        if "=" in line and not line.startswith("#")
    )
    semver = re.compile(
        r"^(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)(-[0-9A-Za-z.-]+)?$"
    )
    assert semver.match(values["zara.version"]), values
    assert values["zara.version"] == values["release.target"], (
        "source must be promoted to the active release target before publication"
    )
    assert values["android.versionCode"] == values["release.targetAndroidVersionCode"]
    assert int(values["android.versionCode"]) >= 5, (
        "the org fleet release line starts at Android versionCode 5"
    )
