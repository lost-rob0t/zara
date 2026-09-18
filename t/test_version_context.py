import json
import re
import subprocess
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
SEMVER_RE = re.compile(
    r"^(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)"
    r"(?:-[0-9A-Za-z]+(?:[.-][0-9A-Za-z]+)*)?"
    r"(?:\+[0-9A-Za-z]+(?:[.-][0-9A-Za-z]+)*)?$"
)


def _properties() -> dict[str, str]:
    values: dict[str, str] = {}
    for raw_line in (ROOT / "version.properties").read_text().splitlines():
        line = raw_line.strip()
        if not line or line.startswith("#"):
            continue
        key, value = line.split("=", 1)
        values[key.strip()] = value.strip()
    return values


def test_version_context_is_machine_readable_and_strict():
    properties = _properties()

    assert properties["schema"] == "1"
    assert SEMVER_RE.fullmatch(properties["zara.version"])
    assert SEMVER_RE.fullmatch(properties["release.target"])
    assert int(properties["android.versionCode"]) >= 1
    assert int(properties["release.targetAndroidVersionCode"]) > int(
        properties["android.versionCode"]
    )


def test_active_release_target_is_0_2_2_alpha():
    properties = _properties()

    assert properties["release.target"] == "0.2.2-alpha"
    assert properties["release.targetAndroidVersionCode"] == "4"


def test_version_context_cli_projects_the_canonical_contract():
    result = subprocess.run(
        [sys.executable, str(ROOT / "scripts/version-context.py"), "--format", "json"],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    context = json.loads(result.stdout)

    assert context["schema"] == 1
    assert context["version"] == _properties()["zara.version"]
    assert context["android_version_code"] == int(_properties()["android.versionCode"])
    assert context["release_target"] == "0.2.2-alpha"
    assert context["release_target_android_version_code"] == 4
    assert context["tag"] == f"v{context['version']}"


def test_all_product_version_consumers_use_version_properties():
    setup_py = (ROOT / "setup.py").read_text()
    android_app = (ROOT / "android/app/build.gradle.kts").read_text()
    wear_app = (ROOT / "android/wear-app/build.gradle.kts").read_text()
    release_workflow = (ROOT / ".github/workflows/release.yml").read_text()

    assert "version.properties" in setup_py
    assert 'version="2.0.0"' not in setup_py

    for gradle in (android_app, wear_app):
        assert "version.properties" in gradle
        assert 'versionName = "0.1.2-alpha"' not in gradle
        assert "android.versionCode" in gradle
        assert "zara.version" in gradle

    assert "scripts/version-context.py" in release_workflow
    assert "sed -nE" not in release_workflow


def test_versioned_release_publication_requires_an_explicit_tag():
    workflow = (ROOT / ".github/workflows/release.yml").read_text()

    assert "github.ref_type == 'tag'" in workflow
    assert "gh release create" in workflow
