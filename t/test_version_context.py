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
    assert int(properties["release.targetAndroidVersionCode"]) >= int(
        properties["android.versionCode"]
    )


def test_release_target_is_monotonic_from_current_source():
    from zara.version_context import compare_semver

    properties = _properties()
    precedence = compare_semver(
        properties["release.target"],
        properties["zara.version"],
    )
    assert precedence >= 0
    if precedence == 0:
        assert properties["release.targetAndroidVersionCode"] == properties["android.versionCode"]
    else:
        assert int(properties["release.targetAndroidVersionCode"]) > int(
            properties["android.versionCode"]
        )


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
    assert context["release_target"] == _properties()["release.target"]
    assert context["release_target_android_version_code"] == int(
        _properties()["release.targetAndroidVersionCode"]
    )
    assert context["tag"] == f"v{context['version']}"


def test_all_product_version_consumers_use_version_properties():
    setup_py = (ROOT / "setup.py").read_text()
    android_app = (ROOT / "android/app/build.gradle.kts").read_text()
    wear_app = (ROOT / "android/wear-app/build.gradle.kts").read_text()
    release_workflow = (ROOT / ".github/workflows/release.yml").read_text()

    assert "version.properties" in setup_py
    assert "load_version_context" in setup_py
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
    assert "Release tag refused: current source is not promoted to release.target" in workflow


def test_promoted_master_publication_does_not_depend_on_push_changed_files():
    workflow = (ROOT / ".github/workflows/release.yml").read_text()

    assert "publish_needed: ${{ steps.release.outputs.publish_needed }}" in workflow
    assert 'gh release view "$tag"' in workflow
    assert '"${GITHUB_REF}" == "refs/heads/master"' in workflow
    assert '"$release_ready" == "true"' in workflow
    assert "needs.validate-version-context.outputs.publish_needed == 'true'" in workflow
    assert "contains(github.event.head_commit.modified, 'version.properties')" not in workflow


def test_agent_and_release_skill_require_live_version_context():
    agents = (ROOT / "AGENTS.md").read_text()
    skill = (ROOT / "skills/zara-android-release/SKILL.md").read_text()
    manifest = (ROOT / "MANIFEST.in").read_text()

    assert "scripts/version-context.py --format json" in agents
    assert "version.properties" in skill
    assert "release_ready=true" in skill
    assert "include version.properties" in manifest


def test_android_gradle_imports_precede_version_declarations():
    gradle = (ROOT / "android/app/build.gradle.kts").read_text()

    last_import = gradle.rfind("import ")
    first_declaration = gradle.index("fun loadZaraVersionProperties")
    assert last_import < first_declaration


def test_version_context_rejects_regression_and_unknown_keys(tmp_path):
    import pytest

    from zara.version_context import VersionContextError, load_version_context

    bad_contexts = (
        """schema=1
zara.version=0.2.2-alpha
android.versionCode=4
release.target=0.2.1-alpha
release.targetAndroidVersionCode=5
""",
        """schema=1
zara.version=0.1.2-alpha
android.versionCode=3
release.target=0.2.2-alpha
release.targetAndroidVersionCode=3
""",
        """schema=1
zara.version=0.1.2-alpha
android.versionCode=3
release.target=0.2.2-alpha
release.targetAndroidVersionCode=4
surprise.key=nope
""",
    )
    for index, content in enumerate(bad_contexts):
        path = tmp_path / f"bad-{index}.properties"
        path.write_text(content)
        with pytest.raises(VersionContextError):
            load_version_context(path)


def test_release_ready_only_after_exact_target_promotion(tmp_path):
    from zara.version_context import load_version_context

    current = load_version_context(ROOT / "version.properties")
    assert current.release_ready is (
        current.version == current.release_target
        and current.android_version_code == current.release_target_android_version_code
    )

    promoted = tmp_path / "promoted.properties"
    promoted.write_text(
        "\n".join(
            (
                "schema=1",
                "zara.version=0.2.2-alpha",
                "android.versionCode=4",
                "release.target=0.2.2-alpha",
                "release.targetAndroidVersionCode=4",
                "",
            )
        )
    )
    assert load_version_context(promoted).release_ready is True


def test_android_gradle_plugins_precede_version_declarations():
    for relative in (
        "android/app/build.gradle.kts",
        "android/wear-app/build.gradle.kts",
    ):
        gradle = (ROOT / relative).read_text()
        assert gradle.index("plugins {") < gradle.index("fun loadZaraVersionProperties")


def test_gradle_semver_pattern_accepts_canonical_version():
    canonical = _properties()["zara.version"]
    for relative in (
        "android/app/build.gradle.kts",
        "android/wear-app/build.gradle.kts",
    ):
        gradle = (ROOT / relative).read_text()
        match = re.search(r'Regex\(\s*"""([^"]+)"""', gradle)
        assert match is not None
        assert re.fullmatch(match.group(1), canonical)
