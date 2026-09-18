from pathlib import Path

from zara.version_context import load_version_context


ROOT = Path(__file__).resolve().parents[1]


def test_local_history_branch_uses_one_release_version_context():
    context = load_version_context(ROOT / "version.properties")
    phone_gradle = (ROOT / "android/app/build.gradle.kts").read_text()
    wear_gradle = (ROOT / "android/wear-app/build.gradle.kts").read_text()
    setup_py = (ROOT / "setup.py").read_text()

    assert context.release_ready
    assert context.version == context.release_target
    assert context.android_version_code == context.release_target_android_version_code

    for gradle in (phone_gradle, wear_gradle):
        assert "versionCode = zaraAndroidVersionCode" in gradle
        assert "versionName = zaraVersionName" in gradle
        assert 'versionCode = 3' not in gradle
        assert 'versionName = "0.1.2-alpha"' not in gradle

    assert "version=VERSION_CONTEXT.python_version" in setup_py
    assert 'version="2.0.0"' not in setup_py
    assert '"version.properties", "CHANGELOG.md"' in setup_py


def test_local_history_schema_packaging_survives_version_reconciliation():
    phone_gradle = (ROOT / "android/app/build.gradle.kts").read_text()

    assert "GeneratePortableConversationSchema" in phone_gradle
    assert "../../zara/conversation_schema.sql" in phone_gradle
    assert 'into(output.resolve("database"))' in phone_gradle
    assert 'rename { "conversation_schema.sql" }' in phone_gradle
    assert "../../CHANGELOG.md" in phone_gradle
