from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


def test_org_app_consumes_canonical_zara_version_context():
    gradle = (ROOT / "android/org-app/build.gradle.kts").read_text()

    assert "version.properties" in gradle
    assert 'getProperty("zara.version")' in gradle
    assert 'getProperty("android.versionCode")' in gradle
    assert "versionName = zaraVersionName" in gradle
    assert "versionCode = zaraAndroidVersionCode" in gradle
    assert 'versionName = "0.1.0-alpha"' not in gradle
