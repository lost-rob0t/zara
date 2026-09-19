from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[1]
ANDROID_GATE = REPO_ROOT / "scripts" / "test-android.sh"


def _gate_source() -> str:
    return ANDROID_GATE.read_text(encoding="utf-8")


def test_focused_org_apks_are_built_and_secret_scanned() -> None:
    source = _gate_source()
    for module in ("org-app", "org-todo", "org-sync", "org-notebook"):
        assert f":{module}:assembleDebug" in source
        assert f'{module}/build/outputs/apk/debug/{module}-debug.apk' in source


def test_focused_org_apks_must_share_one_signing_certificate() -> None:
    source = _gate_source()
    assert "org_signing_fingerprint" in source
    assert "verify --print-certs" in source
    assert "Org APK signer mismatch" in source
