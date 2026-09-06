from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[1]
ANDROID_GATE = REPO_ROOT / "scripts" / "test-android.sh"


def test_android_gate_emits_stock_server_log_before_failing_gradle_gate():
    script = ANDROID_GATE.read_text(encoding="utf-8")

    guarded_gradle = "if ! gradle --no-daemon testDebugUnitTest assembleDebug; then"
    assert guarded_gradle in script

    failure_path = script.split(guarded_gradle, 1)[1].split("fi", 1)[0]
    assert 'cat "$interop_log" >&2' in failure_path
    assert 'echo "stock ZaraServer Android interop gate failed" >&2' in failure_path
    assert "exit 1" in failure_path
