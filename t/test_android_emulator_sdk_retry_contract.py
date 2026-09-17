from pathlib import Path


WORKFLOW = Path(__file__).resolve().parents[1] / ".github" / "workflows" / "ci.yml"


def test_android_screenshot_gate_retries_transient_emulator_package_downloads() -> None:
    workflow = WORKFLOW.read_text(encoding="utf-8")

    assert "Prime Android emulator SDK with bounded retry" in workflow
    assert 'sdk_root="${ANDROID_SDK_ROOT:-${ANDROID_HOME:?ANDROID SDK root is unavailable}}"' in workflow
    assert 'find "$sdk_root/cmdline-tools" -path \'*/bin/sdkmanager\'' in workflow
    assert 'test -n "$sdkmanager_bin" && test -x "$sdkmanager_bin"' in workflow
    assert 'case "$sdkmanager_bin" in' in workflow
    assert '"$sdk_root"/*)' in workflow
    assert "for attempt in 1 2 3; do" in workflow
    assert '"$sdkmanager_bin" --install emulator --channel=0' in workflow
    assert 'rm -rf "$sdk_root/.temp"' in workflow
    assert "emulator SDK install failed after 3 attempts" in workflow
    assert workflow.index("Prime Android emulator SDK with bounded retry") < workflow.index(
        "Capture Android screenshot evidence"
    )
