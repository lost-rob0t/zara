from pathlib import Path


WORKFLOW = Path(__file__).resolve().parents[1] / ".github" / "workflows" / "ci.yml"


def test_android_screenshot_gate_retries_transient_emulator_package_downloads() -> None:
    workflow = WORKFLOW.read_text(encoding="utf-8")

    assert "Prime Android emulator SDK with bounded retry" in workflow
    assert 'sdk_root="${ANDROID_SDK_ROOT:-${ANDROID_HOME:?ANDROID SDK root is unavailable}}"' in workflow
    assert 'emulator_bin="$sdk_root/emulator/emulator"' in workflow

    # Assert the discovery contract, not YAML line wrapping. sdkmanager must be
    # discovered from the host SDK, executable, selected deterministically, and
    # fenced so a PATH-injected binary cannot satisfy the release gate.
    assert 'find "$sdk_root/cmdline-tools"' in workflow
    assert "-path '*/bin/sdkmanager'" in workflow
    assert "-executable" in workflow
    assert "| sort -V" in workflow
    assert "| tail -n 1" in workflow
    assert 'test -n "$sdkmanager_bin" && test -x "$sdkmanager_bin"' in workflow
    assert 'case "$sdkmanager_bin" in' in workflow
    assert '"$sdk_root"/*)' in workflow
    assert "refusing sdkmanager outside host SDK root" in workflow

    # A healthy existing emulator should short-circuit. Otherwise a bounded
    # reinstall must clear partial state before every attempt and verify the
    # resulting binary rather than treating sdkmanager exit 0 as sufficient.
    assert 'test -x "$emulator_bin" && "$emulator_bin" -version' in workflow
    assert "for attempt in 1 2 3; do" in workflow
    assert 'rm -rf "$sdk_root/emulator" "$sdk_root/.temp"' in workflow
    assert '"$sdkmanager_bin" --install emulator --channel=0' in workflow
    assert "emulator SDK install failed after 3 attempts" in workflow
    assert workflow.index("Prime Android emulator SDK with bounded retry") < workflow.index(
        "Capture Android screenshot evidence"
    )
