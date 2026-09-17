from pathlib import Path


WORKFLOW = Path(__file__).resolve().parents[1] / ".github" / "workflows" / "ci.yml"


def test_android_screenshot_gate_retries_transient_emulator_package_downloads() -> None:
    workflow = WORKFLOW.read_text(encoding="utf-8")

    step_name = "Prime Android emulator SDK with current command-line tools"
    assert step_name in workflow
    assert 'sdk_root="${ANDROID_SDK_ROOT:-${ANDROID_HOME:?ANDROID SDK root is unavailable}}"' in workflow
    assert 'emulator_bin="$sdk_root/emulator/emulator"' in workflow
    assert 'android_cache="${HOME:?HOME is unavailable}/.android/cache"' in workflow

    # Pin a current, checksummed sdkmanager inside the ephemeral runner SDK so
    # pre-populated runner images cannot silently select stale package metadata.
    assert 'android_cli_tools_build="15859902"' in workflow
    assert 'android_cli_tools_sha256="4e4c464f145a7512b57d088ac6c278c03c9eea610886b35a5e0804e74eedf583"' in workflow
    assert 'commandlinetools-linux-${android_cli_tools_build}_latest.zip' in workflow
    assert 'curl --fail --location --retry 3 --retry-delay 2' in workflow
    assert "sha256sum --check" in workflow
    assert 'rm -rf "$sdk_root/cmdline-tools/latest"' in workflow
    assert 'sdkmanager_bin="$sdk_root/cmdline-tools/latest/bin/sdkmanager"' in workflow
    assert '"$sdkmanager_bin" --version' in workflow

    # A healthy existing emulator should short-circuit. Otherwise a bounded
    # reinstall clears partial package/cache state before each verified attempt.
    assert 'test -x "$emulator_bin" && "$emulator_bin" -version' in workflow
    assert "for attempt in 1 2 3; do" in workflow
    assert 'rm -rf "$sdk_root/emulator" "$sdk_root/.temp" "$sdk_root/.downloadIntermediates" "$android_cache"' in workflow
    assert '"$sdkmanager_bin" --sdk_root="$sdk_root" --install platform-tools --channel=0' in workflow
    assert '"$sdkmanager_bin" --sdk_root="$sdk_root" --install emulator --channel=0' in workflow
    assert "emulator SDK install failed after 3 attempts" in workflow
    assert workflow.index(step_name) < workflow.index("Capture Android screenshot evidence")
