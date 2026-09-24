from pathlib import Path


ACCEPTANCE = Path("android/integration/device_acceptance.py")
REMOTE_ACCEPTANCE = Path("android/integration/device_remote_acceptance.py")
EMULATOR_GATE = Path("scripts/test-android-emulator-install.sh")


def source() -> str:
    return ACCEPTANCE.read_text(encoding="utf-8")


def test_acceptance_uses_three_primary_menus_not_legacy_drawer_routes():
    text = source()
    assert 'device.tap("Open navigation menu")' in text
    assert '("Chat", "Workspace", "Settings")' in text
    assert 'for route in ("Chat", "Logic", "Voice", "Projects", "Remote", "Scheduled"' not in text


def test_acceptance_captures_settings_overview_children_and_two_theme_states():
    text = source()
    assert '"settings-overview"' in text
    assert '"Runtime & local AI"' in text
    for route in (
        "Runtime & local AI",
        "Connection",
        "Permissions",
        "Appearance",
        "Plugins",
        "Updates",
        "Diagnostics",
        "About",
    ):
        assert f'"{route}"' in text
    assert 'device.capture(f"settings-{capture_name}")' in text
    assert 'device.tap_tab("Connection")' not in text
    assert '"theme-outrun"' in text
    assert '"theme-light"' in text


def test_acceptance_exercises_width_font_ime_and_saved_state():
    text = source()
    assert '"wide-navigation-rail"' in text
    assert 'target_width_dp=700' in text
    assert '"narrow-large-font"' in text
    assert 'target_width_dp=320' in text
    assert '"ime-composer"' in text
    assert '"recreated-chat-draft"' in text
    assert '"recreated-connection-draft"' in text
    assert 'device.recreate()' in text
    assert 'device.press_back()' in text
    assert '"back-to-chat"' in text


def test_acceptance_checks_accessibility_semantics_without_claiming_talkback():
    text = source()
    assert 'device.assert_accessible_targets(' in text
    assert '"Open navigation menu"' in text
    assert '"accessibility_semantics"' in text
    assert '"talkback_spoken_traversal": False' in text


def test_visual_acceptance_stays_non_mutating_but_dedicated_remote_gate_connects():
    text = source()
    assert '"profiles": device.profiles' in text
    assert '"settings", "put", "system", "font_scale"' in text
    assert '"wm", "size", "reset"' in text
    assert '"keyevent", "4"' in text
    assert 'Connect"' not in text.split("def exercise_three_menu_ui", 1)[-1]

    remote = REMOTE_ACCEPTANCE.read_text(encoding="utf-8")
    gate = EMULATOR_GATE.read_text(encoding="utf-8")
    assert 'type_printable_ascii(device, "?- Result = zara_ready.")' in remote
    assert 'type_printable_ascii(device, "set a timer for 2 hours")' in remote
    assert 'device.await_contains("timer.set", timeout=20.0)' in remote
    assert 'if "local_model.generate.begin" in local_diagnostics:' in remote
    assert '"local_natural_turn_completed": True' in remote
    assert 'device.adb("shell", command)' in remote
    assert 'device.adb("shell", "sh", "-c", command)' not in remote
    assert '"local_turn_completed": True' in remote
    assert 'device.tap("Create client identity")' in remote
    assert 'SecurityAdminClient' in remote
    assert 'device.tap("Pin server key")' in remote
    assert 'device.tap("Connect")' in remote
    assert 'device.await_label("connected"' in remote
    assert 'device.tap("Remote")' in remote
    assert 'signal_turn_acceptance(fixture)' in remote
    assert 'device.await_contains("stock server response"' in remote
    assert '"remote_turn_completed": True' in remote
    assert 'adb -s "$serial" reverse "tcp:$reverse_port" "tcp:$reverse_port"' in gate
    assert 'device_remote_acceptance.py' in gate


def test_remote_gate_fails_closed_when_app_diagnostics_are_missing_or_fatal():
    gate = EMULATOR_GATE.read_text(encoding="utf-8")

    assert 'remote_manifest="$repo_root/android/app/build/reports/device/remote-manifest.json"' in gate
    assert 'if data.get("passed") is not True:' in gate
    assert 'if data.get("app_diagnostics_failure") or data.get("logcat_failure"):' in gate
    assert 'if not data.get("app_diagnostics") or not data.get("logcat"):' in gate
    assert 'fatal_markers = data.get("fatal_log_markers")' in gate
    assert 'if not isinstance(fatal_markers, list):' in gate
    assert 'if fatal_markers:' in gate


def test_remote_gates_follow_settings_overview_instead_of_removed_tabs():
    for path in (
        REMOTE_ACCEPTANCE,
        Path("android/integration/device_remote_recovery_acceptance.py"),
    ):
        remote = path.read_text(encoding="utf-8")
        for removed_tab in ("Runtime", "Connection", "Diagnostics"):
            assert f'device.tap_tab("{removed_tab}")' not in remote, path
        assert 'device.tap("Runtime & local AI")' in remote
        assert 'device.press_back()' in remote
    remote = REMOTE_ACCEPTANCE.read_text(encoding="utf-8")
    assert 'device.await_contains("LOCAL RUNTIME", timeout=20.0)' in remote
    assert 'LOCAL ZARA SERVER' not in remote


def test_remote_diagnostics_uses_device_shell_logcat_for_hosted_emulator(tmp_path):
    import importlib.util
    import sys
    import types

    integration_dir = Path("android/integration").resolve()
    security_admin = types.ModuleType("zara.security_admin")
    security_admin.SecurityAdminClient = object
    zmq_utils = types.ModuleType("zmq.utils")
    zmq_utils.z85 = object()
    previous_security_admin = sys.modules.get("zara.security_admin")
    previous_zmq_utils = sys.modules.get("zmq.utils")
    sys.modules["zara.security_admin"] = security_admin
    sys.modules["zmq.utils"] = zmq_utils
    sys.path.insert(0, str(integration_dir))
    try:
        spec = importlib.util.spec_from_file_location(
            "w03_device_remote_acceptance",
            integration_dir / "device_remote_acceptance.py",
        )
        assert spec is not None and spec.loader is not None
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
    finally:
        sys.path.remove(str(integration_dir))
        if previous_security_admin is None:
            sys.modules.pop("zara.security_admin", None)
        else:
            sys.modules["zara.security_admin"] = previous_security_admin
        if previous_zmq_utils is None:
            sys.modules.pop("zmq.utils", None)
        else:
            sys.modules["zmq.utils"] = previous_zmq_utils

    class HostedEmulatorDevice:
        def adb(self, *arguments: str):
            if arguments == (
                "shell",
                "run-as",
                module.APP_PACKAGE,
                "cat",
                module.APP_DIAGNOSTICS_PATH,
            ):
                return "event=text.submit mode=remote\n"
            if arguments == ("shell", "pidof", module.APP_PACKAGE):
                return "4242\n"
            if arguments == (
                "shell",
                "logcat",
                "-d",
                "--pid",
                "4242",
                "-v",
                "threadtime",
            ):
                return "09-23 19:54:43.678 I Zara: remote turn complete\n"
            raise AssertionError(f"unexpected adb invocation: {arguments!r}")

    evidence = module.collect_app_diagnostics(HostedEmulatorDevice(), tmp_path)

    assert evidence["app_diagnostics"] == "remote-app-diagnostics.log"
    assert evidence["logcat"] == "remote-logcat.log"
    assert evidence["logcat_pid_filtered"] is True
    assert evidence["fatal_log_markers"] == []
    assert "logcat_failure" not in evidence
