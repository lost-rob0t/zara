from pathlib import Path


ACCEPTANCE = Path("android/integration/device_acceptance.py")
REMOTE_ACCEPTANCE = Path("android/integration/device_remote_acceptance.py")
EMULATOR_GATE = Path("scripts/test-android-emulator-install.sh")
ANDROID_FLAKE = Path("android/flake.nix")


def source() -> str:
    return ACCEPTANCE.read_text(encoding="utf-8")


def test_acceptance_uses_three_primary_menus_not_legacy_drawer_routes():
    text = source()
    assert 'device.tap("Open navigation menu")' in text
    assert '("Chat", "Workspace", "Settings")' in text
    assert 'for route in ("Chat", "Logic", "Voice", "Projects", "Remote", "Scheduled"' not in text


def test_acceptance_captures_each_settings_tab_and_two_theme_states():
    text = source()
    for tab in (
        "Runtime",
        "Connection",
        "Permissions",
        "Appearance",
        "Plugins",
        "Updates",
        "Diagnostics",
        "About",
    ):
        assert f'"{tab}"' in text
    assert 'device.capture(f"settings-{tab.lower()}")' in text
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
    assert 'device.await_contains("timer.set"' in remote
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


def test_drawer_conversation_overflow_has_pixel_and_geometry_catcher():
    text = source()
    android_flake = ANDROID_FLAKE.read_text(encoding="utf-8")

    assert 'from PIL import Image' in text
    assert '"visual_checks": device.visual_checks' in text
    assert 'device.tap_contains("Actions for ")' in text
    assert 'device.await_label("Rename")' in text
    assert 'device.await_label("Move to project")' in text
    assert 'screenshot_name="drawer-conversation-overflow"' in text
    assert 'luma_span < 18' in text
    assert 'occupied_bins < 4' in text
    assert 'Transient-surface action is semantically present but visually blank' in text
    assert 'union_width > viewport_width * 0.65' in text
    assert 'union_height > viewport_height * 0.45' in text
    assert 'Transient menu is not anchored near its trigger' in text
    assert 'pkgs.python3Packages.pillow' in android_flake


def test_overflow_acceptance_creates_two_chats_before_visual_capture():
    text = source()
    new_chat = 'device.tap_contains("New chat")'
    count = 'device.assert_contains_count("Actions for ", minimum=2)'
    screenshot = 'screenshot_name="drawer-conversation-overflow"'

    assert text.count(new_chat) >= 2
    assert count in text
    assert text.index(new_chat) < text.index(count) < text.index(screenshot)


def test_visual_gate_binds_preopen_trigger_and_same_state_text_twin():
    text = source()

    assert 'trigger_bounds = device.tap_contains("Actions for ")' in text
    assert 'trigger_bounds=trigger_bounds' in text
    assert 'def capture_text_twin(' in text
    assert '"screenshot_sha256": screenshot_sha256' in text
    assert '"text_twin_sha256": text_twin["sha256"]' in text
    assert '"source_sha": getattr(self, "source_sha", None)' in text
    assert '"device_api": getattr(self, "device_api", None)' in text


def test_emulator_gate_verifies_visual_receipt_files_hashes_and_source_identity():
    gate = EMULATOR_GATE.read_text(encoding="utf-8")

    assert 'visual_manifest="$repo_root/android/app/build/reports/device/manifest.json"' in gate
    assert 'drawer-conversation-overflow' in gate
    assert 'screenshot_sha256' in gate
    assert 'text_twin_sha256' in gate
    assert 'hashlib.sha256' in gate
    assert 'receipt.get("source_sha") != expected_source_sha' in gate
    assert 'receipt.get("device_api") != data.get("device", {}).get("api")' in gate
    assert 'expected_actions = {"Pin", "Unpin", "Rename", "Move to project"}' in gate


def test_remote_gate_fails_closed_when_app_diagnostics_are_missing_or_fatal():
    gate = EMULATOR_GATE.read_text(encoding="utf-8")

    assert 'remote_manifest="$repo_root/android/app/build/reports/device/remote-manifest.json"' in gate
    assert 'if data.get("passed") is not True:' in gate
    assert 'if data.get("app_diagnostics_failure") or data.get("logcat_failure"):' in gate
    assert 'if not data.get("app_diagnostics") or not data.get("logcat"):' in gate
    assert 'fatal_markers = data.get("fatal_log_markers")' in gate
    assert 'if not isinstance(fatal_markers, list):' in gate
    assert 'if fatal_markers:' in gate
