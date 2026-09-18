from pathlib import Path


ACCEPTANCE = Path("android/integration/device_acceptance.py")


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


def test_acceptance_manifest_records_profiles_and_never_calls_runtime_actions():
    text = source()
    assert '"profiles": device.profiles' in text
    assert '"settings", "put", "system", "font_scale"' in text
    assert '"wm", "size", "reset"' in text
    assert '"keyevent", "4"' in text
    assert 'Connect"' not in text.split("def exercise_three_menu_ui", 1)[-1]

def test_acceptance_recovers_from_unrelated_system_anr_without_hiding_zara_anr():
    text = source()
    assert "def dismiss_unrelated_system_dialogs" in text
    assert 'title.startswith("Zara")' in text
    assert 'button_text = "Wait"' in text
    assert '== button_text' in text
    assert "self.dismiss_unrelated_system_dialogs()" in text

def test_acceptance_switches_all_runtime_modes_and_leaves_local_selected():
    text = source()
    assert 'for mode in ("Auto", "Local", "Remote")' in text
    assert 'def reveal_contains(self, fragment: str)' in text
    assert 'def tap_contains(self, fragment: str)' in text
    assert 'device.tap_contains("Runtime mode Auto;")' in text
    assert 'device.tap_contains("Runtime mode Remote;")' in text
    assert 'device.await_label("Degraded")' in text
    assert 'device.capture("runtime-mode-remote-chat")' in text
    assert 'device.tap_contains("Runtime mode Local;")' in text
    assert 'device.await_label("Runtime mode Local; selected")' in text
    assert 'device.recreate()' in text
    assert 'device.reveal_contains("Runtime mode Local; selected")' in text
    assert 'device.capture("runtime-mode-local-recreated")' in text

def test_acceptance_captures_truthful_local_model_state():
    text = source()
    assert 'device.reveal("LOCAL MODEL")' in text
    assert 'device.capture("runtime-local-model-state")' in text

def test_acceptance_proves_symbolic_local_chat_state():
    text = source()
    assert 'device.await_label("Offline · Symbolic")' in text
    assert 'device.capture("local-chat-symbolic")' in text
