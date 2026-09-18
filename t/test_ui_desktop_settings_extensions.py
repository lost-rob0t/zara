from __future__ import annotations

import os

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtWidgets import QApplication, QCheckBox, QPushButton

from zara.config import DEFAULT_CONFIG_TOML, ZaraConfig
from zara.desktop.ui_extensions import DesktopUiExtensionHost
from zara.desktop.windows import SettingsWindow
from zara.ui.extensions import (
    UiContribution,
    UiContributionKind,
    UiExtensionRegistry,
    UiSlot,
)


def app() -> QApplication:
    instance = QApplication.instance()
    assert instance is None or isinstance(instance, QApplication)
    return instance or QApplication([])


def test_desktop_settings_renders_shared_settings_slot_and_emits_action(tmp_path):
    app()
    config_path = tmp_path / "xdg" / "config.toml"
    config_path.parent.mkdir()
    config_path.write_text(DEFAULT_CONFIG_TOML, encoding="utf-8")
    config_path.with_name("config.pl").write_text("% user config\n", encoding="utf-8")
    config_path.with_name("init.py").write_text(
        '''
def register(ui):
    ui.add("notes-settings", "settings", "button", "Open Settings", "route:settings", 10, ["desktop", "android"])
''',
        encoding="utf-8",
    )
    root = tmp_path / "repo"
    (root / "kb").mkdir(parents=True)
    (root / "modules").mkdir()
    (root / "main.pl").write_text("main :- true.\n", encoding="utf-8")
    (root / "kb" / "intents.pl").write_text("intent(ok).\n", encoding="utf-8")
    (root / "modules" / "logic.pl").write_text("logic(ok).\n", encoding="utf-8")

    window = SettingsWindow(ZaraConfig(str(config_path)), repo_root=root)
    actions: list[str] = []
    window.ui_action_requested.connect(actions.append)
    try:
        assert window.settings_extensions.slot is UiSlot.SETTINGS
        button = next(
            child
            for child in window.settings_extensions.findChildren(QPushButton)
            if child.text() == "Open Settings"
        )
        button.click()
        assert actions == ["route:settings"]
    finally:
        window.prepare_for_quit()
        window.close()
        window.deleteLater()
        app().processEvents()


def test_plugin_action_is_disabled_until_typed_host_dispatch_exists():
    app()
    registry = UiExtensionRegistry()
    registry.replace_owner(
        "plugin:notes",
        [
            UiContribution(
                id="sync-now",
                slot=UiSlot.SETTINGS,
                kind=UiContributionKind.BUTTON,
                label="Sync now",
                action="plugin:sync-now",
            )
        ],
    )
    host = DesktopUiExtensionHost(registry, UiSlot.SETTINGS)
    actions: list[str] = []
    host.action_requested.connect(actions.append)
    try:
        button = host.findChild(QPushButton)
        assert button is not None
        assert button.isEnabled() is False
        button.click()
        assert actions == []
    finally:
        host.close()
        host.deleteLater()
        app().processEvents()


def test_toggle_is_read_only_until_canonical_setting_state_exists():
    app()
    registry = UiExtensionRegistry()
    registry.replace_owner(
        "plugin:notes",
        [
            UiContribution(
                id="sync",
                slot=UiSlot.SETTINGS,
                kind=UiContributionKind.TOGGLE,
                label="Sync notes",
                action="plugin:set sync {value}",
            )
        ],
    )
    host = DesktopUiExtensionHost(registry, UiSlot.SETTINGS)
    actions: list[str] = []
    host.action_requested.connect(actions.append)
    try:
        toggle = host.findChild(QCheckBox)
        assert toggle is not None
        assert toggle.isEnabled() is False
        assert toggle.isChecked() is False
        toggle.click()
        assert toggle.isChecked() is False
        assert actions == []
    finally:
        host.close()
        host.deleteLater()
        app().processEvents()
