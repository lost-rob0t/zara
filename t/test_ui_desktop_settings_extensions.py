from __future__ import annotations

import os

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtWidgets import QApplication, QPushButton

from zara.config import DEFAULT_CONFIG_TOML, ZaraConfig
from zara.desktop.windows import SettingsWindow


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
    ui.add("notes-settings", "settings", "button", "Open Notes UI", "plugin:open notes", 10, ["desktop", "android"])
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
        assert window.settings_extensions.isVisibleTo(window.stack.widget(5))
        button = next(
            child
            for child in window.settings_extensions.findChildren(QPushButton)
            if child.text() == "Open Notes UI"
        )
        button.click()
        assert actions == ["plugin:open notes"]
    finally:
        window.prepare_for_quit()
        window.close()
        window.deleteLater()
        app().processEvents()
