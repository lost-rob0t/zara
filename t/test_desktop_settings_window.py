from __future__ import annotations

import os

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtWidgets import QApplication

from zara.config import DEFAULT_CONFIG_TOML, ZaraConfig
from zara.desktop.theme import apply_desktop_theme
from zara.desktop.windows import SettingsWindow


def app() -> QApplication:
    instance = QApplication.instance()
    assert instance is None or isinstance(instance, QApplication)
    return instance or QApplication([])


def make_window(tmp_path, *, reload_result=True):
    app()
    config_path = tmp_path / "xdg" / "config.toml"
    config_path.parent.mkdir()
    config_path.write_text(DEFAULT_CONFIG_TOML, encoding="utf-8")
    prolog_path = config_path.with_name("config.pl")
    prolog_path.write_text("% actual user config\n", encoding="utf-8")
    root = tmp_path / "repo"
    (root / "kb").mkdir(parents=True)
    (root / "modules").mkdir()
    (root / "main.pl").write_text("main :- true.\n", encoding="utf-8")
    (root / "kb" / "intents.pl").write_text("intent(ok).\n", encoding="utf-8")
    (root / "modules" / "logic.pl").write_text("logic(ok).\n", encoding="utf-8")
    reload_calls = []

    def reload_config():
        reload_calls.append(True)
        return reload_result

    window = SettingsWindow(
        ZaraConfig(str(config_path)),
        repo_root=root,
        prolog_reload=reload_config,
    )
    return window, config_path, prolog_path, reload_calls


def dispose(window: SettingsWindow) -> None:
    window.prepare_for_quit()
    window.close()
    window.deleteLater()
    app().processEvents()


def test_settings_has_complete_navigation_and_many_real_controls(tmp_path):
    window, _, _, _ = make_window(tmp_path)
    try:
        assert window.objectName() == "zaraSettings"
        assert [window.category_list.item(index).text() for index in range(window.category_list.count())] == [
            "Appearance",
            "Assistant",
            "Voice & Speech",
            "Tools & Privacy",
            "Prolog",
            "Advanced",
        ]
        assert {
            "desktop.theme",
            "llm.provider",
            "llm.model",
            "llm.endpoint",
            "llm.history_limit",
            "agent.max_steps",
            "agent.system_prompt",
            "wake.threshold",
            "stt.provider",
            "stt.model",
            "stt.device",
            "tts.provider",
            "tools.calculator",
            "tools.query_prolog",
            "tools.file_tools",
            "memory.enabled",
            "latency.enabled",
            "database.path",
            "prolog.main_file",
            "prolog.load_on_startup",
        } <= set(window.setting_widgets)
        assert len(window.setting_widgets) >= 20
        assert [button.theme_key for button in window.theme_buttons] == [
            "signal-cabin",
            "dotfiles-outrun",
            "nord",
            "dracula",
            "chatgpt-neutral",
        ]
    finally:
        dispose(window)


def test_theme_previews_live_and_save_persists_all_changed_settings(tmp_path):
    qt_app = app()
    window, config_path, _, _ = make_window(tmp_path)
    previews = []
    window.theme_preview_requested.connect(previews.append)
    try:
        theme = window.setting_widgets["desktop.theme"]
        theme.setCurrentIndex(theme.findData("dotfiles-outrun"))
        window.setting_widgets["llm.model"].setText("qwen3:14b")
        window.setting_widgets["agent.max_steps"].setValue(17)
        qt_app.processEvents()
        assert previews[-1] == "dotfiles-outrun"

        window.save_settings()
        text = config_path.read_text(encoding="utf-8")
        assert 'theme = "dotfiles-outrun"' in text
        assert 'model = "qwen3:14b"' in text
        assert "max_steps = 17" in text
        assert window.feedback_label.text() == "Settings saved. Restart Zara to apply runtime changes."
    finally:
        dispose(window)


def test_theme_previews_keep_a_complete_card_height(tmp_path):
    qt_app = app()
    window, _, _, _ = make_window(tmp_path)
    try:
        window.resize(1180, 800)
        window.show()
        apply_desktop_theme(qt_app, "dotfiles-outrun")
        qt_app.processEvents()

        assert len(window.theme_buttons) == 5
        assert all(button.height() >= 72 for button in window.theme_buttons)
        assert window.theme_buttons[0].parentWidget().height() >= 80
    finally:
        dispose(window)


def test_prolog_page_is_one_highlighted_editor_plus_fact_list_add_flow(tmp_path):
    window, _, prolog_path, reload_calls = make_window(tmp_path)
    try:
        assert window.prolog_editor.objectName() == "zaraPrologEditor"
        assert window.prolog_highlighter.document() is window.prolog_editor.document()
        assert window.fact_list.objectName() == "zaraFactList"
        assert window.add_fact_button.text() == "Add"
        assert window.edit_fact_button.isEnabled() is False
        assert window.delete_fact_button.isEnabled() is False
        assert window.source_combo.count() == 4
        assert window.source_combo.itemData(0) == "user-config"

        window.add_fact(
            "app_mapping",
            {"name": "studio", "argv": ["code", "--new-window"]},
        )
        assert window.fact_list.count() == 1
        assert window.edit_fact_button.isEnabled() is False
        assert window.delete_fact_button.isEnabled() is False
        window.fact_list.setCurrentRow(0)
        assert window.edit_fact_button.isEnabled() is True
        assert window.delete_fact_button.isEnabled() is True
        assert "studio" in window.fact_list.item(0).text()
        assert 'app_mapping(studio, ["code", "--new-window"]).' in prolog_path.read_text(
            encoding="utf-8"
        )
        assert reload_calls == [True]
    finally:
        dispose(window)


def test_failed_prolog_reload_restores_actual_config(tmp_path):
    window, _, prolog_path, reload_calls = make_window(tmp_path, reload_result=False)
    before = prolog_path.read_bytes()
    try:
        window.add_fact("direct_app", {"name": "wireshark"})
        assert prolog_path.read_bytes() == before
        assert reload_calls == [True]
        assert "could not be reloaded" in window.feedback_label.text()
    finally:
        dispose(window)


def test_config_source_editor_validates_and_saves_actual_toml(tmp_path):
    window, config_path, _, _ = make_window(tmp_path)
    try:
        assert window.config_editor.toPlainText() == config_path.read_text(encoding="utf-8")
        window.config_editor.setPlainText(window.config_editor.toPlainText().replace('theme = "signal-cabin"', 'theme = "nord"'))
        window.save_config_source()
        assert 'theme = "nord"' in config_path.read_text(encoding="utf-8")
        assert window.feedback_label.text() == "config.toml saved. Restart Zara to apply runtime changes."
    finally:
        dispose(window)
