from __future__ import annotations

import os
import concurrent.futures
import threading

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtCore import Qt
from PySide6.QtWidgets import QApplication, QScrollArea

from zara.config import DEFAULT_CONFIG_TOML, ZaraConfig
from zara.desktop.theme import apply_desktop_theme
from zara.desktop.windows import SettingsWindow
from zara.runtime.commands import PrologQueryReceipt


def app() -> QApplication:
    instance = QApplication.instance()
    assert instance is None or isinstance(instance, QApplication)
    return instance or QApplication([])


def make_window(tmp_path, *, reload_result=True, prolog_query=None):
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
        prolog_query=prolog_query,
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
            "Connections",
            "Voice & Speech",
            "Tools & Privacy",
            "Prolog IDE",
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
            "outrun",
            "starintel",
            "midnight",
            "terminal",
            "light",
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

        assert len(window.theme_buttons) == 10
        assert all(button.height() >= 72 for button in window.theme_buttons)
        assert window.theme_buttons[0].parentWidget().height() >= 80
        theme_strip = window.findChild(QScrollArea, "zaraThemeStrip")
        assert theme_strip is not None
        assert theme_strip.horizontalScrollBarPolicy() == Qt.ScrollBarPolicy.ScrollBarAsNeeded
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
        assert window.prolog_cursor_status.text() == "Ln 1, Col 1"
        assert window.prolog_dirty_status.text() == "Saved"
        assert window.prolog_query_input.placeholderText() == "?- goal"
        assert window.prolog_query_button.text() == "Run"

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


def test_prolog_ide_tracks_dirty_state_and_supports_find_replace(tmp_path):
    window, _, _, _ = make_window(tmp_path)
    try:
        window.prolog_editor.setPlainText("color(red).\ncolor(blue).\n")
        assert window.prolog_dirty_status.text() == "Modified"

        window.prolog_find_input.setText("color")
        window.prolog_replace_input.setText("tone")
        window.replace_all_prolog_matches()

        assert window.prolog_editor.toPlainText() == "tone(red).\ntone(blue).\n"
        assert window.prolog_find_status.text() == "2 replacements"
    finally:
        dispose(window)


def test_prolog_ide_runs_query_off_qt_thread_and_records_history(tmp_path):
    caller_thread = threading.get_ident()
    called = []

    def query(goal, max_solutions):
        called.append((threading.get_ident(), goal, max_solutions))
        future = concurrent.futures.Future()
        future.set_result(
            PrologQueryReceipt(
                request_id="query-1",
                solutions=({"X": "red"}, {"X": "blue"}),
            )
        )
        return future

    window, _, _, _ = make_window(tmp_path, prolog_query=query)
    try:
        window.prolog_query_input.setText("color(X)")
        window.run_prolog_query()
        app().processEvents()

        assert called == [(caller_thread, "color(X)", 50)]
        assert "X = red" in window.prolog_results.toPlainText()
        assert "X = blue" in window.prolog_results.toPlainText()
        assert window.prolog_query_history.itemText(1) == "color(X)"
        assert window.prolog_query_status.text() == "2 solutions"
    finally:
        dispose(window)


def test_prolog_ide_cancel_discards_late_query_result(tmp_path):
    future = concurrent.futures.Future()
    window, _, _, _ = make_window(tmp_path, prolog_query=lambda _goal, _limit: future)
    try:
        window.prolog_query_input.setText("slow(X)")
        window.run_prolog_query()
        window.cancel_prolog_query()
        future.set_result(
            PrologQueryReceipt(request_id="query-1", solutions=({"X": "stale"},))
        )
        app().processEvents()

        assert "stale" not in window.prolog_results.toPlainText()
        assert window.prolog_query_status.text() == "Cancelled · late results will be discarded"
    finally:
        dispose(window)


def test_prolog_ide_reports_syntax_diagnostic_without_replacing_source(tmp_path):
    window, _, prolog_path, _ = make_window(tmp_path)
    before = prolog_path.read_text(encoding="utf-8")
    try:
        window.prolog_editor.setPlainText("valid.\nbroken(\n")
        window.save_prolog_source()

        assert prolog_path.read_text(encoding="utf-8") == before
        assert window.prolog_diagnostics.count() == 1
        diagnostic = window.prolog_diagnostics.item(0)
        assert "syntax" in diagnostic.text().lower()
        window._open_prolog_diagnostic(diagnostic)
        assert window.prolog_editor.textCursor().blockNumber() == 1
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
        window.config_editor.setPlainText(
            window.config_editor.toPlainText().replace('theme = "outrun"', 'theme = "nord"')
        )
        window.save_config_source()
        assert 'theme = "nord"' in config_path.read_text(encoding="utf-8")
        assert window.feedback_label.text() == "config.toml saved. Restart Zara to apply runtime changes."
    finally:
        dispose(window)



def test_connections_page_exposes_pairing_flow_without_raw_key_fields(tmp_path):
    window, _, _, _ = make_window(tmp_path)
    try:
        assert window.pairing_button.text() == "Pair this desktop"
        assert window.pairing_uri_input.placeholderText().startswith("zara://pair/v1")
        assert "Not paired" in window.pairing_status.text()
        source = __import__("pathlib").Path(
            __import__("zara.desktop.windows.settings", fromlist=["__file__"]).__file__
        ).read_text(encoding="utf-8")
        assert "pair_client(" in source
        assert "curve_secret_key" not in source
    finally:
        dispose(window)
