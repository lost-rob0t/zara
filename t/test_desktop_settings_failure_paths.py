from __future__ import annotations

import os
from pathlib import Path
from types import SimpleNamespace

import pytest

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtWidgets import (
    QApplication,
    QCheckBox,
    QComboBox,
    QDialog,
    QDoubleSpinBox,
    QLineEdit,
    QPlainTextEdit,
    QPushButton,
    QSpinBox,
)

from zara.config import DEFAULT_CONFIG_TOML, ZaraConfig
from zara.desktop.preferences import SettingsValidationError
from zara.desktop.prolog_studio import PrologStudioError
from zara.desktop.windows import settings as settings_module
from zara.desktop.windows.settings import FactEditorDialog, SettingsWindow


def app() -> QApplication:
    instance = QApplication.instance()
    assert instance is None or isinstance(instance, QApplication)
    return instance or QApplication([])


def make_window(tmp_path: Path, *, reload_callback=None) -> SettingsWindow:
    app()
    config_path = tmp_path / "xdg" / "config.toml"
    config_path.parent.mkdir(parents=True)
    config_path.write_text(DEFAULT_CONFIG_TOML, encoding="utf-8")
    config_path.with_name("config.pl").write_text("% user config\n", encoding="utf-8")
    root = tmp_path / "repo"
    (root / "kb").mkdir(parents=True)
    (root / "modules").mkdir()
    (root / "main.pl").write_text("main :- true.\n", encoding="utf-8")
    (root / "kb" / "intents.pl").write_text("intent(ok).\n", encoding="utf-8")
    (root / "modules" / "logic.pl").write_text("logic(ok).\n", encoding="utf-8")
    return SettingsWindow(
        ZaraConfig(str(config_path)),
        repo_root=root,
        prolog_reload=reload_callback,
    )


def dispose(window: SettingsWindow) -> None:
    window.prepare_for_quit()
    window.close()
    window.deleteLater()
    app().processEvents()


@pytest.mark.parametrize(
    ("kind", "expected_inputs"),
    [
        ("app_mapping", {"name", "argv"}),
        ("direct_app", {"name"}),
        ("search_engine", {"template"}),
        ("dictation_command", {"argv"}),
        ("timer_sound", {"value"}),
        ("alarm_sound", {"value"}),
        ("llm_provider", {"value"}),
        ("llm_model", {"value"}),
        ("llm_endpoint", {"value"}),
        ("todo_destination", {"value"}),
        ("todo_context_mode", {"value"}),
        ("verb_intent", {"phrase", "intent", "arity"}),
    ],
)
def test_fact_editor_builds_every_supported_fact_shape(kind, expected_inputs):
    app()
    dialog = FactEditorDialog()
    try:
        dialog.kind_combo.setCurrentIndex(dialog.kind_combo.findData(kind))
        assert dialog.fact_kind == kind
        assert set(dialog.inputs) == expected_inputs
    finally:
        dialog.close()
        dialog.deleteLater()
        app().processEvents()


def test_fact_editor_parses_commands_loads_values_and_rejects_bad_quotes():
    app()
    dialog = FactEditorDialog()
    try:
        dialog.kind_combo.setCurrentIndex(dialog.kind_combo.findData("app_mapping"))
        dialog._load_fields({"name": "studio", "argv": ["code", "--new-window"]})
        assert dialog.inputs["name"].text() == "studio"
        assert dialog.inputs["argv"].text() == "code --new-window"
        assert dialog.values() == {"name": "studio", "argv": ["code", "--new-window"]}

        dialog.inputs["argv"].setText("'unterminated")
        with pytest.raises(PrologStudioError, match="command could not be parsed"):
            dialog.values()

        dialog.kind_combo.setCurrentIndex(dialog.kind_combo.findData("llm_provider"))
        dialog._load_fields({"value": "anthropic"})
        assert dialog.inputs["value"].currentData() == "anthropic"
        assert dialog.values() == {"value": "anthropic"}
    finally:
        dialog.close()
        dialog.deleteLater()
        app().processEvents()


def test_setting_value_supports_every_real_control_and_rejects_unknown(tmp_path):
    window = make_window(tmp_path)
    try:
        line = QLineEdit("value")
        plain = QPlainTextEdit("body")
        combo = QComboBox()
        combo.addItem("One", "one")
        check = QCheckBox()
        check.setChecked(True)
        spin = QSpinBox()
        spin.setValue(7)
        double = QDoubleSpinBox()
        double.setValue(1.25)

        assert window._setting_value(line) == "value"
        assert window._setting_value(plain) == "body"
        assert window._setting_value(combo) == "one"
        assert window._setting_value(check) is True
        assert window._setting_value(spin) == 7
        assert window._setting_value(double) == 1.25
        with pytest.raises(SettingsValidationError, match="unsupported settings control"):
            window._setting_value(QPushButton("nope"))
    finally:
        dispose(window)


def test_pairing_ui_fails_closed_then_reports_progress_and_success(tmp_path, monkeypatch):
    window = make_window(tmp_path)
    try:
        window.start_desktop_pairing()
        assert "Paste the zara://pair/v1 URI" in window.pairing_status.text()

        window._on_pairing_progress("ABCD", "desktop-1")
        assert "ABCD" in window.pairing_status.text()
        assert "desktop-1" in window.pairing_status.text()

        window._on_pairing_finished("", "denied")
        assert window.pairing_button.isEnabled() is True
        assert window.pairing_uri_input.isEnabled() is True
        assert window.pairing_status.text() == "Pairing failed: denied"

        refreshed = []
        monkeypatch.setattr(window, "_refresh_pairing_status", lambda: refreshed.append(True))
        window._on_pairing_finished("ipc:///tmp/zara.sock", "")
        assert refreshed == [True]
        assert "ipc:///tmp/zara.sock" in window.feedback_label.text()
    finally:
        dispose(window)


def test_pairing_worker_propagates_success_and_typed_failure(tmp_path, monkeypatch):
    window = make_window(tmp_path)
    events = []
    window.pairing_progress_observed.connect(lambda code, device: events.append(("progress", code, device)))
    window.pairing_finished.connect(lambda endpoint, error: events.append(("done", endpoint, error)))

    class InlineThread:
        def __init__(self, *, target, **_kwargs):
            self.target = target

        def start(self):
            self.target()

    try:
        monkeypatch.setattr(settings_module.threading, "Thread", InlineThread)

        def success(_uri, *, config, on_progress):
            assert config is window.config
            on_progress(SimpleNamespace(verification_code="7K3P", device_id="desktop-7"))
            return SimpleNamespace(endpoint="ipc:///tmp/zara.sock")

        monkeypatch.setattr(settings_module, "pair_client", success)
        window.pairing_uri_input.setText("zara://pair/v1?token=ok")
        window.start_desktop_pairing()
        assert ("progress", "7K3P", "desktop-7") in events
        assert ("done", "ipc:///tmp/zara.sock", "") in events

        def failure(*_args, **_kwargs):
            raise ValueError("bad pairing uri")

        events.clear()
        monkeypatch.setattr(settings_module, "pair_client", failure)
        window.pairing_uri_input.setText("zara://pair/v1?token=bad")
        window.start_desktop_pairing()
        assert events == [("done", "", "bad pairing uri")]
    finally:
        dispose(window)


def test_pairing_status_handles_store_failure_absent_and_ready_profiles(tmp_path, monkeypatch):
    window = make_window(tmp_path)

    class Store:
        def __init__(self, result=None, error=None):
            self.result = result
            self.error = error

        def ready_profile(self):
            if self.error is not None:
                raise self.error
            return self.result

    try:
        monkeypatch.setattr(
            settings_module.ClientEnrollmentStore,
            "for_config",
            lambda _config: Store(error=settings_module.ClientEnrollmentError("broken")),
        )
        window._refresh_pairing_status()
        assert window.pairing_status.text() == "Pairing state error: broken"

        monkeypatch.setattr(settings_module.ClientEnrollmentStore, "for_config", lambda _config: Store())
        window._refresh_pairing_status()
        assert window.pairing_status.text().startswith("Not paired.")

        profile = SimpleNamespace(
            endpoint="tcp://127.0.0.1:5555",
            server_public_key="12345678abcdefghijkl87654321",
        )
        monkeypatch.setattr(
            settings_module.ClientEnrollmentStore,
            "for_config",
            lambda _config: Store(result=profile),
        )
        window._refresh_pairing_status()
        assert "tcp://127.0.0.1:5555" in window.pairing_status.text()
        assert "12345678…87654321" in window.pairing_status.text()
    finally:
        dispose(window)


def test_settings_and_config_save_failures_are_user_visible(tmp_path, monkeypatch):
    window = make_window(tmp_path)
    try:
        def reject_update(_values):
            raise SettingsValidationError("invalid settings")

        monkeypatch.setattr(window.document, "update", reject_update)
        window.save_settings()
        assert window.feedback_label.text() == "Settings were not saved: invalid settings"

        def reject_source(_text):
            raise SettingsValidationError("invalid toml")

        monkeypatch.setattr(window.document, "replace_source", reject_source)
        window.save_config_source()
        assert window.feedback_label.text() == "config.toml was not saved: invalid toml"
    finally:
        dispose(window)


def test_prolog_source_failure_paths_restore_and_fail_closed(tmp_path, monkeypatch):
    reload_calls = []

    def reload_false():
        reload_calls.append(True)
        return False

    window = make_window(tmp_path, reload_callback=reload_false)
    try:
        window.source_combo.setCurrentIndex(window.source_combo.findData("user-config"))
        source_path = window.user_prolog_config
        before = source_path.read_bytes()
        window.prolog_editor.setPlainText("changed_fact(ok).\n")
        window.save_prolog_source()
        assert source_path.read_bytes() == before
        assert reload_calls == [True]
        assert "restored" in window.feedback_label.text()

        monkeypatch.setattr(window.source_repository, "list", lambda: [])
        window._load_selected_source()
        assert window.prolog_editor.isReadOnly() is True
        assert window.save_prolog_button.isEnabled() is False
        window.save_prolog_source()
        assert "source is not approved" in window.feedback_label.text()
    finally:
        dispose(window)


def test_reload_exception_fact_summaries_and_atomic_restore(tmp_path):
    def explode():
        raise RuntimeError("reload exploded")

    window = make_window(tmp_path, reload_callback=explode)
    try:
        assert window._reload_prolog() is False
        assert "reload exploded" in window.feedback_label.text()

        assert window._fact_summary(
            SimpleNamespace(kind="app_mapping", fields={"name": "studio", "argv": ["code", "--new-window"]})
        ) == "App · studio → code --new-window"
        assert window._fact_summary(
            SimpleNamespace(kind="verb_intent", fields={"phrase": "summon studio", "intent": "open"})
        ) == "Intent · summon studio → open"
        assert window._fact_summary(
            SimpleNamespace(kind="direct_app", fields={"name": "wireshark"})
        ) == "Direct app · wireshark"
        assert window._fact_summary(
            SimpleNamespace(kind="search_engine", fields={"template": "https://example.test/?q=%s"})
        ) == "Search engine · https://example.test/?q=%s"

        path = tmp_path / "restore-target"
        path.write_bytes(b"old")
        path.chmod(0o640)
        window._restore_bytes(path, b"new")
        assert path.read_bytes() == b"new"
        assert path.stat().st_mode & 0o777 == 0o640
    finally:
        dispose(window)


def test_fact_add_edit_delete_rejected_dialog_and_no_selection_paths(tmp_path, monkeypatch):
    window = make_window(tmp_path)

    class RejectDialog:
        def __init__(self, *_args, **_kwargs):
            self.fact_kind = "direct_app"

        def exec(self):
            return QDialog.DialogCode.Rejected

        def values(self):
            raise AssertionError("rejected dialog must not read values")

    try:
        monkeypatch.setattr(settings_module, "FactEditorDialog", RejectDialog)
        window._open_add_fact()
        assert window.fact_list.count() == 0

        window._open_edit_fact()
        assert window.feedback_label.text() == "Select a fact to edit."
        window.delete_selected_fact()
        assert window.feedback_label.text() == "Select a fact to delete."

        fact = window.add_fact("direct_app", {"name": "wireshark"})
        assert fact is not None
        assert window.fact_list.count() == 1
        window.fact_list.setCurrentRow(0)
        assert window._selected_fact() is not None
        window.delete_selected_fact()
        assert window.fact_list.count() == 0
        assert "Fact deleted" in window.feedback_label.text()
    finally:
        dispose(window)


def test_fact_dialog_invalid_values_are_reported_without_mutation(tmp_path, monkeypatch):
    window = make_window(tmp_path)

    class InvalidDialog:
        def __init__(self, *_args, **_kwargs):
            self.fact_kind = "app_mapping"

        def exec(self):
            return QDialog.DialogCode.Accepted

        def values(self):
            raise PrologStudioError("bad command")

    try:
        monkeypatch.setattr(settings_module, "FactEditorDialog", InvalidDialog)
        before = window.user_prolog_config.read_bytes()
        window._open_add_fact()
        assert window.user_prolog_config.read_bytes() == before
        assert window.feedback_label.text() == "Fact was not added: bad command"
    finally:
        dispose(window)


def test_source_selection_read_only_state_and_window_close_policy(tmp_path, monkeypatch):
    window = make_window(tmp_path)
    try:
        source_id = "main.pl"
        source_path = window.repo_root / source_id
        read_only = SimpleNamespace(
            id=source_id,
            label=source_id,
            path=source_path,
            writable=False,
        )
        monkeypatch.setattr(window.source_repository, "list", lambda: [read_only])
        monkeypatch.setattr(window.source_repository, "read", lambda _source_id: "main :- true.\n")
        index = window.source_combo.findData(source_id)
        assert index >= 0
        window.source_combo.setCurrentIndex(index)
        window._load_selected_source()
        assert window.prolog_editor.isReadOnly() is True
        assert window.save_prolog_button.isEnabled() is False
        assert "read-only" in window.source_status.text()

        window.show()
        app().processEvents()
        window.close()
        app().processEvents()
        assert window.isVisible() is False

        window.show_raised()
        app().processEvents()
        assert window.isVisible() is True
    finally:
        dispose(window)
