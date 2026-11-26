"""Inspector drawer (the Prolog ADE) behaviors for the adaptive Copilot.

Gates the calm-console council decision C1-C10: renderer identity across
toggle, off-Qt-thread queries with the KB cap, cancel/restart fences,
allowlisted facts browsing, zero-command live state, honest no-runtime state,
and per-presentation visibility persistence.
"""

from __future__ import annotations

import concurrent.futures
import os
import threading

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtCore import QObject, QSettings, Signal
from PySide6.QtWidgets import QApplication

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationService, ConversationStore
from zara.desktop.conversation.symbolic_projection import SymbolicConversationProjection
from zara.desktop.inspector import MAX_QUERY_BYTES, UI_REQUESTED_SOLUTIONS
from zara.desktop.state import DesktopRuntimeState, DesktopStatus
from zara.desktop.windows import CopilotPresentation, CopilotWindow
from zara.runtime.commands import PrologQueryReceipt


def app() -> QApplication:
    instance = QApplication.instance()
    assert instance is None or isinstance(instance, QApplication)
    result = instance or QApplication([])
    result.setQuitOnLastWindowClosed(False)
    return result


class FakeBridge(QObject):
    runtime_event = Signal(object)
    command_completed = Signal(object)
    command_failed = Signal(str, str)

    def __init__(self) -> None:
        super().__init__()
        self.commands: list[object] = []

    def submit(self, command):
        self.commands.append(command)
        future: concurrent.futures.Future = concurrent.futures.Future()
        future.set_result(None)
        return future


def make_window(
    tmp_path,
    *,
    prolog_query=None,
    settings: QSettings | None = None,
):
    qt_app = app()
    bridge = FakeBridge()
    service = ConversationService(
        ConversationStore(DatabaseManager(tmp_path / "inspector.db"))
    )
    if settings is None:
        settings = QSettings(str(tmp_path / "inspector.ini"), QSettings.IniFormat)
    window = CopilotWindow(
        bridge,
        service,
        settings=settings,  # type: ignore[arg-type]
        prolog_query=prolog_query,
    )
    qt_app.processEvents()
    return qt_app, bridge, service, window, settings


def dispose(window: CopilotWindow) -> None:
    window.prepare_for_quit()
    window.close()
    window.deleteLater()
    app().processEvents()


def _receipt(solutions: tuple[dict, ...]) -> concurrent.futures.Future:
    future: concurrent.futures.Future = concurrent.futures.Future()
    future.set_result(PrologQueryReceipt(request_id="query-1", solutions=solutions))
    return future


def test_inspector_constants_match_kb_limits():
    assert UI_REQUESTED_SOLUTIONS == 50
    assert MAX_QUERY_BYTES == 4096


def test_inspector_toggle_preserves_renderer_identity_in_both_presentations(tmp_path):
    qt_app, bridge, service, window, _ = make_window(tmp_path)
    try:
        window.show()
        qt_app.processEvents()
        conversation_id = window.current_conversation_id
        service.add_user_message(
            conversation_id,
            "draft message",
            request_id="inspector-seed-message",
        )
        window.sync_from_shared_state()
        qt_app.processEvents()
        widgets_before = dict(window.message_widgets)
        window.composer.setPlainText("composer draft")

        for presentation in (
            CopilotPresentation.EXPANDED,
            CopilotPresentation.COMPACT,
        ):
            window.set_presentation(presentation)
            assert not window.inspector.isVisible()
            window.toggle_inspector()
            qt_app.processEvents()
            assert window.inspector.isVisible()
            assert window.current_conversation_id == conversation_id
            assert dict(window.message_widgets) == widgets_before
            assert window.composer.toPlainText() == "composer draft"
            window.toggle_inspector()
            qt_app.processEvents()
            assert not window.inspector.isVisible()
    finally:
        dispose(window)


def test_inspector_header_button_and_shortcut_summon_drawer(tmp_path):
    qt_app, bridge, service, window, _ = make_window(tmp_path)
    try:
        window.show()
        qt_app.processEvents()
        assert window.inspector_button.parent() is not None
        window.inspector_button.click()
        qt_app.processEvents()
        assert window.inspector.isVisible()
        window.inspector_button.click()
        qt_app.processEvents()
        assert not window.inspector.isVisible()
    finally:
        dispose(window)


def test_inspector_query_runs_off_qt_thread_with_kb_cap_and_history(tmp_path):
    caller_thread = threading.get_ident()
    called: list[tuple[int, str, int]] = []

    def query(goal, max_solutions):
        called.append((threading.get_ident(), goal, max_solutions))
        return _receipt(({"X": "red"}, {"X": "blue"}))

    qt_app, bridge, service, window, _ = make_window(tmp_path, prolog_query=query)
    try:
        window.inspector.query_input.setText("color(X)")
        window.inspector.run_query()
        qt_app.processEvents()

        assert called == [(caller_thread, "color(X)", 50)]
        assert "X = red" in window.inspector.results.toPlainText()
        assert window.inspector.query_status.text() == "2 solutions"
        assert window.inspector.query_history.itemText(1) == "color(X)"

        for index in range(25):
            window.inspector.query_input.setText(f"goal_{index}(X)")
            window.inspector.run_query()
            qt_app.processEvents()
        # 1 header item + at most 20 history entries (KB query_history limit)
        assert window.inspector.query_history.count() <= 21
    finally:
        dispose(window)


def test_inspector_cancel_discards_late_query_result(tmp_path):
    future = concurrent.futures.Future()
    qt_app, bridge, service, window, _ = make_window(
        tmp_path, prolog_query=lambda _goal, _limit: future
    )
    try:
        window.inspector.query_input.setText("slow(X)")
        window.inspector.run_query()
        window.inspector.cancel_query()
        future.set_result(
            PrologQueryReceipt(request_id="query-1", solutions=({"X": "stale"},))
        )
        qt_app.processEvents()

        assert "stale" not in window.inspector.results.toPlainText()
        assert (
            window.inspector.query_status.text()
            == "Cancelled · late results will be discarded"
        )
    finally:
        dispose(window)


def test_inspector_rejects_oversized_goal_before_submit(tmp_path):
    called: list[tuple[str, int]] = []

    def query(goal, max_solutions):
        called.append((goal, max_solutions))
        return _receipt(({"X": "late"},))

    qt_app, bridge, service, window, _ = make_window(tmp_path, prolog_query=query)
    try:
        window.inspector.query_input.setText("x" * (MAX_QUERY_BYTES + 1))
        window.inspector.run_query()
        qt_app.processEvents()

        assert called == []
        assert "4096" in window.inspector.query_status.text()
        assert "late" not in window.inspector.results.toPlainText()
    finally:
        dispose(window)


def test_inspector_facts_browser_issues_only_allowlisted_goals(tmp_path):
    submitted: list[tuple[str, int]] = []

    def query(goal, max_solutions):
        submitted.append((goal, max_solutions))
        return _receipt(({"Module": "kb_intents", "Count": "3"},))

    qt_app, bridge, service, window, _ = make_window(tmp_path, prolog_query=query)
    try:
        window.inspector.refresh_engine_facts()
        qt_app.processEvents()

        assert submitted, "facts browser must query live projections"
        for goal, max_solutions in submitted:
            assert goal.startswith("desktop_ade:")
            assert max_solutions == UI_REQUESTED_SOLUTIONS
    finally:
        dispose(window)


def test_inspector_live_state_renders_projection_without_runtime_commands(tmp_path):
    qt_app, bridge, service, window, _ = make_window(tmp_path)
    try:
        conversation_id = window.current_conversation_id
        window.conversations.store.save_symbolic_projection(
            SymbolicConversationProjection(
                conversation_id=conversation_id,
                projection_generation=1,
                runtime_generation=1,
                turn_id="turn-inspector-1",
                outcome="success",
                renderer_provenance="symbolic-dcg/v1",
                dialogue_state={
                    "prolog_context_term": "[frame(todo.add, missing([when]))]",
                    "response_act_term": "answer",
                },
                unresolved_questions=[{"id": "q1", "question": "When?"}],
                discourse_entities=[{"name": "standup"}],
            ),
            expected_generation=0,
        )

        window.bind_conversation(conversation_id)
        window.set_presentation(CopilotPresentation.EXPANDED)
        window.toggle_inspector()
        qt_app.processEvents()

        rendered = window.inspector.live_state.toPlainText()
        assert "todo.add" in rendered
        assert "When?" in rendered
        assert bridge.commands == []
    finally:
        dispose(window)


def test_inspector_without_runtime_shows_honest_state(tmp_path):
    qt_app, bridge, service, window, _ = make_window(tmp_path, prolog_query=None)
    try:
        assert window.inspector.query_status.text() == "Runtime query unavailable"
        window.inspector.query_input.setText("color(X)")
        window.inspector.run_query()
        qt_app.processEvents()
        assert window.inspector.query_status.text() == "Runtime query unavailable"
        assert window.inspector.results.toPlainText() == ""
    finally:
        dispose(window)


def test_inspector_restart_fence_discards_inflight_results(tmp_path):
    future = concurrent.futures.Future()
    qt_app, bridge, service, window, _ = make_window(
        tmp_path, prolog_query=lambda _goal, _limit: future
    )
    try:
        window.inspector.query_input.setText("slow(X)")
        window.inspector.run_query()
        window.set_status(
            DesktopStatus(
                state=DesktopRuntimeState.ERROR,
                detail="daemon lost",
            )
        )
        qt_app.processEvents()
        future.set_result(
            PrologQueryReceipt(request_id="query-1", solutions=({"X": "stale"},))
        )
        qt_app.processEvents()

        assert "stale" not in window.inspector.results.toPlainText()
        assert "Runtime restarting" in window.inspector.query_status.text()
    finally:
        dispose(window)


def test_inspector_visibility_persists_per_presentation(tmp_path):
    settings = QSettings(str(tmp_path / "persist.ini"), QSettings.IniFormat)
    qt_app, bridge, service, window, _ = make_window(tmp_path, settings=settings)
    window.set_presentation(CopilotPresentation.EXPANDED)
    window.show()
    qt_app.processEvents()
    window.toggle_inspector()
    qt_app.processEvents()
    dispose(window)

    reopened = CopilotWindow(
        bridge,
        service,
        settings=settings,
    )
    try:
        reopened.set_presentation(CopilotPresentation.EXPANDED)
        reopened.show()
        qt_app.processEvents()
        assert reopened.inspector.isVisible()
    finally:
        dispose(reopened)
