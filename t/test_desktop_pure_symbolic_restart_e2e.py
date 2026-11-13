from __future__ import annotations

import os
import queue
import socket
import time

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtCore import QObject, Signal
from PySide6.QtWidgets import QApplication

from zara.database import DatabaseManager
from zara.desktop import app as desktop_app
from zara.desktop.controller import DesktopController
from zara.desktop.conversation import ConversationService, ConversationStore, MessageRole
from zara.desktop.qt_bridge import QtRuntimeBridge
from zara.runtime import events


PROVIDER_ENV = (
    "ANTHROPIC_API_KEY",
    "AZURE_OPENAI_API_KEY",
    "AZURE_OPENAI_ENDPOINT",
    "COHERE_API_KEY",
    "DEEPSEEK_API_KEY",
    "GEMINI_API_KEY",
    "GOOGLE_API_KEY",
    "GROQ_API_KEY",
    "HF_TOKEN",
    "HUGGINGFACEHUB_API_TOKEN",
    "MISTRAL_API_KEY",
    "OLLAMA_HOST",
    "OPENAI_API_KEY",
    "OPENAI_BASE_URL",
    "OPENROUTER_API_KEY",
    "TOGETHER_API_KEY",
    "ZAI_API_KEY",
)


class PureSymbolicConfig:
    def get(self, section: str, key: str, default=None):
        if section == "conversation" and key == "execution_policy":
            return "pure_symbolic"
        if section == "agent" and key == "backend":
            return "langgraph"
        return default

    def get_api_service_config(self):
        return {"enabled": False}

    def get_tasks_config(self):
        return {"enabled": False}

    def get_latency_config(self):
        return {"enabled": False}

    def get_module_search_paths(self):
        return []

    def get_plugin_runtime_config(self):
        return {
            "lifecycle_timeout": 0.2,
            "event_queue_size": 4,
            "max_managed_workers": 1,
        }

    def get_plugin_config(self, _name: str):
        return {}


class FakeAction:
    def setEnabled(self, _enabled: bool) -> None:  # noqa: N802 - Qt API
        return None


class FakeTray(QObject):
    toggle_requested = Signal()
    quick_requested = Signal()
    full_chat_requested = Signal()
    settings_requested = Signal()
    restart_requested = Signal()
    diagnostics_requested = Signal()
    quit_requested = Signal()

    def __init__(self) -> None:
        super().__init__()
        self.quit_action = FakeAction()

    def show_if_available(self) -> bool:
        return True

    def set_status(self, _status) -> None:
        return None

    def hide(self) -> None:
        return None


def _app() -> QApplication:
    instance = QApplication.instance()
    assert instance is None or isinstance(instance, QApplication)
    app = instance or QApplication([])
    app.setQuitOnLastWindowClosed(False)
    return app


def _wait_until(
    app: QApplication,
    predicate,
    timeout: float = 8.0,
    *,
    failure_detail=None,
) -> None:
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        app.processEvents()
        if predicate():
            return
        time.sleep(0.01)
    app.processEvents()
    if predicate():
        return
    detail = ""
    if failure_detail is not None:
        detail = f": {failure_detail()}"
    raise AssertionError(
        f"timed out waiting for Desktop pure-symbolic restart state{detail}"
    )


def _runtime_failures(diagnostics) -> list[str]:
    observed_failures: list[str] = []
    while True:
        try:
            envelope = diagnostics.get(timeout=0.01)
        except queue.Empty:
            break
        event = envelope.event
        if isinstance(event, events.RuntimeError):
            observed_failures.append(f"RuntimeError: {event.reason}")
        elif isinstance(event, events.AgentFailed):
            observed_failures.append(f"AgentFailed: {event.reason}")
    return observed_failures


def _assert_no_runtime_failures(diagnostics) -> None:
    assert _runtime_failures(diagnostics) == []


def _conversation_failure_detail(
    service: ConversationService,
    conversation_id: str,
    diagnostics,
) -> str:
    state = service.get_state(conversation_id)
    transcript = [
        {
            "role": message.role.value,
            "status": message.status.value,
            "content": message.content,
            "error": message.error,
            "turn_id": message.turn_id,
        }
        for message in state.messages
    ]
    failures = _runtime_failures(diagnostics)
    return (
        f"active_turn_id={state.active_turn_id!r}; "
        f"transcript={transcript!r}; runtime_failures={failures!r}"
    )


def _close_surface(
    app: QApplication,
    client,
    bridge: QtRuntimeBridge,
    controller: DesktopController,
) -> None:
    surface = controller.window
    client.close(timeout=5.0)
    bridge.close()
    surface.prepare_for_quit()
    surface.close()
    # Drain zero-delay layout callbacks while their Qt wrappers are still valid.
    # Reusing the same QApplication to emulate a restarted process otherwise lets
    # the old surface's queued _MessageBody resize callback fire after deletion.
    app.processEvents()
    app.processEvents()
    surface.deleteLater()
    controller.setParent(None)
    controller.deleteLater()
    app.processEvents()


def _network_fence(monkeypatch) -> None:
    real_socket = socket.socket

    def fenced_socket(family=socket.AF_INET, *args, **kwargs):
        if family in (socket.AF_INET, socket.AF_INET6):
            raise AssertionError("pure-symbolic Desktop attempted provider/network fallback")
        return real_socket(family, *args, **kwargs)

    def fenced_connection(*_args, **_kwargs):
        raise AssertionError("pure-symbolic Desktop attempted a network connection")

    monkeypatch.setattr(socket, "socket", fenced_socket)
    monkeypatch.setattr(socket, "create_connection", fenced_connection)


def test_real_desktop_surface_reopens_durable_symbolic_clarification_without_model_fallback(
    tmp_path,
    monkeypatch,
):
    for key in PROVIDER_ENV:
        monkeypatch.delenv(key, raising=False)
        assert key not in os.environ
    _network_fence(monkeypatch)

    qt_app = _app()
    config = PureSymbolicConfig()
    database_path = tmp_path / "desktop-symbolic-restart.db"

    first_database = DatabaseManager(database_path)
    first_store = ConversationStore(first_database)
    first_service = ConversationService(first_store)
    first_client = desktop_app._default_desktop_client(  # type: ignore[arg-type]
        config,
        conversation_store=first_store,
    )
    first_diagnostics = first_client.subscribe(maxsize=256)
    first_bridge = QtRuntimeBridge(first_client, parent=qt_app, auto_start_timer=True)
    first_controller = DesktopController(
        qt_app,
        first_client,
        first_bridge,
        tray_factory=FakeTray,
        conversation_service=first_service,
    )
    first_surface = first_controller.window

    conversation_id = ""
    first_closed = False
    second_closed = False
    second_database = None
    second_client = None
    second_bridge = None
    second_controller = None

    try:
        first_controller.start().result(timeout=8.0)
        _wait_until(qt_app, lambda: first_controller.status.detail == "Zara is ready")

        conversation_id = first_surface.current_conversation_id
        first_surface.composer.setPlainText("timer")
        first_surface.submit_current_text()

        _wait_until(
            qt_app,
            lambda: any(
                message.role is MessageRole.ASSISTANT
                and message.content == "How long should I set the timer for?"
                for message in first_service.get_state(conversation_id).messages
            ),
        )

        before_restart = first_store.load_symbolic_projection(conversation_id)
        assert before_restart is not None
        before_restart.assert_pure_symbolic()
        assert before_restart.dialogue_act == "clarify"
        assert "partial_frame" in before_restart.dialogue_state["prolog_context_term"]
        assert before_restart.providers_enabled is False
        assert before_restart.max_model_calls == 0
        assert before_restart.provider_calls == 0
        assert before_restart.model_calls == 0
        _assert_no_runtime_failures(first_diagnostics)

        _close_surface(qt_app, first_client, first_bridge, first_controller)
        first_closed = True
        first_database.close()

        second_database = DatabaseManager(database_path)
        second_store = ConversationStore(second_database)
        second_service = ConversationService(second_store)
        second_client = desktop_app._default_desktop_client(  # type: ignore[arg-type]
            config,
            conversation_store=second_store,
        )
        second_diagnostics = second_client.subscribe(maxsize=256)
        second_bridge = QtRuntimeBridge(
            second_client,
            parent=qt_app,
            auto_start_timer=True,
        )
        second_controller = DesktopController(
            qt_app,
            second_client,
            second_bridge,
            tray_factory=FakeTray,
            conversation_service=second_service,
        )
        second_surface = second_controller.window

        second_controller.start().result(timeout=8.0)
        _wait_until(qt_app, lambda: second_controller.status.detail == "Zara is ready")
        second_controller.open_full_chat(conversation_id)
        qt_app.processEvents()

        assert second_surface.current_conversation_id == conversation_id
        assert [
            message.content
            for message in second_service.get_state(conversation_id).messages
        ] == [
            "timer",
            "How long should I set the timer for?",
        ]

        recovered = second_store.load_symbolic_projection(conversation_id)
        assert recovered is not None
        recovered.assert_pure_symbolic()
        assert recovered.projection_generation == before_restart.projection_generation
        assert recovered.runtime_generation == before_restart.runtime_generation
        assert recovered.dialogue_state == before_restart.dialogue_state

        second_surface.composer.setPlainText("5 minutes")
        second_surface.submit_current_text()

        submitted = second_service.get_state(conversation_id)
        assert [message.content for message in submitted.messages] == [
            "timer",
            "How long should I set the timer for?",
            "5 minutes",
        ]

        expected = (
            "That action needs capability-checked execution before I can report success."
        )
        _wait_until(
            qt_app,
            lambda: any(
                message.role is MessageRole.ASSISTANT and message.content == expected
                for message in second_service.get_state(conversation_id).messages
            ),
            failure_detail=lambda: _conversation_failure_detail(
                second_service,
                conversation_id,
                second_diagnostics,
            ),
        )

        state = second_service.get_state(conversation_id)
        assert [message.content for message in state.messages] == [
            "timer",
            "How long should I set the timer for?",
            "5 minutes",
            expected,
        ]
        assert state.active_turn_id is None

        after_restart = second_store.load_symbolic_projection(conversation_id)
        assert after_restart is not None
        after_restart.assert_pure_symbolic()
        assert after_restart.projection_generation == before_restart.projection_generation + 1
        assert after_restart.runtime_generation == before_restart.runtime_generation + 1
        assert after_restart.dialogue_act == "dispatch_required"
        assert "completed_frame" in after_restart.dialogue_state["prolog_context_term"]
        assert after_restart.providers_enabled is False
        assert after_restart.max_model_calls == 0
        assert after_restart.provider_calls == 0
        assert after_restart.model_calls == 0
        _assert_no_runtime_failures(second_diagnostics)
    finally:
        if second_controller is not None and not second_closed:
            assert second_client is not None
            assert second_bridge is not None
            _close_surface(qt_app, second_client, second_bridge, second_controller)
            second_closed = True
        if second_database is not None:
            second_database.close()
        if not first_closed:
            _close_surface(qt_app, first_client, first_bridge, first_controller)
            first_database.close()
