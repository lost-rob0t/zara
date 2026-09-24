from __future__ import annotations

import concurrent.futures
import os
from types import SimpleNamespace

import pytest

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtCore import QObject, Signal
from PySide6.QtWidgets import QApplication

from zara import daemon_client
from zara.client import InProcessZaraClient, ZaraClient, ZaraClientState
from zara.desktop import control
from zara.desktop.controller import DesktopController
from zara.desktop.conversation.replay_status import (
    _normalize_conversation_id,
    conversation_symbolic_status,
    main as replay_main,
    symbolic_projection_payload,
)
from zara.desktop.conversation.symbolic_projection import SymbolicConversationProjection
from zara.desktop.conversation.symbolic_runtime import (
    PureSymbolicProjectionAdapter,
    _bounded_expert_evidence_ref,
    _dialogue_context_matches_project,
)
from zara.desktop.state import DesktopRuntimeState, DesktopStatus, INITIAL_STATUS
from zara.runtime import events
from zara.runtime.commands import CommandReceipt
from zara.runtime.host import RuntimeHostState, RuntimeNotReady


def qt_app() -> QApplication:
    instance = QApplication.instance()
    assert instance is None or isinstance(instance, QApplication)
    app = instance or QApplication([])
    app.setQuitOnLastWindowClosed(False)
    return app


class _SignalTray(QObject):
    toggle_requested = Signal()
    settings_requested = Signal()
    restart_requested = Signal()
    diagnostics_requested = Signal()
    quit_requested = Signal()

    def __init__(self) -> None:
        super().__init__()
        self.quit_action = SimpleNamespace(setEnabled=lambda _enabled: None)
        self.statuses: list[DesktopStatus] = []
        self.hidden = False

    def show_if_available(self) -> bool:
        return True

    def set_status(self, status: DesktopStatus) -> None:
        self.statuses.append(status)

    def hide(self) -> None:
        self.hidden = True


class _SignalWindow(QObject):
    restart_requested = Signal()
    diagnostics_requested = Signal()
    settings_requested = Signal()

    def __init__(self) -> None:
        super().__init__()
        self.statuses: list[DesktopStatus] = []
        self.calls: list[tuple] = []
        self.visible = False
        self.current_conversation_id = "c1"

    def set_status(self, status: DesktopStatus) -> None:
        self.statuses.append(status)

    def show_raised(self) -> None:
        self.visible = True
        self.calls.append(("show",))

    def hide(self) -> None:
        self.visible = False
        self.calls.append(("hide",))

    def isVisible(self) -> bool:
        return self.visible

    def toggle_visibility(self) -> None:
        self.visible = not self.visible
        self.calls.append(("toggle",))

    def sync_from_shared_state(self, *args) -> None:
        self.calls.append(("sync", *args))

    def set_presentation(self, presentation) -> None:
        self.calls.append(("presentation", presentation))

    def bind_conversation(self, conversation_id: str) -> None:
        self.current_conversation_id = conversation_id
        self.calls.append(("bind", conversation_id))

    def open_conversation(self, conversation_id: str) -> None:
        self.calls.append(("open", conversation_id))

    def apply_conversation_update(self, update) -> None:
        self.calls.append(("update", update))

    def refresh_history(self) -> None:
        self.calls.append(("history",))

    def handle_command_completed(self, *args) -> None:
        self.calls.append(("completed", *args))

    def handle_command_failed(self, *args) -> None:
        self.calls.append(("failed", *args))

    def prepare_for_quit(self) -> None:
        self.calls.append(("prepare",))

    def close(self) -> None:
        self.calls.append(("close",))


class _SignalSettings(QObject):
    theme_preview_requested = Signal(str)
    restart_requested = Signal()

    def __init__(self) -> None:
        super().__init__()
        self.calls: list[str] = []

    def show_raised(self) -> None:
        self.calls.append("show")

    def prepare_for_quit(self) -> None:
        self.calls.append("prepare")

    def close(self) -> None:
        self.calls.append("close")


class _Bridge(QObject):
    runtime_event = Signal(object)
    command_completed = Signal(object)
    command_failed = Signal(str, str)

    def __init__(self) -> None:
        super().__init__()
        self.commands = []
        self.closed = False
        self.submit_error: Exception | None = None

    def submit(self, command):
        if self.submit_error is not None:
            raise self.submit_error
        self.commands.append(command)
        future = concurrent.futures.Future()
        future.set_result(None)
        return future

    def close(self) -> None:
        self.closed = True


class _Client:
    def __init__(self) -> None:
        self.starts = 0
        self.closes = 0
        self.shutdowns: list[str] = []
        self.start_error: Exception | None = None

    def start(self):
        self.starts += 1
        if self.start_error is not None:
            raise self.start_error
        future = concurrent.futures.Future()
        future.set_result(True)
        return future

    def close(self) -> None:
        self.closes += 1

    def shutdown(self, reason: str):
        self.shutdowns.append(reason)


class _Service:
    def __init__(self) -> None:
        self.calls: list[tuple] = []
        self.update = SimpleNamespace(metadata_changed=True, full_reload=True)

    def apply_event(self, event):
        self.calls.append(("event", event))
        return self.update

    def bind_receipt(self, receipt):
        self.calls.append(("receipt", receipt))
        return self.update

    def mark_command_failed(self, request_id, message):
        self.calls.append(("failed", request_id, message))
        return self.update


def make_controller(*, service=None):
    app = qt_app()
    client = _Client()
    bridge = _Bridge()
    tray = _SignalTray()
    window = _SignalWindow()
    settings = _SignalSettings()
    controller = DesktopController(
        app,
        client,  # type: ignore[arg-type]
        bridge,  # type: ignore[arg-type]
        tray_factory=lambda: tray,  # type: ignore[arg-type]
        window_factory=lambda: window,  # type: ignore[arg-type]
        settings_factory=lambda: settings,
        conversation_service=service,
    )
    return app, controller, client, bridge, tray, window, settings


def dispose(controller: DesktopController) -> None:
    controller.setParent(None)
    controller.deleteLater()
    qt_app().processEvents()


def test_controller_edge_lifecycle_and_surface_paths():
    service = _Service()
    app, controller, client, bridge, tray, window, settings = make_controller(service=service)
    quick = _SignalWindow()
    controller.quick_window = quick
    try:
        controller.show_quick_copilot()
        assert quick.calls[-2:] == [("sync",), ("show",)]
        controller.hide_quick_copilot()
        assert quick.calls[-1] == ("hide",)
        controller.toggle_quick_copilot()
        assert quick.visible is True
        controller.toggle_quick_copilot()
        assert quick.visible is False

        controller.apply_desktop_control("show")
        controller.apply_desktop_control("hide")
        controller.apply_desktop_control("toggle")
        with pytest.raises(ValueError, match="unsupported desktop control command"):
            controller.apply_desktop_control("explode")

        controller.open_full_chat("conversation-2")
        controller.open_full_chat()
        assert ("open", "conversation-2") in window.calls
        assert ("show",) in window.calls

        quick.current_conversation_id = "quick-conversation"
        controller.expand_quick_to_full_chat()
        assert ("open", "quick-conversation") in window.calls
        assert quick.calls[-1] == ("hide",)

        controller.open_settings()
        controller.open_settings()
        assert settings.calls == ["show", "show"]

        update = SimpleNamespace(metadata_changed=True, full_reload=False)
        controller._on_surface_conversation_changed(update)
        assert ("update", update) in window.calls
        assert ("history",) in window.calls
        assert quick.calls[-1] == ("sync",)

        controller._on_runtime_envelope(SimpleNamespace(event=None))
        controller._on_runtime_envelope(SimpleNamespace(event=events.AgentStarted()))
        assert service.calls[-1][0] == "event"
        assert any(call[0] == "update" for call in window.calls)
        assert quick.calls[-1][0] == "sync"

        controller._on_command_completed(SimpleNamespace(request_id="not-a-receipt"))
        receipt = CommandReceipt(request_id="r1")
        controller._on_command_completed(receipt)
        assert service.calls[-1] == ("receipt", receipt)
        assert any(call[0] == "completed" for call in window.calls)
        assert any(call[0] == "completed" for call in quick.calls)

        controller._on_command_failed("failed-request", "nope")
        assert service.calls[-1] == ("failed", "failed-request", "nope")
        assert any(call[0] == "failed" for call in window.calls)
        assert any(call[0] == "failed" for call in quick.calls)

        controller._set_status(DesktopStatus(DesktopRuntimeState.ERROR, "boom"))
        assert tray.statuses[-1].detail == "boom"
        assert window.statuses[-1].detail == "boom"
        assert quick.statuses[-1].detail == "boom"

        controller._close_surfaces()
        assert bridge.closed is True
        assert tray.hidden is True
        assert ("prepare",) in window.calls and ("close",) in window.calls
        assert ("prepare",) in quick.calls and ("close",) in quick.calls
        assert settings.calls[-2:] == ["prepare", "close"]
    finally:
        controller.quick_window = None
        dispose(controller)


def test_controller_guards_restart_start_receipts_and_legacy_shutdown():
    app, controller, client, bridge, _tray, window, _settings = make_controller(service=None)
    try:
        controller._quitting = True
        cancelled = controller.start()
        assert cancelled.cancelled()
        controller.restart_runtime()
        assert bridge.commands == []
        controller._on_runtime_envelope(SimpleNamespace(event=events.RuntimeStarted()))
        controller._on_command_completed(CommandReceipt(request_id="ignored"))
        controller._on_command_failed("ignored", "ignored")
        assert controller.status == INITIAL_STATUS

        controller._quitting = False
        controller._finalized = False
        controller._start_future = None
        client.start_error = RuntimeError("sync start failed")
        future = controller.start()
        assert isinstance(future.exception(), RuntimeError)
        controller._on_client_start_completed(future)
        assert controller.status.state is DesktopRuntimeState.DISCONNECTED

        client.close = None  # type: ignore[assignment]
        controller._close_client()
        assert client.shutdowns == ["desktop standalone compatibility exit"]

        controller._finalized = True
        controller._finalize_quit()
        controller._about_to_quit()
        assert window.calls.count(("close",)) == 0
    finally:
        dispose(controller)


def test_controller_restart_failure_default_and_same_surface_resync():
    service = _Service()
    _app, controller, _client, bridge, _tray, window, _settings = make_controller(service=service)
    try:
        controller.quick_window = controller.window
        controller._resync_conversation_surfaces()
        assert window.calls[-1] == ("sync",)

        update = SimpleNamespace(metadata_changed=False, full_reload=False)
        controller._on_surface_conversation_changed(update)
        assert window.calls[-1] == ("sync",)

        controller.restart_runtime()
        request_id = bridge.commands[-1].request_id
        controller._on_command_failed(request_id, "")
        assert controller.status == DesktopStatus(
            DesktopRuntimeState.ERROR,
            "Runtime restart failed",
        )
    finally:
        controller.quick_window = None
        dispose(controller)


def test_desktop_control_failure_boundaries(tmp_path, monkeypatch):
    with pytest.raises(ConnectionError, match="not running"):
        control.send_desktop_control("show", runtime_dir=tmp_path)

    endpoint = control.desktop_control_path(tmp_path)
    tmp_path.mkdir(exist_ok=True)
    endpoint.write_text("not a socket", encoding="utf-8")
    with pytest.raises(PermissionError, match="private owned unix socket"):
        control.send_desktop_control("show", runtime_dir=tmp_path)
    endpoint.unlink()

    private = tmp_path / "private"
    private.mkdir(mode=0o700)
    os.chmod(private, 0o700)
    monkeypatch.setattr(control.os, "getuid", lambda: os.stat(private).st_uid + 1)
    with pytest.raises(PermissionError, match="not private"):
        control._validate_private_directory(private)
    monkeypatch.undo()

    public = tmp_path / "public"
    public.mkdir(mode=0o777)
    os.chmod(public, 0o777)
    with pytest.raises(PermissionError, match="not private"):
        control._validate_private_directory(public)

    regular = tmp_path / "regular"
    regular.write_text("x", encoding="utf-8")
    with pytest.raises(PermissionError, match="not a unix socket"):
        control._recover_endpoint(regular)
    control._recover_endpoint(tmp_path / "absent.sock")

    with pytest.raises(ValueError, match="unsupported desktop control command"):
        control._validate_command(" launch ")


def test_desktop_control_handler_replies_fail_closed(tmp_path):
    server = control.DesktopControlServer(tmp_path, lambda _command: None)

    class Connection:
        def __init__(self, payload: bytes, *, send_error: bool = False) -> None:
            self.payload = payload
            self.sent: list[bytes] = []
            self.send_error = send_error

        def recv(self, _size: int) -> bytes:
            return self.payload

        def sendall(self, payload: bytes) -> None:
            if self.send_error:
                raise OSError("peer gone")
            self.sent.append(payload)

    empty = Connection(b"")
    server._handle(empty)  # type: ignore[arg-type]
    assert empty.sent == []

    invalid = Connection(b"\xff\n")
    server._handle(invalid)  # type: ignore[arg-type]
    assert invalid.sent == [b"error invalid-command\n"]

    oversized = Connection(b"x" * (control._MAX_COMMAND_BYTES + 1))
    server._handle(oversized)  # type: ignore[arg-type]
    assert oversized.sent == [b"error invalid-command\n"]

    failing_dispatch = control.DesktopControlServer(
        tmp_path,
        lambda _command: (_ for _ in ()).throw(RuntimeError("boom")),
    )
    connection = Connection(b"show\n")
    failing_dispatch._handle(connection)  # type: ignore[arg-type]
    assert connection.sent == [b"error dispatch-failed\n"]

    control.DesktopControlServer._reply(
        Connection(b"", send_error=True), b"ok\n"
    )  # type: ignore[arg-type]


def test_desktop_control_server_close_is_idempotent_and_does_not_unlink_foreign_path(tmp_path):
    server = control.DesktopControlServer(tmp_path, lambda _command: None)
    server.close()
    server.start()
    endpoint = server.endpoint
    server.close()
    assert not endpoint.exists()
    server.close()

    endpoint.write_text("replacement", encoding="utf-8")
    server._owns_endpoint = True
    server.close()
    assert endpoint.read_text(encoding="utf-8") == "replacement"


def test_client_contract_backoff_validation_and_inprocess_close():
    class Client(ZaraClient):
        def __init__(self) -> None:
            self.outcomes: list[BaseException | None] = []

        @property
        def state(self):
            return ZaraClientState.NEW

        def start(self):
            raise AssertionError

        def submit(self, command):
            raise AssertionError

        def subscribe(self, *, maxsize=0):
            raise AssertionError

        def shutdown(self, reason="client shutdown"):
            raise AssertionError

        def close(self, timeout=None):
            raise AssertionError

        def reconnect(self):
            future = concurrent.futures.Future()
            outcome = self.outcomes.pop(0)
            if outcome is None:
                future.set_result(True)
            else:
                future.set_exception(outcome)
            return future

    client = Client()

    for value in (0, -1, True, 1.5):
        with pytest.raises((TypeError, ValueError)):
            client.reconnect_with_backoff(max_attempts=value)  # type: ignore[arg-type]
    for name, value in (("initial_delay", True), ("max_delay", "1")):
        with pytest.raises(TypeError):
            client.reconnect_with_backoff(**{name: value})  # type: ignore[arg-type]
    with pytest.raises(ValueError, match="non-negative"):
        client.reconnect_with_backoff(initial_delay=-0.1)
    with pytest.raises(TypeError, match="sleeper"):
        client.reconnect_with_backoff(sleeper=None)  # type: ignore[arg-type]

    client.outcomes = [ConnectionError("one"), ConnectionError("two"), None]
    sleeps: list[float] = []
    result = client.reconnect_with_backoff(
        max_attempts=3,
        initial_delay=0.25,
        max_delay=0.4,
        sleeper=sleeps.append,
    )
    assert result.result(timeout=2.0) is True
    assert sleeps == [0.25, 0.4]

    client.outcomes = [ConnectionError("dead"), ConnectionError("still dead")]
    failed = client.reconnect_with_backoff(
        max_attempts=2,
        initial_delay=0,
        sleeper=lambda _delay: None,
    )
    with pytest.raises(ConnectionError, match="still dead"):
        failed.result(timeout=2.0)

    default_client = Client()
    default_client.reconnect = ZaraClient.reconnect.__get__(
        default_client, Client
    )  # type: ignore[method-assign]
    with pytest.raises(NotImplementedError):
        default_client.reconnect().result()

    class Host:
        def __init__(self) -> None:
            self.state = RuntimeHostState.RUNNING
            self.is_alive = True
            self.join_calls: list[float] = []

        def shutdown(self, reason="client shutdown"):
            future = concurrent.futures.Future()
            future.set_result(None)
            return future

        def join(self, timeout=None):
            self.join_calls.append(timeout)

    in_process = InProcessZaraClient.__new__(InProcessZaraClient)
    in_process._host = Host()
    in_process._bus = SimpleNamespace(subscribe=lambda **_kwargs: None)
    in_process._shutdown_timeout = 0.1
    in_process._closed = False
    assert in_process.state is ZaraClientState.READY
    with pytest.raises(RuntimeNotReady, match="did not stop"):
        in_process.close(timeout=0)
    in_process._host.is_alive = False
    in_process.close(timeout=0)
    assert in_process.state is ZaraClientState.STOPPED
    in_process.close(timeout=0)


def test_daemon_client_validation_and_precedence(monkeypatch, tmp_path):
    class Config:
        config_dir = tmp_path

        def __init__(self, daemon):
            self.daemon = daemon

        def get_section(self, name):
            assert name == "daemon"
            return self.daemon

    with pytest.raises(ValueError, match="must be a table"):
        daemon_client._daemon_section(Config("bad"))  # type: ignore[arg-type]

    with pytest.raises(ValueError, match="must not be empty"):
        daemon_client.resolve_daemon_endpoint(Config({}), explicit="   ")

    monkeypatch.setenv(daemon_client.DAEMON_ENDPOINT_ENV, " tcp://127.0.0.1:5555 ")
    assert daemon_client.resolve_daemon_endpoint(Config({})) == "tcp://127.0.0.1:5555"
    monkeypatch.delenv(daemon_client.DAEMON_ENDPOINT_ENV)
    assert daemon_client.resolve_daemon_endpoint(
        Config({"endpoint": " ipc:///tmp/daemon "})
    ) == "ipc:///tmp/daemon"

    monkeypatch.setattr(daemon_client, "_paired_profile", lambda _config=None: None)
    assert daemon_client.curve_client_config(Config({})) is None
    with pytest.raises(ValueError, match="requires public, secret, and server public keys"):
        daemon_client.curve_client_config(Config({"curve_public_key": "pub"}))
    with pytest.raises(TypeError, match="curve_client is owned"):
        daemon_client.create_daemon_client(config=Config({}), curve_client=object())


def test_replay_status_input_boundaries_and_main(monkeypatch, capsys):
    for value, error in (
        (123, TypeError),
        ("   ", ValueError),
        ("bad\x00id", ValueError),
        ("x" * 129, ValueError),
    ):
        with pytest.raises(error):
            _normalize_conversation_id(value)  # type: ignore[arg-type]
    assert _normalize_conversation_id("  ok  ") == "ok"
    assert symbolic_projection_payload(None) is None

    class Store:
        def get_conversation(self, conversation_id):
            return SimpleNamespace(id=conversation_id)

        def load_symbolic_projection(self, conversation_id):
            return None

    assert conversation_symbolic_status(Store(), " c1 ")["conversation_id"] == "c1"

    monkeypatch.setattr(
        "zara.desktop.conversation.replay_status.ConversationStore",
        lambda: Store(),
    )
    assert replay_main(["--conversation-id", "c1"]) == 0
    payload = capsys.readouterr().out
    assert '"conversation_id":"c1"' in payload

    class BrokenStore(Store):
        def get_conversation(self, conversation_id):
            return None

    monkeypatch.setattr(
        "zara.desktop.conversation.replay_status.ConversationStore",
        lambda: BrokenStore(),
    )
    assert replay_main(["--conversation-id", "missing"]) == 2
    assert "unknown conversation" in capsys.readouterr().err


def projection(**overrides) -> SymbolicConversationProjection:
    values = dict(
        conversation_id="c1",
        projection_generation=1,
        runtime_generation=1,
        turn_id="t1",
        outcome="success",
        project_id="project-a",
        project_generation=2,
        dialogue_act="inform",
        dialogue_state={
            "prolog_context_project_id": "project-a",
            "prolog_context_project_generation": 2,
            "prolog_context_term": "[topic(foo)]",
            "response_act_term": "inform(foo)",
        },
        discourse_entities=[],
        unresolved_questions=[
            {"id": "q1", "source": "symbolic_dialogue", "text": "old"}
        ],
        expert_evidence=[],
        verified_facts=[],
        verified_outcome_refs=[],
        renderer_provenance="symbolic-dcg/v1",
        providers_enabled=False,
        max_model_calls=0,
        provider_calls=0,
        model_calls=0,
    )
    values.update(overrides)
    return SymbolicConversationProjection(**values)


class ProjectionStore:
    def __init__(self, current=None) -> None:
        self.current = current
        self.saved = None

    def load_symbolic_projection(self, _conversation_id):
        return self.current

    def save_symbolic_projection(self, value, *, expected_generation):
        assert expected_generation == value.projection_generation - 1
        self.saved = value
        self.current = value
        return value


def test_symbolic_runtime_validation_stale_project_error_and_expert_paths():
    assert _dialogue_context_matches_project(projection()) is True
    assert _dialogue_context_matches_project(
        projection(dialogue_state={"prolog_context_project_id": 42})
    ) is False
    assert _dialogue_context_matches_project(
        projection(dialogue_state={"prolog_context_project_generation": True})
    ) is False

    for value, error in (
        (123, TypeError),
        ("", ValueError),
        ("x" * 129, ValueError),
        ("bad\nref", ValueError),
    ):
        with pytest.raises(error):
            _bounded_expert_evidence_ref(value)
    assert _bounded_expert_evidence_ref("evidence:ok") == "evidence:ok"

    empty = ProjectionStore(None)
    adapter = PureSymbolicProjectionAdapter(empty)
    assert adapter.load_dialogue_state("c1") == ("[]", None, 0)

    mismatched = ProjectionStore(
        projection(
            dialogue_state={
                "prolog_context_project_id": "other",
                "prolog_context_project_generation": 2,
            }
        )
    )
    assert adapter.__class__(mismatched).load_dialogue_state("c1") == ("[]", None, 1)

    bad_context = ProjectionStore(
        projection(
            dialogue_state={
                "prolog_context_project_id": "project-a",
                "prolog_context_project_generation": 2,
                "prolog_context_term": 123,
            }
        )
    )
    with pytest.raises(TypeError, match="context must be text"):
        adapter.__class__(bad_context).load_dialogue_state("c1")

    bad_act = ProjectionStore(
        projection(
            dialogue_state={
                "prolog_context_project_id": "project-a",
                "prolog_context_project_generation": 2,
                "prolog_context_term": "[]",
                "response_act_term": 123,
            }
        )
    )
    with pytest.raises(TypeError, match="response act must be text"):
        adapter.__class__(bad_act).load_dialogue_state("c1")

    store = ProjectionStore(projection())
    with pytest.raises(RuntimeError, match="stale symbolic projection result"):
        adapter.__class__(store).commit_turn(
            conversation_id="c1",
            expected_generation=0,
            turn_id="t2",
            response="answer",
            dialogue_act="inform",
            response_act_term="inform(answer)",
            context_term="[]",
            renderer_provenance="symbolic-dcg/v1",
        )

    adapter = adapter.__class__(store)
    adapter.commit_turn(
        conversation_id="c1",
        expected_generation=1,
        turn_id="t2",
        response="need detail",
        dialogue_act="clarify",
        response_act_term="clarify(detail)",
        context_term="[topic(bar)]",
        renderer_provenance="symbolic-dcg/v1",
    )
    assert store.saved.unresolved_questions[-1]["source"] == "symbolic_dialogue"

    adapter.commit_turn(
        conversation_id="c1",
        expected_generation=2,
        turn_id="t3",
        response="expert answer",
        dialogue_act="expert_answer",
        response_act_term="inform(expert)",
        context_term="[topic(expert)]",
        renderer_provenance="symbolic-dcg/v1",
        expert_evidence_ref="expert:1",
    )
    assert store.saved.expert_evidence == [{"ref": "expert:1"}]

    with pytest.raises(RuntimeError, match="non-expert"):
        adapter.commit_turn(
            conversation_id="c1",
            expected_generation=3,
            turn_id="t4",
            response="bad",
            dialogue_act="inform",
            response_act_term="inform(bad)",
            context_term="[]",
            renderer_provenance="symbolic-dcg/v1",
            expert_evidence_ref="expert:bad",
        )

    error_store = ProjectionStore(
        projection(
            dialogue_state={
                "prolog_context_project_id": "project-a",
                "prolog_context_project_generation": 2,
                "prolog_context_term": "[keep(old)]",
                "response_act_term": "inform(old)",
            }
        )
    )
    prior_questions = list(error_store.current.unresolved_questions)
    adapter = adapter.__class__(error_store)
    adapter.commit_turn(
        conversation_id="c1",
        expected_generation=1,
        turn_id="error-turn",
        response="renderer failed",
        dialogue_act="error",
        response_act_term="error(new)",
        context_term="[drop(new)]",
        renderer_provenance="symbolic-dcg/v1",
    )
    assert error_store.saved.outcome == "error"
    assert error_store.saved.dialogue_state["prolog_context_term"] == "[keep(old)]"
    assert error_store.saved.dialogue_state["response_act_term"] == "inform(old)"
    assert error_store.saved.unresolved_questions == prior_questions
