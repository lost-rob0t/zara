from __future__ import annotations

import json
import os
import socket
import sys
import threading
import time

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

import pytest
import zmq
from PySide6.QtCore import Qt
from PySide6.QtWidgets import QApplication, QInputDialog

from zara.config import DEFAULT_CONFIG_TOML, ZaraConfig
from zara.desktop.windows import SettingsWindow
from zara.desktop.windows.pairing_host import (
    PairingHostError,
    PairRemoteDeviceDialog,
    list_paired_devices,
    nickname_suggestion,
    qr_pixmap,
    set_device_nickname,
)
from zara.pairing import PairingHostPlan, derive_device_id, parse_pairing_uri
from zara.security import Capability, PrincipalContext
from zara.security_admin import SecurityAdminServer
from zara.security_state import PersistentSecurityState

CLIENT_KEY = "b" * 40
CLIENT_DEVICE_ID = derive_device_id(CLIENT_KEY)


def app() -> QApplication:
    instance = QApplication.instance()
    assert instance is None or isinstance(instance, QApplication)
    return instance or QApplication([])


def make_config(tmp_path) -> ZaraConfig:
    config_path = tmp_path / "xdg" / "config.toml"
    config_path.parent.mkdir(parents=True, exist_ok=True)
    config_path.write_text(DEFAULT_CONFIG_TOML, encoding="utf-8")
    return ZaraConfig(str(config_path))


def make_window(tmp_path, *, device_lister=list_paired_devices, device_renamer=set_device_nickname):
    app()
    root = tmp_path / "repo"
    (root / "kb").mkdir(parents=True, exist_ok=True)
    (root / "modules").mkdir(parents=True, exist_ok=True)
    (root / "main.pl").write_text("main :- true.\n", encoding="utf-8")
    (root / "kb" / "intents.pl").write_text("intent(ok).\n", encoding="utf-8")
    (root / "modules" / "logic.pl").write_text("logic(ok).\n", encoding="utf-8")
    return SettingsWindow(
        make_config(tmp_path),
        repo_root=root,
        device_lister=device_lister,
        device_renamer=device_renamer,
    )


def dispose_window(window: SettingsWindow) -> None:
    window.prepare_for_quit()
    window.close()
    window.deleteLater()
    app().processEvents()


def wait_for(predicate, timeout: float = 5.0) -> bool:
    qt_app = app()
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        qt_app.processEvents()
        if predicate():
            return True
        time.sleep(0.02)
    return predicate()


def enroll_one_device(tmp_path, device_id: str = "android-abc123", nickname: str = "") -> PersistentSecurityState:
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    public, _secret = zmq.curve_keypair()
    state.enroll_client(
        public.decode("ascii"),
        device_id=device_id,
        principal=PrincipalContext.local_owner(),
        capabilities={Capability.SESSION_BASIC},
    )
    if nickname:
        state.rename_device(device_id, nickname)
    return state


def live_pairing_plan(tmp_path) -> tuple[PairingHostPlan, SecurityAdminServer]:
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    registry = state.load_registry()
    admin = SecurityAdminServer(
        state,
        capabilities={Capability.SESSION_BASIC, Capability.TURN_SUBMIT},
    )
    admin.bind_registry(registry)
    admin.start()
    plan = PairingHostPlan(
        endpoint="tcp://127.0.0.1:7731",
        advertise_host="127.0.0.1",
        bind_host="127.0.0.1",
        server_public_key=state.server_public_key(),
        security_dir=tmp_path / "security",
        control_socket_path=admin.path,
    )
    return plan, admin


def scripted_device(uri: str, received: list[dict[str, object]], kind: str = "android") -> None:
    payload = parse_pairing_uri(uri)
    connection = socket.create_connection((payload.broker_host, payload.broker_port), timeout=5.0)
    connection.settimeout(10.0)
    stream = connection.makefile("rwb", buffering=0)
    try:
        request = {
            "version": 1,
            "token": payload.token,
            "public_key": CLIENT_KEY,
            "client_kind": kind,
        }
        stream.write(json.dumps(request, separators=(",", ":")).encode("utf-8") + b"\n")
        received.append(json.loads(stream.readline(4097)))
        received.append(json.loads(stream.readline(4097)))
    finally:
        stream.close()
        connection.close()


def test_connections_page_lists_paired_devices_with_nicknames(tmp_path, monkeypatch) -> None:
    enroll_one_device(tmp_path, nickname="Pixel 9")
    monkeypatch.setenv("ZARA_SECURITY_DIR", str(tmp_path / "security"))
    window = make_window(tmp_path)
    try:
        assert window.pair_remote_button.text() == "Pair Remote device"
        assert window.device_list.objectName() == "zaraRemoteDeviceList"
        assert wait_for(lambda: window.device_list.count() == 1)
        row = window.device_list.item(0).text()
        assert "Pixel 9" in row
        assert "android-abc123" in row
        assert window.device_list.item(0).data(Qt.ItemDataRole.UserRole) == "android-abc123"
    finally:
        dispose_window(window)


def test_connections_page_reports_unreadable_security_state(tmp_path, monkeypatch) -> None:
    (tmp_path / "broken").mkdir()
    clients = tmp_path / "broken" / "clients.json"
    clients.write_text("{not json}\n", encoding="utf-8")
    os.chmod(clients, 0o600)
    monkeypatch.setenv("ZARA_SECURITY_DIR", str(tmp_path / "broken"))
    window = make_window(tmp_path)
    try:
        assert wait_for(lambda: "unavailable" in window.device_status.text())
    finally:
        dispose_window(window)


def test_rename_action_persists_device_nickname(tmp_path, monkeypatch) -> None:
    enroll_one_device(tmp_path, nickname="")
    monkeypatch.setenv("ZARA_SECURITY_DIR", str(tmp_path / "security"))
    window = make_window(tmp_path)
    try:
        assert wait_for(lambda: window.device_list.count() == 1)
        window.device_list.setCurrentRow(0)
        monkeypatch.setattr(
            QInputDialog,
            "getText",
            staticmethod(lambda *args, **kwargs: ("Kitchen display", True)),
        )
        window.rename_selected_device()
        assert wait_for(lambda: "Kitchen display" in window.device_list.item(0).text())
        state = PersistentSecurityState(tmp_path / "security")
        assert state.list_clients()[0]["nickname"] == "Kitchen display"
    finally:
        dispose_window(window)


def test_qr_payload_renders_a_decodable_image(monkeypatch) -> None:
    pixmap = qr_pixmap("zara://pair/v1?broker_host=127.0.0.1")
    assert not pixmap.isNull()
    assert pixmap.width() >= 100 and pixmap.height() >= 100

    monkeypatch.setitem(sys.modules, "qrcode", None)
    with pytest.raises(PairingHostError, match="QR rendering is unavailable"):
        qr_pixmap("zara://pair/v1?broker_host=127.0.0.1")


def test_nickname_suggestion_is_device_generic() -> None:
    assert nickname_suggestion("android-e26d2da3ab58") == "Phone"
    assert nickname_suggestion("desktop-e26d2da3ab58") == "Desktop"
    assert nickname_suggestion("toaster-e26d2da3ab58") == "Device"


def test_pair_remote_dialog_pairs_names_and_enrolls_device(tmp_path) -> None:
    plan, admin = live_pairing_plan(tmp_path)
    received: list[dict[str, object]] = []
    qt_app = app()
    dialog = PairRemoteDeviceDialog(
        make_config(tmp_path),
        session_seconds=60,
        plan_factory=lambda _config: plan,
    )
    finished: list[tuple[str, str]] = []
    claims: list[object] = []
    dialog.session_finished.connect(lambda device, error: finished.append((device, error)))
    dialog.claim_observed.connect(claims.append)
    try:
        dialog.start_session()
        assert wait_for(lambda: bool(dialog.current_uri()))
        assert "tcp://127.0.0.1:7731" in dialog.endpoint_label.text()
        assert "127.0.0.1" in dialog.host_label.text()
        client_thread = threading.Thread(
            target=lambda: scripted_device(dialog.current_uri(), received),
            daemon=True,
        )
        client_thread.start()

        assert wait_for(lambda: bool(claims))
        claim = claims[0]
        assert claim.device_id == CLIENT_DEVICE_ID
        assert claim.verification_code == received[0]["verification_code"]
        assert claim.nickname_suggestion == "Phone"
        assert len(received) == 1

        dialog.nickname_input.setText("Pixel 9")
        dialog.approve_button.click()
        client_thread.join(timeout=5.0)
        assert wait_for(lambda: bool(finished))
        assert finished == [(CLIENT_DEVICE_ID, "")]
        assert received[1]["status"] == "approved"

        state = PersistentSecurityState(tmp_path / "security")
        clients = state.list_clients()
        assert len(clients) == 1
        assert clients[0]["device_id"] == CLIENT_DEVICE_ID
        assert clients[0]["active"] is True
        assert clients[0]["nickname"] == "Pixel 9"
        assert wait_for(lambda: not dialog.is_session_active())
        qt_app.processEvents()
    finally:
        dialog.cancel_session()
        admin.close(timeout=1.0)
        dialog.deleteLater()
        qt_app.processEvents()


def test_pair_remote_dialog_rejection_reports_error_and_enrolls_nothing(tmp_path) -> None:
    plan, admin = live_pairing_plan(tmp_path)
    received: list[dict[str, object]] = []
    qt_app = app()
    dialog = PairRemoteDeviceDialog(
        make_config(tmp_path),
        session_seconds=60,
        plan_factory=lambda _config: plan,
    )
    finished: list[tuple[str, str]] = []
    claims: list[object] = []
    dialog.session_finished.connect(lambda device, error: finished.append((device, error)))
    dialog.claim_observed.connect(claims.append)
    try:
        dialog.start_session()
        assert wait_for(lambda: bool(dialog.current_uri()))
        client_thread = threading.Thread(
            target=lambda: scripted_device(dialog.current_uri(), received),
            daemon=True,
        )
        client_thread.start()
        assert wait_for(lambda: bool(claims))
        dialog.reject_button.click()
        client_thread.join(timeout=5.0)
        assert not client_thread.is_alive()
        assert wait_for(lambda: bool(finished))
        device_id, error = finished[0]
        assert device_id == CLIENT_DEVICE_ID
        assert "operator_rejected" in error
        assert received[1] == {"status": "rejected", "code": "operator_rejected"}
        assert PersistentSecurityState(tmp_path / "security").list_clients() == ()
    finally:
        dialog.cancel_session()
        admin.close(timeout=1.0)
        dialog.deleteLater()
        qt_app.processEvents()


def test_pair_remote_dialog_cancel_stops_promptly(tmp_path) -> None:
    plan, admin = live_pairing_plan(tmp_path)
    qt_app = app()
    dialog = PairRemoteDeviceDialog(
        make_config(tmp_path),
        session_seconds=60,
        plan_factory=lambda _config: plan,
    )
    finished: list[tuple[str, str]] = []
    dialog.session_finished.connect(lambda device, error: finished.append((device, error)))
    try:
        dialog.start_session()
        assert wait_for(lambda: bool(dialog.current_uri()))
        started = time.monotonic()
        dialog.cancel_session()
        assert wait_for(lambda: bool(finished), timeout=3.0)
        assert time.monotonic() - started < 3.0
        assert finished == [("", "Pairing cancelled")]
        assert not dialog.is_session_active()
    finally:
        dialog.cancel_session()
        admin.close(timeout=1.0)
        dialog.deleteLater()
        qt_app.processEvents()


def test_pair_remote_dialog_cancel_with_pending_claim_finishes_once(tmp_path) -> None:
    plan, admin = live_pairing_plan(tmp_path)
    received: list[dict[str, object]] = []
    qt_app = app()
    dialog = PairRemoteDeviceDialog(
        make_config(tmp_path),
        session_seconds=60,
        plan_factory=lambda _config: plan,
    )
    finished: list[tuple[str, str]] = []
    claims: list[object] = []
    dialog.session_finished.connect(lambda device, error: finished.append((device, error)))
    dialog.claim_observed.connect(claims.append)
    try:
        dialog.start_session()
        assert wait_for(lambda: bool(dialog.current_uri()))
        client_thread = threading.Thread(
            target=lambda: scripted_device(dialog.current_uri(), received),
            daemon=True,
        )
        client_thread.start()
        assert wait_for(lambda: bool(claims))
        dialog.cancel_session()
        client_thread.join(timeout=5.0)
        assert not client_thread.is_alive()
        assert wait_for(lambda: bool(finished), timeout=3.0)
        assert len(finished) == 1
        assert finished[0] == (CLIENT_DEVICE_ID, "Pairing cancelled")
        assert received[1] == {"status": "rejected", "code": "cancelled"}
        assert not dialog.is_session_active()
    finally:
        dialog.cancel_session()
        admin.close(timeout=1.0)
        dialog.deleteLater()
        qt_app.processEvents()


def test_pair_remote_dialog_surfaces_plan_errors(tmp_path) -> None:
    qt_app = app()

    def broken_plan(_config):
        raise RuntimeError("Start the Zara daemon first")

    dialog = PairRemoteDeviceDialog(
        make_config(tmp_path),
        plan_factory=broken_plan,
    )
    finished: list[tuple[str, str]] = []
    dialog.session_finished.connect(lambda device, error: finished.append((device, error)))
    try:
        dialog.start_session()
        assert wait_for(lambda: bool(finished))
        assert finished == [("", "Start the Zara daemon first")]
    finally:
        dialog.cancel_session()
        dialog.deleteLater()
        qt_app.processEvents()
