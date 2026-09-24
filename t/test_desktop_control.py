from __future__ import annotations

import os
import socket
import stat
from pathlib import Path

import pytest

from zara.desktop.control import (
    DesktopControlAlreadyRunning,
    DesktopControlServer,
    desktop_control_path,
    send_desktop_control,
)


def test_desktop_control_round_trip_uses_closed_vocabulary(tmp_path):
    commands: list[str] = []
    server = DesktopControlServer(tmp_path, commands.append)
    server.start()
    try:
        assert send_desktop_control("toggle", runtime_dir=tmp_path) == "ok"
        assert send_desktop_control("show", runtime_dir=tmp_path) == "ok"
        assert send_desktop_control("hide", runtime_dir=tmp_path) == "ok"
        assert commands == ["toggle", "show", "hide"]
        with pytest.raises(ValueError, match="unsupported desktop control command"):
            send_desktop_control("eval print(1)", runtime_dir=tmp_path)
    finally:
        server.close()


def test_desktop_control_endpoint_is_owner_private(tmp_path):
    runtime_dir = tmp_path / "runtime"
    runtime_dir.mkdir(mode=0o777)
    os.chmod(runtime_dir, 0o777)
    server = DesktopControlServer(runtime_dir, lambda _command: None)
    server.start()
    try:
        assert stat.S_IMODE(runtime_dir.stat().st_mode) == 0o700
        endpoint = desktop_control_path(runtime_dir)
        info = os.lstat(endpoint)
        assert stat.S_ISSOCK(info.st_mode)
        assert info.st_uid == os.getuid()
        assert stat.S_IMODE(info.st_mode) == 0o600
        assert endpoint.parent.stat().st_uid == os.getuid()
        assert stat.S_IMODE(endpoint.parent.stat().st_mode) == 0o700
    finally:
        server.close()


def test_desktop_control_rejects_duplicate_owner_without_unlinking_live_owner(tmp_path):
    first = DesktopControlServer(tmp_path, lambda _command: None)
    second = DesktopControlServer(tmp_path, lambda _command: None)
    first.start()
    try:
        with pytest.raises(DesktopControlAlreadyRunning):
            second.start()
        second.close()
        assert desktop_control_path(tmp_path).exists()
        assert send_desktop_control("toggle", runtime_dir=tmp_path) == "ok"
    finally:
        second.close()
        first.close()


def test_desktop_control_recovers_same_user_stale_socket(tmp_path):
    runtime_dir = Path(tmp_path)
    endpoint = desktop_control_path(runtime_dir)
    runtime_dir.mkdir(mode=0o700, parents=True, exist_ok=True)
    endpoint.parent.mkdir(mode=0o700, parents=True, exist_ok=True)
    os.chmod(endpoint.parent, 0o700)
    stale = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    stale.bind(str(endpoint))
    stale.close()
    assert endpoint.exists()

    commands: list[str] = []
    server = DesktopControlServer(runtime_dir, commands.append)
    server.start()
    try:
        assert send_desktop_control("toggle", runtime_dir=runtime_dir) == "ok"
        assert commands == ["toggle"]
    finally:
        server.close()


def test_desktop_control_rejects_oversized_raw_command(tmp_path):
    server = DesktopControlServer(tmp_path, lambda _command: pytest.fail("oversized dispatched"))
    server.start()
    try:
        endpoint = desktop_control_path(tmp_path)
        client = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        client.settimeout(1.0)
        client.connect(str(endpoint))
        client.sendall(b"x" * 1024 + b"\n")
        response = client.recv(64)
        client.close()
        assert response.startswith(b"error")
    finally:
        server.close()


def test_desktop_control_long_runtime_path_uses_deterministic_private_fallback(tmp_path):
    runtime_dir = tmp_path / ("nested-" * 24)
    endpoint = desktop_control_path(runtime_dir)
    assert len(os.fsencode(endpoint)) <= 100
    assert endpoint == desktop_control_path(runtime_dir)
    assert endpoint.parent != runtime_dir

    commands: list[str] = []
    server = DesktopControlServer(runtime_dir, commands.append)
    server.start()
    try:
        assert send_desktop_control("show", runtime_dir=runtime_dir) == "ok"
        assert commands == ["show"]
        assert endpoint.parent.stat().st_uid == os.getuid()
        assert stat.S_IMODE(endpoint.parent.stat().st_mode) == 0o700
    finally:
        server.close()


def test_desktop_control_start_is_idempotent_and_keeps_single_owner(tmp_path):
    commands: list[str] = []
    server = DesktopControlServer(tmp_path, commands.append)
    server.start()
    endpoint = server.endpoint
    first_inode = os.lstat(endpoint).st_ino
    try:
        server.start()
        assert os.lstat(endpoint).st_ino == first_inode
        assert send_desktop_control("show", runtime_dir=tmp_path) == "ok"
        assert commands == ["show"]
    finally:
        server.close()


def test_desktop_control_rejects_regular_file_at_endpoint_without_deleting_it(tmp_path):
    runtime_dir = Path(tmp_path)
    runtime_dir.mkdir(mode=0o700, exist_ok=True)
    os.chmod(runtime_dir, 0o700)
    endpoint = desktop_control_path(runtime_dir)
    endpoint.write_text("not a socket", encoding="utf-8")

    server = DesktopControlServer(runtime_dir, lambda _command: None)
    with pytest.raises(PermissionError, match="not a unix socket"):
        server.start()

    assert endpoint.read_text(encoding="utf-8") == "not a socket"


def test_desktop_control_dispatch_failure_is_explicit_and_server_survives(tmp_path):
    calls: list[str] = []

    def dispatch(command: str) -> None:
        calls.append(command)
        if command == "show":
            raise RuntimeError("ui unavailable")

    server = DesktopControlServer(tmp_path, dispatch)
    server.start()
    try:
        with pytest.raises(RuntimeError, match="error dispatch-failed"):
            send_desktop_control("show", runtime_dir=tmp_path)
        assert send_desktop_control("hide", runtime_dir=tmp_path) == "ok"
        assert calls == ["show", "hide"]
    finally:
        server.close()


def test_desktop_control_rejects_non_ascii_wire_payload_without_killing_owner(tmp_path):
    commands: list[str] = []
    server = DesktopControlServer(tmp_path, commands.append)
    server.start()
    try:
        client = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        client.settimeout(1.0)
        client.connect(str(server.endpoint))
        client.sendall(b"\xff\n")
        assert client.recv(64) == b"error invalid-command\n"
        client.close()

        assert send_desktop_control("toggle", runtime_dir=tmp_path) == "ok"
        assert commands == ["toggle"]
    finally:
        server.close()


def test_desktop_control_empty_peer_does_not_kill_owner(tmp_path):
    commands: list[str] = []
    server = DesktopControlServer(tmp_path, commands.append)
    server.start()
    try:
        client = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        client.connect(str(server.endpoint))
        client.close()

        assert send_desktop_control("show", runtime_dir=tmp_path) == "ok"
        assert commands == ["show"]
    finally:
        server.close()


def test_send_desktop_control_reports_missing_endpoint(tmp_path):
    runtime_dir = Path(tmp_path)
    runtime_dir.mkdir(mode=0o700, exist_ok=True)
    os.chmod(runtime_dir, 0o700)

    with pytest.raises(ConnectionError, match="not running"):
        send_desktop_control("show", runtime_dir=runtime_dir)


def test_send_desktop_control_rejects_owned_regular_file_endpoint(tmp_path):
    runtime_dir = Path(tmp_path)
    runtime_dir.mkdir(mode=0o700, exist_ok=True)
    os.chmod(runtime_dir, 0o700)
    endpoint = desktop_control_path(runtime_dir)
    endpoint.write_text("not a socket", encoding="utf-8")

    with pytest.raises(PermissionError, match="private owned unix socket"):
        send_desktop_control("hide", runtime_dir=runtime_dir)


def test_send_desktop_control_surfaces_owner_error_response(tmp_path):
    runtime_dir = Path(tmp_path)
    runtime_dir.mkdir(mode=0o700, exist_ok=True)
    os.chmod(runtime_dir, 0o700)
    endpoint = desktop_control_path(runtime_dir)
    owner = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    owner.bind(str(endpoint))
    os.chmod(endpoint, 0o600)
    owner.listen(1)

    def serve_once() -> None:
        connection, _ = owner.accept()
        with connection:
            assert connection.recv(64) == b"show\n"
            connection.sendall(b"error rejected\n")

    worker = __import__("threading").Thread(target=serve_once, daemon=True)
    worker.start()
    try:
        with pytest.raises(RuntimeError, match="error rejected"):
            send_desktop_control("show", runtime_dir=runtime_dir)
    finally:
        owner.close()
        worker.join(timeout=1.0)
        endpoint.unlink(missing_ok=True)


def test_desktop_control_close_tolerates_endpoint_removed_by_owner_cleanup(tmp_path):
    server = DesktopControlServer(tmp_path, lambda _command: None)
    server.start()
    endpoint = server.endpoint
    endpoint.unlink()

    server.close()

    assert not endpoint.exists()
