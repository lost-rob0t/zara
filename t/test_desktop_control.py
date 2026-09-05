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
    finally:
        server.close()


def test_desktop_control_rejects_duplicate_owner(tmp_path):
    first = DesktopControlServer(tmp_path, lambda _command: None)
    second = DesktopControlServer(tmp_path, lambda _command: None)
    first.start()
    try:
        with pytest.raises(DesktopControlAlreadyRunning):
            second.start()
    finally:
        second.close()
        first.close()


def test_desktop_control_recovers_same_user_stale_socket(tmp_path):
    runtime_dir = Path(tmp_path)
    endpoint = desktop_control_path(runtime_dir)
    runtime_dir.mkdir(mode=0o700, parents=True, exist_ok=True)
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
