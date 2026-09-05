"""Bounded per-user control channel for the process-owned desktop UI."""

from __future__ import annotations

import errno
import hashlib
import os
import socket
import stat
import tempfile
import threading
from pathlib import Path
from typing import Callable, Optional

_ALLOWED_COMMANDS = frozenset({"toggle", "show", "hide"})
_MAX_COMMAND_BYTES = 64
_SOCKET_NAME = "zara-desktop-control.sock"
# Linux sockaddr_un.sun_path is 108 bytes including the terminating NUL. Keep
# margin for platform differences and encoded path bytes.
_MAX_UNIX_PATH_BYTES = 100


class DesktopControlAlreadyRunning(RuntimeError):
    """Raised when another live desktop process owns the control endpoint."""


def desktop_control_path(runtime_dir: Path | str) -> Path:
    runtime = Path(runtime_dir).expanduser()
    direct = runtime / _SOCKET_NAME
    if len(os.fsencode(direct)) <= _MAX_UNIX_PATH_BYTES:
        return direct

    digest = hashlib.blake2s(
        os.fsencode(str(runtime.absolute())),
        digest_size=10,
    ).hexdigest()
    fallback_dir = Path(tempfile.gettempdir()) / f"zara-desktop-control-{os.getuid()}"
    fallback = fallback_dir / f"{digest}.sock"
    if len(os.fsencode(fallback)) > _MAX_UNIX_PATH_BYTES:
        raise OSError(errno.ENAMETOOLONG, "desktop control fallback path is too long")
    return fallback


def _prepare_runtime_dir(runtime_dir: Path) -> None:
    runtime_dir.mkdir(mode=0o700, parents=True, exist_ok=True)
    info = runtime_dir.stat()
    if info.st_uid != os.getuid():
        raise PermissionError("desktop control runtime directory is not owned by current user")
    if not stat.S_ISDIR(info.st_mode):
        raise PermissionError("desktop control runtime path is not a directory")
    os.chmod(runtime_dir, 0o700)


def _validate_private_directory(path: Path) -> None:
    info = path.stat()
    if info.st_uid != os.getuid() or not stat.S_ISDIR(info.st_mode):
        raise PermissionError("desktop control endpoint directory is not private")
    if stat.S_IMODE(info.st_mode) & 0o077:
        raise PermissionError("desktop control endpoint directory is not private")


def _validate_command(command: str) -> str:
    normalized = command.strip()
    if normalized not in _ALLOWED_COMMANDS:
        raise ValueError(f"unsupported desktop control command: {normalized!r}")
    return normalized


def _probe_existing(endpoint: Path) -> bool:
    client = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    client.settimeout(0.15)
    try:
        client.connect(str(endpoint))
        return True
    except OSError as error:
        if error.errno in {errno.ECONNREFUSED, errno.ENOENT}:
            return False
        raise
    finally:
        client.close()


def _recover_endpoint(endpoint: Path) -> None:
    try:
        info = os.lstat(endpoint)
    except FileNotFoundError:
        return
    if info.st_uid != os.getuid():
        raise PermissionError("desktop control endpoint is not owned by current user")
    if not stat.S_ISSOCK(info.st_mode):
        raise PermissionError("desktop control endpoint is not a unix socket")
    if _probe_existing(endpoint):
        raise DesktopControlAlreadyRunning("desktop control endpoint already has a live owner")
    endpoint.unlink(missing_ok=True)


class DesktopControlServer:
    """Single-owner local desktop control server with a closed command vocabulary."""

    def __init__(
        self,
        runtime_dir: Path | str,
        dispatch: Callable[[str], None],
    ) -> None:
        self._runtime_dir = Path(runtime_dir).expanduser()
        self._dispatch = dispatch
        self._socket: Optional[socket.socket] = None
        self._thread: Optional[threading.Thread] = None
        self._closed = threading.Event()
        self._owns_endpoint = False

    @property
    def endpoint(self) -> Path:
        return desktop_control_path(self._runtime_dir)

    def start(self) -> None:
        if self._socket is not None:
            return
        _prepare_runtime_dir(self._runtime_dir)
        endpoint = self.endpoint
        if endpoint.parent != self._runtime_dir:
            _prepare_runtime_dir(endpoint.parent)
        _validate_private_directory(endpoint.parent)
        _recover_endpoint(endpoint)

        owner = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        bound = False
        try:
            owner.bind(str(endpoint))
            bound = True
            os.chmod(endpoint, 0o600)
            owner.listen(8)
            owner.settimeout(0.1)
        except OSError as error:
            owner.close()
            if bound:
                endpoint.unlink(missing_ok=True)
            if error.errno == errno.EADDRINUSE:
                raise DesktopControlAlreadyRunning(
                    "desktop control endpoint already has a live owner"
                ) from error
            raise
        except Exception:
            owner.close()
            if bound:
                endpoint.unlink(missing_ok=True)
            raise

        self._closed.clear()
        self._socket = owner
        self._owns_endpoint = True
        self._thread = threading.Thread(
            target=self._serve,
            name="zara-desktop-control",
            daemon=True,
        )
        self._thread.start()

    def close(self) -> None:
        self._closed.set()
        owner = self._socket
        self._socket = None
        if owner is not None:
            owner.close()
        thread = self._thread
        self._thread = None
        if thread is not None and thread is not threading.current_thread():
            thread.join(timeout=1.0)
        if not self._owns_endpoint:
            return
        self._owns_endpoint = False
        endpoint = self.endpoint
        try:
            info = os.lstat(endpoint)
        except FileNotFoundError:
            return
        if info.st_uid == os.getuid() and stat.S_ISSOCK(info.st_mode):
            endpoint.unlink(missing_ok=True)

    def _serve(self) -> None:
        while not self._closed.is_set():
            owner = self._socket
            if owner is None:
                return
            try:
                connection, _ = owner.accept()
            except socket.timeout:
                continue
            except OSError:
                if self._closed.is_set():
                    return
                continue
            with connection:
                connection.settimeout(0.5)
                self._handle(connection)

    def _handle(self, connection: socket.socket) -> None:
        try:
            payload = connection.recv(_MAX_COMMAND_BYTES + 1)
            if not payload or len(payload) > _MAX_COMMAND_BYTES:
                connection.sendall(b"error invalid-command\n")
                return
            if b"\n" in payload:
                payload = payload.split(b"\n", 1)[0]
            command = _validate_command(payload.decode("ascii", errors="strict"))
            self._dispatch(command)
            connection.sendall(b"ok\n")
        except (UnicodeError, ValueError):
            connection.sendall(b"error invalid-command\n")
        except Exception:
            connection.sendall(b"error dispatch-failed\n")


def send_desktop_control(
    command: str,
    *,
    runtime_dir: Path | str,
    timeout: float = 0.5,
) -> str:
    """Send one bounded desktop command and return its explicit response."""

    normalized = _validate_command(command)
    endpoint = desktop_control_path(runtime_dir)
    try:
        _validate_private_directory(endpoint.parent)
        info = os.lstat(endpoint)
    except FileNotFoundError as error:
        raise ConnectionError("desktop control endpoint is not running") from error
    if info.st_uid != os.getuid() or not stat.S_ISSOCK(info.st_mode):
        raise PermissionError("desktop control endpoint is not a private owned unix socket")

    client = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    client.settimeout(max(0.05, min(float(timeout), 2.0)))
    try:
        client.connect(str(endpoint))
        client.sendall(normalized.encode("ascii") + b"\n")
        response = client.recv(64).decode("ascii", errors="strict").strip()
    finally:
        client.close()
    if response == "ok":
        return response
    raise RuntimeError(response or "desktop control returned no response")


__all__ = [
    "DesktopControlAlreadyRunning",
    "DesktopControlServer",
    "desktop_control_path",
    "send_desktop_control",
]
