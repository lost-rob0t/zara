"""Owner-local live security administration for the running Zara daemon."""

from __future__ import annotations

import json
import logging
import os
import socket
import stat
import struct
import threading
from pathlib import Path
from typing import Iterable

from zara.principals import PrincipalContext
from zara.security import Capability, SecurityError, SecurityRegistry
from zara.security_state import PersistentSecurityState, SecurityStateError

logger = logging.getLogger(__name__)

_PROTOCOL_VERSION = 1
_MAX_REQUEST_BYTES = 16 * 1024
_MAX_RESPONSE_BYTES = 64 * 1024
_SOCKET_TIMEOUT = 1.0
_UNIX_PATH_SAFE_BYTES = 100


class SecurityAdminError(RuntimeError):
    pass


def _encode_message(payload: object, *, limit: int) -> bytes:
    encoded = (json.dumps(payload, sort_keys=True, separators=(",", ":")) + "\n").encode(
        "utf-8"
    )
    if len(encoded) > limit:
        raise SecurityAdminError("security admin message exceeds byte limit")
    return encoded


def _recv_message(connection: socket.socket, *, limit: int) -> object:
    chunks = bytearray()
    while True:
        block = connection.recv(min(4096, limit + 1 - len(chunks)))
        if not block:
            raise SecurityAdminError("security admin connection closed before message completion")
        chunks.extend(block)
        if len(chunks) > limit:
            raise SecurityAdminError("security admin message exceeds byte limit")
        newline = chunks.find(b"\n")
        if newline >= 0:
            if bytes(chunks[newline + 1 :]).strip():
                raise SecurityAdminError("security admin accepts one request per connection")
            raw = bytes(chunks[:newline])
            break
    try:
        return json.loads(raw.decode("utf-8"))
    except (UnicodeError, json.JSONDecodeError) as error:
        raise SecurityAdminError("security admin request is invalid JSON") from error


def _socket_info(path: Path):
    try:
        info = os.lstat(path)
    except FileNotFoundError:
        return None
    if stat.S_ISLNK(info.st_mode):
        raise SecurityAdminError("security admin socket must not be a symlink")
    if not stat.S_ISSOCK(info.st_mode):
        raise SecurityAdminError("security admin path is not a Unix socket")
    if info.st_uid != os.getuid():
        raise SecurityAdminError("security admin socket is not owner-owned")
    return info


def _open_socket_address(path: Path) -> tuple[int | None, str]:
    """Return a bind/connect address without moving the socket out of its directory."""
    direct = os.fspath(path)
    if len(os.fsencode(direct)) <= _UNIX_PATH_SAFE_BYTES:
        return None, direct
    proc_fd = Path("/proc/self/fd")
    if not proc_fd.is_dir():
        raise SecurityAdminError("security admin path exceeds AF_UNIX limit and /proc/self/fd is unavailable")
    flags = os.O_RDONLY | getattr(os, "O_DIRECTORY", 0) | getattr(os, "O_CLOEXEC", 0)
    flags |= getattr(os, "O_NOFOLLOW", 0)
    try:
        directory_fd = os.open(path.parent, flags)
    except OSError as error:
        raise SecurityAdminError("security admin directory cannot be opened safely") from error
    address = f"/proc/self/fd/{directory_fd}/{path.name}"
    if len(os.fsencode(address)) > _UNIX_PATH_SAFE_BYTES:
        os.close(directory_fd)
        raise SecurityAdminError("security admin socket name exceeds AF_UNIX limit")
    return directory_fd, address


def _connect_socket(connection: socket.socket, path: Path) -> None:
    directory_fd, address = _open_socket_address(path)
    try:
        connection.connect(address)
    finally:
        if directory_fd is not None:
            os.close(directory_fd)


def _bind_socket(listener: socket.socket, path: Path) -> None:
    directory_fd, address = _open_socket_address(path)
    try:
        listener.bind(address)
    finally:
        if directory_fd is not None:
            os.close(directory_fd)


class SecurityAdminServer:
    """Bounded AF_UNIX control plane that mutates one live SecurityRegistry."""

    def __init__(
        self,
        state: PersistentSecurityState,
        *,
        capabilities: Iterable[Capability],
    ) -> None:
        if not isinstance(state, PersistentSecurityState):
            raise TypeError("state must be PersistentSecurityState")
        normalized: set[Capability] = set()
        for capability in capabilities:
            if not isinstance(capability, Capability):
                raise TypeError("capabilities must contain Capability values")
            normalized.add(capability)
        self._state = state
        self._capabilities = frozenset(normalized)
        self._registry: SecurityRegistry | None = None
        self._registry_lock = threading.RLock()
        self._listener: socket.socket | None = None
        self._thread: threading.Thread | None = None
        self._stop = threading.Event()

    @property
    def path(self) -> Path:
        return self._state.control_socket_path

    def bind_registry(self, registry: SecurityRegistry) -> None:
        if not isinstance(registry, SecurityRegistry):
            raise TypeError("registry must be SecurityRegistry")
        with self._registry_lock:
            if self._registry is not None and self._registry is not registry:
                raise SecurityAdminError("security admin registry is already bound")
            self._registry = registry

    def start(self) -> None:
        if self._listener is not None:
            return
        if getattr(socket, "SO_PEERCRED", None) is None:
            raise SecurityAdminError("owner peer credential checks are unavailable")
        self._state.prepare_directory()
        path = self.path
        info = _socket_info(path)
        if info is not None:
            probe = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            probe.settimeout(0.1)
            try:
                _connect_socket(probe, path)
            except (ConnectionRefusedError, FileNotFoundError):
                path.unlink()
            except OSError as error:
                raise SecurityAdminError("existing security admin socket is not safely reusable") from error
            else:
                raise SecurityAdminError("security admin endpoint is already active")
            finally:
                probe.close()

        listener = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        try:
            _bind_socket(listener, path)
            os.chmod(path, 0o600)
            listener.listen(8)
            listener.settimeout(0.1)
        except BaseException:
            listener.close()
            if path.exists():
                try:
                    path.unlink()
                except OSError:
                    pass
            raise

        self._stop.clear()
        self._listener = listener
        self._thread = threading.Thread(
            target=self._run,
            name="zara-security-admin",
            daemon=True,
        )
        self._thread.start()

    def close(self, *, timeout: float = 1.0) -> None:
        self._stop.set()
        listener = self._listener
        self._listener = None
        if listener is not None:
            listener.close()
        thread = self._thread
        self._thread = None
        if thread is not None:
            thread.join(timeout=max(0.0, float(timeout)))
            if thread.is_alive():
                raise SecurityAdminError("security admin thread did not stop")
        info = _socket_info(self.path)
        if info is not None:
            self.path.unlink()

    def _run(self) -> None:
        listener = self._listener
        if listener is None:
            return
        while not self._stop.is_set():
            try:
                connection, _address = listener.accept()
            except socket.timeout:
                continue
            except OSError:
                if self._stop.is_set():
                    break
                logger.exception("security admin accept failed")
                continue
            with connection:
                try:
                    connection.settimeout(_SOCKET_TIMEOUT)
                    self._require_owner_peer(connection)
                    request = _recv_message(connection, limit=_MAX_REQUEST_BYTES)
                    result = self._dispatch(request)
                    response = {"ok": True, "result": result}
                except (SecurityAdminError, SecurityError, SecurityStateError, TypeError, ValueError) as error:
                    response = {"ok": False, "error": str(error)}
                except BaseException:
                    logger.exception("security admin request failed")
                    response = {"ok": False, "error": "internal security admin error"}
                try:
                    connection.sendall(_encode_message(response, limit=_MAX_RESPONSE_BYTES))
                except (OSError, SecurityAdminError):
                    pass

    @staticmethod
    def _require_owner_peer(connection: socket.socket) -> None:
        option = getattr(socket, "SO_PEERCRED", None)
        if option is None:
            raise SecurityAdminError("owner peer credential checks are unavailable")
        raw = connection.getsockopt(socket.SOL_SOCKET, option, struct.calcsize("3i"))
        _pid, uid, _gid = struct.unpack("3i", raw)
        if uid != os.getuid():
            raise SecurityAdminError("security admin peer is not the daemon owner")

    def _live_registry(self) -> SecurityRegistry:
        with self._registry_lock:
            registry = self._registry
        if registry is None:
            raise SecurityAdminError("security admin authority is still starting")
        return registry

    def _dispatch(self, request: object) -> object:
        if not isinstance(request, dict):
            raise SecurityAdminError("security admin request must be an object")
        version = request.get("version")
        action = request.get("action")
        if version != _PROTOCOL_VERSION:
            raise SecurityAdminError("unsupported security admin protocol version")
        if action == "enroll":
            if set(request) != {"version", "action", "public_key", "device_id"}:
                raise SecurityAdminError("security enroll request has invalid fields")
            public_key = request.get("public_key")
            device_id = request.get("device_id")
            if not isinstance(public_key, str) or not isinstance(device_id, str):
                raise SecurityAdminError("security enroll request has invalid values")
            enrolled = self._state.enroll_client(
                public_key,
                device_id=device_id,
                principal=PrincipalContext.local_owner(),
                capabilities=self._capabilities,
                live_registry=self._live_registry(),
            )
            return {
                "device_id": enrolled.device_id,
                "principal_id": enrolled.principal.principal_id,
                "public_key": enrolled.public_key,
                "capabilities": sorted(capability.value for capability in enrolled.capabilities),
                "active": enrolled.active,
            }
        if action == "revoke":
            if set(request) != {"version", "action", "device_id"}:
                raise SecurityAdminError("security revoke request has invalid fields")
            device_id = request.get("device_id")
            if not isinstance(device_id, str):
                raise SecurityAdminError("security revoke request has invalid values")
            revoked = self._state.revoke_device(
                device_id,
                live_registry=self._live_registry(),
            )
            return {"device_id": revoked.device_id, "active": False}
        if action == "list":
            if set(request) != {"version", "action"}:
                raise SecurityAdminError("security list request has invalid fields")
            self._live_registry()
            return list(self._state.list_clients())
        raise SecurityAdminError("unknown security admin action")


class SecurityAdminClient:
    """Owner-side client for the running daemon's live security authority."""

    def __init__(self, path: Path | str) -> None:
        self._path = Path(path)

    def request(self, action: str, **fields: object) -> object:
        info = _socket_info(self._path)
        if info is None:
            raise SecurityAdminError("security admin endpoint is not running")
        if stat.S_IMODE(info.st_mode) != 0o600:
            raise SecurityAdminError("security admin socket permissions are not owner-only")
        payload = {"version": _PROTOCOL_VERSION, "action": action, **fields}
        encoded = _encode_message(payload, limit=_MAX_REQUEST_BYTES)
        connection = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        connection.settimeout(_SOCKET_TIMEOUT)
        try:
            _connect_socket(connection, self._path)
            connection.sendall(encoded)
            response = _recv_message(connection, limit=_MAX_RESPONSE_BYTES)
        except OSError as error:
            raise SecurityAdminError("security admin endpoint is unreachable") from error
        finally:
            connection.close()
        if not isinstance(response, dict) or set(response) not in (
            {"ok", "result"},
            {"ok", "error"},
        ):
            raise SecurityAdminError("security admin response is malformed")
        if response.get("ok") is not True:
            error = response.get("error")
            if not isinstance(error, str) or not error:
                error = "security admin request failed"
            raise SecurityAdminError(error)
        return response.get("result")


__all__ = ["SecurityAdminClient", "SecurityAdminError", "SecurityAdminServer"]
