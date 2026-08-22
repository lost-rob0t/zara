"""Long-lived Zara service lifecycle.

``zara-server`` owns process lifecycle and RuntimeHost instances. It also owns
the local-first ZARA/1 gateway lifecycle. Authentication and hard multi-user
persistence isolation remain ordered work in issues #130-#131.
"""

from __future__ import annotations

import argparse
import concurrent.futures
import enum
import errno
import fcntl
import hashlib
import json
import logging
import os
import signal
import stat
import sys
import tempfile
import threading
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Optional

from zara.runtime import bridge
from zara.runtime.backend import AgentRuntimeBackend
from zara.runtime.commands import RuntimeCommand
from zara.runtime.host import RuntimeHost

logger = logging.getLogger(__name__)


def _ipc_path_limit() -> int:
    import zmq

    return int(getattr(zmq, "IPC_PATH_MAX_LEN", 0) or 0)


def _validate_ipc_endpoint(endpoint: str) -> str:
    if not endpoint.startswith("ipc://"):
        raise ValueError("TCP and non-IPC endpoints require authentication from issue #130")
    path = endpoint.removeprefix("ipc://")
    if not path:
        raise ValueError("IPC endpoint path must not be empty")
    max_len = _ipc_path_limit()
    path_bytes = os.fsencode(path)
    if max_len and len(path_bytes) > max_len:
        raise ValueError(
            f"IPC endpoint path is too long ({len(path_bytes)} bytes; maximum {max_len})"
        )
    return endpoint


def _private_ipc_fallback(runtime_dir: Path) -> Path:
    source = os.fsencode(os.fspath(runtime_dir.absolute()))
    digest = hashlib.blake2s(source, digest_size=10).hexdigest()
    parent = Path("/tmp") / f"zarathushtra-{os.getuid()}"
    parent.mkdir(mode=0o700, parents=True, exist_ok=True)
    info = os.lstat(parent)
    if not stat.S_ISDIR(info.st_mode) or info.st_uid != os.getuid():
        raise ServerError(f"IPC fallback directory is not owner-private: {parent}")
    if stat.S_IMODE(info.st_mode) != 0o700:
        os.chmod(parent, 0o700)
    return parent / f"zara-server-{digest}.sock"


def default_zmq_endpoint(runtime_dir: Path | str) -> str:
    """Return a bounded owner-private local IPC endpoint for ``zara-server``."""

    runtime_path = Path(runtime_dir).expanduser()
    candidate = runtime_path / "zara-server.sock"
    max_len = _ipc_path_limit()
    if max_len and len(os.fsencode(candidate)) > max_len:
        candidate = _private_ipc_fallback(runtime_path)
    return _validate_ipc_endpoint(f"ipc://{candidate}")


class ServerError(RuntimeError):
    pass


class ServerAlreadyRunning(ServerError):
    pass


class ServerStateError(ServerError):
    pass


class PrincipalLimitExceeded(ServerError):
    pass


class PrincipalMismatch(ServerError):
    pass


class ServerState(str, enum.Enum):
    NEW = "new"
    STARTING = "starting"
    READY = "ready"
    DEGRADED = "degraded"
    STOPPING = "stopping"
    STOPPED = "stopped"
    FAILED = "failed"


@dataclass(frozen=True)
class PrincipalContext:
    principal_id: str
    kind: str = "synthetic"

    def __post_init__(self) -> None:
        if not isinstance(self.principal_id, str) or not self.principal_id.strip():
            raise ValueError("principal_id must be a non-empty string")
        if self.principal_id != self.principal_id.strip():
            raise ValueError("principal_id must not contain leading or trailing whitespace")
        if not isinstance(self.kind, str) or not self.kind.strip():
            raise ValueError("principal kind must be a non-empty string")
        if self.kind != self.kind.strip():
            raise ValueError("principal kind must not contain leading or trailing whitespace")

    @classmethod
    def local_owner(cls) -> "PrincipalContext":
        return cls(principal_id=f"uid:{os.getuid()}", kind="local-owner")


@dataclass
class PrincipalRuntime:
    principal: PrincipalContext
    host: RuntimeHost
    bus: bridge.RuntimeEventBus
    startup_error: Optional[BaseException] = None

    @property
    def healthy(self) -> bool:
        return self.startup_error is None and self.host.state.value == "running"


HostFactory = Callable[[PrincipalContext, bridge.RuntimeEventBus], RuntimeHost]
GatewayFactory = Callable[..., object]


class RuntimeSupervisor:
    """Principal-explicit lifecycle authority around RuntimeHost."""

    def __init__(
        self,
        host_factory: Optional[HostFactory] = None,
        *,
        max_active_principals: int = 1,
        shutdown_timeout: float = 5.0,
        config=None,
    ) -> None:
        if max_active_principals < 1:
            raise ValueError("max_active_principals must be at least 1")
        self._host_factory = host_factory or self._build_default_host
        self._max_active_principals = int(max_active_principals)
        self._shutdown_timeout = max(0.1, float(shutdown_timeout))
        self._config = config
        self._state = ServerState.NEW
        self._lock = threading.RLock()
        self._shutdown_lock = threading.Lock()
        self._runtimes: dict[str, PrincipalRuntime] = {}

    @property
    def state(self) -> ServerState:
        with self._lock:
            return self._state

    @property
    def principals(self) -> tuple[PrincipalContext, ...]:
        with self._lock:
            return tuple(slot.principal for slot in self._runtimes.values())

    def _build_default_host(
        self,
        principal: PrincipalContext,
        bus: bridge.RuntimeEventBus,
    ) -> RuntimeHost:
        config = self._config
        if config is None:
            from zara.config import get_config

            config = get_config()

        def manager_factory():
            from zara.agent import AgentManager

            return AgentManager(config=config, principal=principal)

        return RuntimeHost(
            backend_factory=lambda: AgentRuntimeBackend(manager_factory),
            publisher=bus.publish,
            subscriber=bus.subscribe,
            shutdown_timeout=self._shutdown_timeout,
            plugin_paths=tuple(config.get_module_search_paths()),
            config=config,
        )

    @staticmethod
    def _require_principal(principal: PrincipalContext) -> PrincipalContext:
        if not isinstance(principal, PrincipalContext):
            raise TypeError("supervisor operations require PrincipalContext")
        return principal

    def start(self, principal: PrincipalContext) -> PrincipalRuntime:
        principal = self._require_principal(principal)
        with self._lock:
            if self._state not in {ServerState.NEW, ServerState.STOPPED}:
                raise ServerStateError(f"supervisor cannot start from {self._state.value}")
            self._state = ServerState.STARTING

        try:
            slot = self._open_runtime(principal)
        except BaseException:
            with self._lock:
                self._state = ServerState.FAILED
            raise

        with self._lock:
            self._state = ServerState.READY if slot.startup_error is None else ServerState.DEGRADED
        return slot

    def open_principal(self, principal: PrincipalContext) -> PrincipalRuntime:
        principal = self._require_principal(principal)
        with self._lock:
            if self._state not in {ServerState.READY, ServerState.DEGRADED}:
                raise ServerStateError(f"supervisor is not accepting principals: {self._state.value}")
        slot = self._open_runtime(principal)
        if slot.startup_error is not None:
            with self._lock:
                self._state = ServerState.DEGRADED
        return slot

    def _open_runtime(self, principal: PrincipalContext) -> PrincipalRuntime:
        with self._lock:
            existing = self._runtimes.get(principal.principal_id)
            if existing is not None:
                if existing.principal != principal:
                    raise PrincipalMismatch(
                        f"principal id {principal.principal_id!r} already has different ownership metadata"
                    )
                return existing
            if len(self._runtimes) >= self._max_active_principals:
                raise PrincipalLimitExceeded(
                    f"active principal limit is {self._max_active_principals}; hard multi-user isolation is not enabled"
                )
            if self._state is ServerState.STOPPING:
                raise ServerStateError("supervisor is stopping")
            bus = bridge.RuntimeEventBus()
            host = self._host_factory(principal, bus)
            slot = PrincipalRuntime(principal=principal, host=host, bus=bus)
            self._runtimes[principal.principal_id] = slot

        try:
            host.start().result(timeout=self._shutdown_timeout)
        except BaseException as error:
            slot.startup_error = error
            logger.warning(
                "Runtime startup degraded for principal %s: %s",
                principal.principal_id,
                error,
            )
        return slot

    def runtime(self, principal: PrincipalContext) -> PrincipalRuntime:
        principal = self._require_principal(principal)
        with self._lock:
            slot = self._runtimes.get(principal.principal_id)
            if slot is None:
                raise KeyError(principal.principal_id)
            if slot.principal != principal:
                raise PrincipalMismatch(principal.principal_id)
            return slot

    def submit(
        self,
        principal: PrincipalContext,
        command: RuntimeCommand,
    ) -> concurrent.futures.Future:
        return self.runtime(principal).host.submit(command)

    def subscribe(
        self,
        principal: PrincipalContext,
        *,
        maxsize: int = 0,
    ) -> bridge.RuntimeEventSubscription:
        return self.runtime(principal).bus.subscribe(maxsize=maxsize)

    def shutdown(self) -> bool:
        with self._shutdown_lock:
            with self._lock:
                if self._state in {ServerState.NEW, ServerState.STOPPED}:
                    self._state = ServerState.STOPPED
                    return True
                if self._state is ServerState.FAILED and not self._runtimes:
                    return False
                self._state = ServerState.STOPPING
                slots = tuple(self._runtimes.values())

            deadline = time.monotonic() + self._shutdown_timeout
            clean = True
            futures: list[tuple[PrincipalRuntime, concurrent.futures.Future]] = []

            for slot in slots:
                try:
                    futures.append(
                        (slot, slot.host.shutdown(reason="zara-server shutdown"))
                    )
                except BaseException:
                    logger.exception(
                        "Failed to request runtime shutdown for %s",
                        slot.principal.principal_id,
                    )
                    clean = False

            for slot, future in futures:
                remaining = max(0.0, deadline - time.monotonic())
                try:
                    future.result(timeout=remaining)
                except BaseException:
                    logger.exception(
                        "Runtime shutdown failed for %s",
                        slot.principal.principal_id,
                    )
                    clean = False

            for slot in slots:
                remaining = max(0.0, deadline - time.monotonic())
                slot.host.join(timeout=remaining)
                if slot.host.is_alive:
                    logger.error(
                        "Runtime host remained alive after shutdown deadline for %s",
                        slot.principal.principal_id,
                    )
                    clean = False

            with self._lock:
                self._runtimes.clear()
                self._state = ServerState.STOPPED if clean else ServerState.FAILED
            return clean


class ServerLease:
    """Single-process ownership held by a local advisory file lock."""

    def __init__(self, runtime_dir: Optional[Path | str] = None) -> None:
        self._runtime_dir_override = None if runtime_dir is None else Path(runtime_dir)
        self._fd: Optional[int] = None
        self._path: Optional[Path] = None

    @property
    def path(self) -> Optional[Path]:
        return self._path

    @property
    def held(self) -> bool:
        return self._fd is not None

    def _runtime_dir(self) -> Path:
        if self._runtime_dir_override is not None:
            return self._runtime_dir_override.expanduser()

        xdg_runtime = os.environ.get("XDG_RUNTIME_DIR", "").strip()
        if xdg_runtime and Path(xdg_runtime).is_absolute():
            return Path(xdg_runtime) / "zarathushtra"

        fallback = Path(tempfile.gettempdir()) / f"zarathushtra-{os.getuid()}"
        logger.warning(
            "XDG_RUNTIME_DIR is unavailable; using UID-scoped fallback %s",
            fallback,
        )
        return fallback

    @staticmethod
    def _prepare_directory(path: Path) -> None:
        path.mkdir(mode=0o700, parents=True, exist_ok=True)
        info = os.lstat(path)
        if not stat.S_ISDIR(info.st_mode):
            raise ServerError(f"runtime path is not a directory: {path}")
        if info.st_uid != os.getuid():
            raise ServerError(f"runtime directory is not owned by current user: {path}")
        if stat.S_IMODE(info.st_mode) != 0o700:
            os.chmod(path, 0o700)

    def acquire(self) -> Path:
        if self._fd is not None:
            return self._path

        runtime_dir = self._runtime_dir()
        self._prepare_directory(runtime_dir)
        path = runtime_dir / "zara-server.lock"
        flags = os.O_RDWR | os.O_CREAT | os.O_CLOEXEC
        if hasattr(os, "O_NOFOLLOW"):
            flags |= os.O_NOFOLLOW
        fd = os.open(path, flags, 0o600)
        try:
            os.fchmod(fd, 0o600)
            try:
                fcntl.flock(fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
            except OSError as error:
                if error.errno in {errno.EACCES, errno.EAGAIN}:
                    raise ServerAlreadyRunning(
                        f"another zara-server owns {path}"
                    ) from error
                raise

            metadata = {
                "pid": os.getpid(),
                "started_ns": time.time_ns(),
                "executable": "zara-server",
            }
            payload = (json.dumps(metadata, sort_keys=True) + "\n").encode("utf-8")
            os.ftruncate(fd, 0)
            os.lseek(fd, 0, os.SEEK_SET)
            os.write(fd, payload)
        except BaseException:
            os.close(fd)
            raise

        self._fd = fd
        self._path = path
        return path

    def release(self) -> None:
        fd = self._fd
        self._fd = None
        if fd is None:
            return
        try:
            fcntl.flock(fd, fcntl.LOCK_UN)
        finally:
            os.close(fd)


class ZaraServer:
    """Foreground service process owning RuntimeSupervisor and ZARA/1 gateway."""

    def __init__(
        self,
        *,
        supervisor: Optional[RuntimeSupervisor] = None,
        lease: Optional[ServerLease] = None,
        runtime_dir: Optional[Path | str] = None,
        endpoint: Optional[str] = None,
        gateway_factory: Optional[GatewayFactory] = None,
        shutdown_timeout: float = 5.0,
        principal: Optional[PrincipalContext] = None,
        config=None,
    ) -> None:
        if endpoint is not None:
            if not isinstance(endpoint, str) or not endpoint.strip():
                raise ValueError("endpoint must be a non-empty string")
            endpoint = _validate_ipc_endpoint(endpoint)
        self._shutdown_timeout = max(0.1, float(shutdown_timeout))
        self._supervisor = supervisor or RuntimeSupervisor(
            shutdown_timeout=self._shutdown_timeout,
            max_active_principals=1,
            config=config,
        )
        self._lease = lease or ServerLease(runtime_dir)
        self._runtime_dir_override = None if runtime_dir is None else Path(runtime_dir).expanduser()
        self._endpoint_override = endpoint
        self._gateway_factory = gateway_factory or self._build_default_gateway
        self._gateway = None
        self._principal = principal or PrincipalContext.local_owner()
        self._state = ServerState.NEW
        self._lock = threading.RLock()

    def _build_default_gateway(self, endpoint: str, *, supervisor, principal):
        from zara.zmq_transport import ZaraZmqGateway

        return ZaraZmqGateway(
            endpoint,
            supervisor=supervisor,
            principal=principal,
        )

    @property
    def state(self) -> ServerState:
        with self._lock:
            return self._state

    @property
    def principal(self) -> PrincipalContext:
        return self._principal

    @property
    def supervisor(self) -> RuntimeSupervisor:
        return self._supervisor

    def _resolve_endpoint(self, lease_path: Path) -> str:
        if self._endpoint_override is not None:
            return self._endpoint_override
        runtime_dir = self._runtime_dir_override or lease_path.parent
        return default_zmq_endpoint(runtime_dir)

    def start(self) -> ServerState:
        with self._lock:
            if self._state in {ServerState.READY, ServerState.DEGRADED}:
                return self._state
            if self._state not in {ServerState.NEW, ServerState.STOPPED}:
                raise ServerStateError(f"server cannot start from {self._state.value}")
            self._state = ServerState.STARTING

        supervisor_started = False
        gateway = None
        try:
            lease_path = Path(self._lease.acquire())
            self._supervisor.start(self._principal)
            supervisor_started = True
            endpoint = self._resolve_endpoint(lease_path)
            gateway = self._gateway_factory(
                endpoint,
                supervisor=self._supervisor,
                principal=self._principal,
            )
            self._gateway = gateway
            gateway.start().result(timeout=self._shutdown_timeout)
            supervisor_state = self._supervisor.state
            with self._lock:
                self._state = (
                    ServerState.READY
                    if supervisor_state is ServerState.READY
                    else ServerState.DEGRADED
                )
                return self._state
        except BaseException:
            if gateway is not None:
                try:
                    gateway.close(timeout=self._shutdown_timeout)
                except BaseException:
                    logger.exception("Failed to close ZARA/1 gateway after startup failure")
            self._gateway = None
            if supervisor_started:
                try:
                    self._supervisor.shutdown()
                except BaseException:
                    logger.exception("Failed to rollback runtime after gateway startup failure")
            self._lease.release()
            with self._lock:
                self._state = ServerState.FAILED
            raise

    def stop(self) -> bool:
        with self._lock:
            if self._state is ServerState.STOPPED:
                return True
            if self._state is ServerState.NEW:
                self._state = ServerState.STOPPED
                self._lease.release()
                return True
            self._state = ServerState.STOPPING

        clean = True
        gateway = self._gateway
        self._gateway = None
        try:
            if gateway is not None:
                try:
                    gateway.close(timeout=self._shutdown_timeout)
                except BaseException:
                    logger.exception("Failed to stop ZARA/1 gateway cleanly")
                    clean = False
            try:
                clean = self._supervisor.shutdown() and clean
            except BaseException:
                logger.exception("Failed to stop runtime supervisor cleanly")
                clean = False
            return clean
        finally:
            self._lease.release()
            with self._lock:
                self._state = ServerState.STOPPED if clean else ServerState.FAILED

    def run(self, stop_event: threading.Event) -> int:
        self.start()
        if self.state is ServerState.DEGRADED:
            logger.warning("zara-server started in degraded state")
        try:
            stop_event.wait()
        finally:
            clean = self.stop()
        return 0 if clean else 1


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="zara-server",
        description=(
            "Long-lived Zara assistant service. ZARA/1 is local IPC only until "
            "transport authentication lands in issue #130."
        ),
    )
    parser.add_argument(
        "--runtime-dir",
        help="Override the owner-private runtime directory used for the server lease",
    )
    parser.add_argument(
        "--endpoint",
        help="Override the local ipc:// ZARA/1 endpoint; TCP is disabled until authentication",
    )
    parser.add_argument(
        "--shutdown-timeout",
        type=float,
        default=5.0,
        help="Maximum seconds allowed for runtime/gateway drain and join (default: 5)",
    )
    parser.add_argument("-v", "--verbose", action="store_true")
    return parser


def main(argv: Optional[list[str]] = None) -> int:
    args = _parser().parse_args(argv)
    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="%(asctime)s %(levelname)s %(name)s: %(message)s",
    )

    stop_event = threading.Event()

    def request_stop(_signum, _frame) -> None:
        stop_event.set()

    signal.signal(signal.SIGINT, request_stop)
    signal.signal(signal.SIGTERM, request_stop)

    server = ZaraServer(
        runtime_dir=args.runtime_dir,
        endpoint=args.endpoint,
        shutdown_timeout=args.shutdown_timeout,
    )
    try:
        return server.run(stop_event)
    except ServerAlreadyRunning as error:
        print(str(error), file=sys.stderr)
        return 2
    except KeyboardInterrupt:
        stop_event.set()
        return 0 if server.stop() else 1
    except BaseException:
        logger.exception("zara-server failed")
        server.stop()
        return 1


if __name__ == "__main__":
    raise SystemExit(main())


__all__ = [
    "GatewayFactory",
    "PrincipalContext",
    "PrincipalLimitExceeded",
    "PrincipalMismatch",
    "PrincipalRuntime",
    "RuntimeSupervisor",
    "ServerAlreadyRunning",
    "ServerError",
    "ServerLease",
    "ServerState",
    "ServerStateError",
    "ZaraServer",
    "default_zmq_endpoint",
    "main",
]
