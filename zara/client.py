"""Transport-neutral Zara client boundary.

The user-facing executable remains ``zara``. This module defines the client
contract that standalone callers can use today and that the ZeroMQ client in
issue #129 will implement later.
"""

from __future__ import annotations

import concurrent.futures
import enum
import threading
import time
from abc import ABC, abstractmethod
from typing import Optional

from zara.runtime import bridge
from zara.runtime.commands import RuntimeCommand
from zara.runtime.host import BackendFactory, RuntimeHost, RuntimeHostState, RuntimeNotReady


_RECONNECT_LOCK_INIT = threading.Lock()


class ZaraClientState(str, enum.Enum):
    NEW = "new"
    STARTING = "starting"
    READY = "ready"
    DEGRADED = "degraded"
    STOPPING = "stopping"
    STOPPED = "stopped"
    FAILED = "failed"


def _map_host_state(state: RuntimeHostState) -> ZaraClientState:
    mapping = {
        RuntimeHostState.NEW: ZaraClientState.NEW,
        RuntimeHostState.STARTING: ZaraClientState.STARTING,
        RuntimeHostState.RUNNING: ZaraClientState.READY,
        RuntimeHostState.DEGRADED: ZaraClientState.DEGRADED,
        RuntimeHostState.STOPPING: ZaraClientState.STOPPING,
        RuntimeHostState.STOPPED: ZaraClientState.STOPPED,
        RuntimeHostState.FAILED: ZaraClientState.FAILED,
    }
    return mapping[state]


class ZaraClient(ABC):
    """Application-facing client contract independent of transport."""

    @property
    @abstractmethod
    def state(self) -> ZaraClientState:
        raise NotImplementedError

    @abstractmethod
    def start(self) -> concurrent.futures.Future:
        raise NotImplementedError

    @abstractmethod
    def submit(self, command: RuntimeCommand) -> concurrent.futures.Future:
        raise NotImplementedError

    @abstractmethod
    def subscribe(self, *, maxsize: int = 0) -> bridge.RuntimeEventSubscription:
        raise NotImplementedError

    @abstractmethod
    def shutdown(self, reason: str = "client shutdown") -> concurrent.futures.Future:
        raise NotImplementedError

    @abstractmethod
    def close(self, timeout: Optional[float] = None) -> None:
        raise NotImplementedError

    def reconnect(self) -> concurrent.futures.Future:
        """Reconnect this client if the concrete transport supports it."""
        future = concurrent.futures.Future()
        future.set_exception(NotImplementedError("client does not support reconnect"))
        return future

    def _reconnect_controller_lock(self) -> threading.Lock:
        lock = getattr(self, "_reconnect_backoff_lock", None)
        if lock is not None:
            return lock
        with _RECONNECT_LOCK_INIT:
            lock = getattr(self, "_reconnect_backoff_lock", None)
            if lock is None:
                lock = threading.Lock()
                self._reconnect_backoff_lock = lock
        return lock

    def reconnect_with_backoff(
        self,
        *,
        max_attempts: int = 4,
        initial_delay: float = 0.1,
        max_delay: float = 1.0,
        sleeper=time.sleep,
    ) -> concurrent.futures.Future:
        """Reconnect asynchronously with one bounded in-flight retry generation."""
        if type(max_attempts) is not int or max_attempts <= 0:
            raise ValueError("max_attempts must be a positive integer")
        if not isinstance(initial_delay, (int, float)) or isinstance(initial_delay, bool):
            raise TypeError("initial_delay must be a number")
        if not isinstance(max_delay, (int, float)) or isinstance(max_delay, bool):
            raise TypeError("max_delay must be a number")
        if initial_delay < 0 or max_delay < 0:
            raise ValueError("reconnect delays must be non-negative")
        if not callable(sleeper):
            raise TypeError("sleeper must be callable")

        lock = self._reconnect_controller_lock()
        with lock:
            active = getattr(self, "_reconnect_backoff_future", None)
            if active is not None and not active.done():
                return active
            result = concurrent.futures.Future()
            self._reconnect_backoff_future = result

        def run() -> None:
            try:
                delay = min(float(initial_delay), float(max_delay))
                for attempt in range(max_attempts):
                    try:
                        reconnect_future = self.reconnect()
                        reconnect_future.result()
                    except BaseException as error:
                        if attempt + 1 == max_attempts:
                            result.set_exception(error)
                            return
                        sleeper(delay)
                        delay = min(delay * 2, float(max_delay))
                        continue
                    result.set_result(True)
                    return
            finally:
                with lock:
                    if getattr(self, "_reconnect_backoff_future", None) is result:
                        self._reconnect_backoff_future = None

        threading.Thread(
            target=run,
            name="zara-client-reconnect",
            daemon=True,
        ).start()
        return result


class InProcessZaraClient(ZaraClient):
    """Standalone ZaraClient backed by one private RuntimeHost."""

    def __init__(
        self,
        backend_factory: Optional[BackendFactory] = None,
        *,
        shutdown_timeout: float = 5.0,
        config=None,
    ) -> None:
        self._bus = bridge.RuntimeEventBus()
        self._shutdown_timeout = max(0.1, float(shutdown_timeout))

        resolved_config = config
        resolved_backend_factory = backend_factory
        plugin_paths = None
        if resolved_backend_factory is None:
            if resolved_config is None:
                from zara.config import get_config

                resolved_config = get_config()

            from zara.runtime.backend import AgentRuntimeBackend

            resolved_backend_factory = lambda: AgentRuntimeBackend(config=resolved_config)
            plugin_paths = tuple(resolved_config.get_module_search_paths())

        self._host = RuntimeHost(
            backend_factory=resolved_backend_factory,
            publisher=self._bus.publish,
            subscriber=self._bus.subscribe,
            shutdown_timeout=self._shutdown_timeout,
            plugin_paths=plugin_paths,
            config=resolved_config,
        )
        self._closed = False

    @property
    def state(self) -> ZaraClientState:
        if self._closed:
            return ZaraClientState.STOPPED
        return _map_host_state(self._host.state)

    @property
    def is_alive(self) -> bool:
        return self._host.is_alive

    def start(self) -> concurrent.futures.Future:
        self._closed = False
        return self._host.start()

    def submit(self, command: RuntimeCommand) -> concurrent.futures.Future:
        return self._host.submit(command)

    def subscribe(self, *, maxsize: int = 0) -> bridge.RuntimeEventSubscription:
        return self._bus.subscribe(maxsize=maxsize)

    def shutdown(self, reason: str = "client shutdown") -> concurrent.futures.Future:
        return self._host.shutdown(reason=reason)

    def close(self, timeout: Optional[float] = None) -> None:
        if self._closed and not self._host.is_alive:
            return

        wait_timeout = self._shutdown_timeout if timeout is None else max(0.0, float(timeout))
        deadline = time.monotonic() + wait_timeout
        future = self.shutdown()
        future.result(timeout=max(0.0, deadline - time.monotonic()))
        self._host.join(timeout=max(0.0, deadline - time.monotonic()))
        if self._host.is_alive:
            raise RuntimeNotReady("runtime host did not stop before client close timeout")
        self._closed = True


__all__ = ["InProcessZaraClient", "ZaraClient", "ZaraClientState"]
