"""Transport-neutral Zara client boundary.

The user-facing executable remains ``zara``. This module defines the client
contract that standalone callers can use today and that the ZeroMQ client in
issue #129 will implement later.
"""

from __future__ import annotations

import concurrent.futures
import enum
import time
from abc import ABC, abstractmethod
from typing import Optional

from zara.runtime import bridge
from zara.runtime.commands import RuntimeCommand
from zara.runtime.host import BackendFactory, RuntimeHost, RuntimeHostState, RuntimeNotReady


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
        self._host = RuntimeHost(
            backend_factory=backend_factory,
            publisher=self._bus.publish,
            subscriber=self._bus.subscribe,
            shutdown_timeout=self._shutdown_timeout,
            config=config,
        )

    @property
    def state(self) -> ZaraClientState:
        return _map_host_state(self._host.state)

    @property
    def is_alive(self) -> bool:
        return self._host.is_alive

    def start(self) -> concurrent.futures.Future:
        return self._host.start()

    def submit(self, command: RuntimeCommand) -> concurrent.futures.Future:
        return self._host.submit(command)

    def subscribe(self, *, maxsize: int = 0) -> bridge.RuntimeEventSubscription:
        return self._bus.subscribe(maxsize=maxsize)

    def shutdown(self, reason: str = "client shutdown") -> concurrent.futures.Future:
        return self._host.shutdown(reason=reason)

    def close(self, timeout: Optional[float] = None) -> None:
        wait_timeout = self._shutdown_timeout if timeout is None else max(0.0, float(timeout))
        deadline = time.monotonic() + wait_timeout
        future = self.shutdown()
        future.result(timeout=max(0.0, deadline - time.monotonic()))
        self._host.join(timeout=max(0.0, deadline - time.monotonic()))
        if self._host.is_alive:
            raise RuntimeNotReady("runtime host did not stop before client close timeout")


__all__ = ["InProcessZaraClient", "ZaraClient", "ZaraClientState"]
