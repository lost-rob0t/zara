"""Public API for externally installed Zara service plugins."""

from __future__ import annotations

import concurrent.futures
import copy
import threading
import time
from abc import ABC, abstractmethod
from dataclasses import dataclass
from types import MappingProxyType
from typing import Any, Callable, Mapping, Optional, Sequence

from langchain_core.tools import BaseTool

from zara.runtime import bridge
from zara.runtime.commands import RuntimeCommand


PLUGIN_API_VERSION = "1"
DEFAULT_EVENT_QUEUE_SIZE = 256
MAX_EVENT_QUEUE_SIZE = 4096
MAX_SUBSCRIPTIONS_PER_PLUGIN = 16


@dataclass(frozen=True)
class PluginMetadata:
    name: str
    version: str = ""
    api_version: str = PLUGIN_API_VERSION
    plugin_type: str = "service"
    description: str = ""

    def __post_init__(self) -> None:
        if not self.name or len(self.name) > 64:
            raise ValueError("plugin name must contain 1 to 64 characters")
        allowed = set("abcdefghijklmnopqrstuvwxyz0123456789._-")
        if self.name[0] not in set("abcdefghijklmnopqrstuvwxyz0123456789"):
            raise ValueError("plugin name must start with a lowercase letter or digit")
        if any(character not in allowed for character in self.name):
            raise ValueError(
                "plugin name may contain lowercase letters, digits, '.', '_' and '-'"
            )
        if len(self.version) > 64:
            raise ValueError("plugin version must not exceed 64 characters")
        if not self.api_version or len(self.api_version) > 16:
            raise ValueError("plugin API version must contain 1 to 16 characters")
        if self.plugin_type != "service":
            raise ValueError("service plugin metadata must use plugin_type='service'")
        if len(self.description) > 256:
            raise ValueError("plugin description must not exceed 256 characters")


@dataclass(frozen=True)
class RuntimeStatus:
    state: str
    alive: bool
    thread_id: Optional[int]


class ServicePlugin(ABC):
    """Lifecycle contract returned by a plugin module's ``create_plugin``."""

    metadata: PluginMetadata

    @abstractmethod
    def start(self, runtime: "PluginRuntime") -> None:
        pass

    @abstractmethod
    def stop(self) -> None:
        pass

    def tools(self) -> Sequence[BaseTool]:
        return ()


class ManagedWorker:
    """A bounded, cooperatively stopped worker owned by a plugin runtime."""

    def __init__(
        self,
        name: str,
        target: Callable[[threading.Event], None],
        failure_callback: Callable[[str], None],
    ) -> None:
        self.name = name
        self.stop_event = threading.Event()

        def run() -> None:
            try:
                target(self.stop_event)
            except Exception as error:
                failure_callback(f"managed worker {name!r} failed: {error}")

        self._thread = threading.Thread(
            target=run,
            name=f"zara-plugin-{name}",
            daemon=True,
        )

    @property
    def is_alive(self) -> bool:
        return self._thread.is_alive()

    def start(self) -> None:
        self._thread.start()

    def request_stop(self) -> None:
        self.stop_event.set()

    def join(self, timeout: Optional[float] = None) -> None:
        self._thread.join(timeout=timeout)


class PluginRuntime:
    """Narrow, thread-safe RuntimeHost facade bound to one plugin."""

    def __init__(
        self,
        *,
        plugin_name: str,
        configuration: Mapping[str, Any],
        status_provider: Callable[[], RuntimeStatus],
        dispatcher: Callable[[RuntimeCommand], concurrent.futures.Future],
        subscriber: Callable[..., bridge.RuntimeEventSubscription],
        failure_callback: Callable[[str], None],
        default_event_queue_size: int = DEFAULT_EVENT_QUEUE_SIZE,
        max_workers: int = 8,
        worker_join_timeout: float = 5.0,
    ) -> None:
        self._plugin_name = plugin_name
        self._configuration = MappingProxyType(copy.deepcopy(dict(configuration)))
        self._status_provider = status_provider
        self._dispatcher = dispatcher
        self._subscriber = subscriber
        self._failure_callback = failure_callback
        self._default_event_queue_size = default_event_queue_size
        self._max_workers = max_workers
        self._worker_join_timeout = worker_join_timeout
        self._subscriptions: set[bridge.RuntimeEventSubscription] = set()
        self._workers: dict[str, ManagedWorker] = {}
        self._closed = False
        self._lock = threading.RLock()

    @property
    def plugin_name(self) -> str:
        return self._plugin_name

    @property
    def configuration(self) -> Mapping[str, Any]:
        return self._configuration

    @property
    def status(self) -> RuntimeStatus:
        return self._status_provider()

    @property
    def closed(self) -> bool:
        with self._lock:
            return self._closed

    def dispatch(self, command: RuntimeCommand) -> concurrent.futures.Future:
        if not isinstance(command, RuntimeCommand):
            future: concurrent.futures.Future = concurrent.futures.Future()
            future.set_exception(TypeError("plugins may dispatch RuntimeCommand instances only"))
            return future
        with self._lock:
            if self._closed:
                future = concurrent.futures.Future()
                future.set_exception(RuntimeError("plugin runtime is closed"))
                return future
        return self._dispatcher(command)

    def subscribe(
        self,
        *,
        maxsize: Optional[int] = None,
    ) -> bridge.RuntimeEventSubscription:
        queue_size = self._default_event_queue_size if maxsize is None else maxsize
        if not isinstance(queue_size, int) or isinstance(queue_size, bool):
            raise TypeError("event queue size must be an integer")
        if not 1 <= queue_size <= MAX_EVENT_QUEUE_SIZE:
            raise ValueError(f"event queue size must be between 1 and {MAX_EVENT_QUEUE_SIZE}")
        with self._lock:
            if self._closed:
                raise RuntimeError("plugin runtime is closed")
            self._subscriptions = {
                subscription
                for subscription in self._subscriptions
                if not subscription.closed
            }
            if len(self._subscriptions) >= MAX_SUBSCRIPTIONS_PER_PLUGIN:
                raise RuntimeError("plugin subscription limit reached")
            subscription = self._subscriber(maxsize=queue_size)
            self._subscriptions.add(subscription)
            return subscription

    def start_worker(
        self,
        name: str,
        target: Callable[[threading.Event], None],
    ) -> ManagedWorker:
        if not name or len(name) > 64:
            raise ValueError("worker name must contain 1 to 64 characters")
        if not callable(target):
            raise TypeError("worker target must be callable")
        with self._lock:
            if self._closed:
                raise RuntimeError("plugin runtime is closed")
            if name in self._workers:
                raise ValueError(f"managed worker {name!r} is already registered")
            if len(self._workers) >= self._max_workers:
                raise RuntimeError("managed worker limit reached")
            worker = ManagedWorker(
                f"{self._plugin_name}-{name}",
                target,
                self._failure_callback,
            )
            self._workers[name] = worker
            worker.start()
            return worker

    def _shutdown(self) -> None:
        with self._lock:
            if self._closed:
                return
            self._closed = True
            subscriptions = tuple(self._subscriptions)
            workers = tuple(self._workers.values())
            self._subscriptions.clear()
            self._workers.clear()

        for subscription in subscriptions:
            subscription.close()
        for worker in workers:
            worker.request_stop()

        deadline = time.monotonic() + self._worker_join_timeout
        for worker in workers:
            worker.join(timeout=max(0.0, deadline - time.monotonic()))
            if worker.is_alive:
                self._failure_callback(
                    f"managed worker {worker.name!r} did not stop before the deadline"
                )


__all__ = [
    "DEFAULT_EVENT_QUEUE_SIZE",
    "MAX_EVENT_QUEUE_SIZE",
    "MAX_SUBSCRIPTIONS_PER_PLUGIN",
    "ManagedWorker",
    "PLUGIN_API_VERSION",
    "PluginMetadata",
    "PluginRuntime",
    "RuntimeStatus",
    "ServicePlugin",
]
