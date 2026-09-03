"""Discovery and lifecycle ownership for first-party and installed plugins."""

from __future__ import annotations

import asyncio
import enum
import inspect
import logging
import threading
from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Iterable, Optional, Sequence

from langchain_core.tools import BaseTool

from zara.runtime import events

from .api import PLUGIN_API_VERSION, PluginMetadata, PluginRuntime, RuntimeStatus
from .loader import iter_plugin_files, load_plugin_module

logger = logging.getLogger(__name__)

MAX_ERROR_LENGTH = 512


class PluginState(str, enum.Enum):
    INSTALLED = "installed"
    LOADED = "loaded"
    RUNNING = "running"
    STOPPED = "stopped"
    FAILED = "failed"
    INCOMPATIBLE = "incompatible"


@dataclass(frozen=True)
class PluginDiagnostic:
    name: str
    version: str
    plugin_type: str
    state: PluginState
    error: str = ""


@dataclass
class _PluginRecord:
    path: Path
    metadata: PluginMetadata
    instance: object
    state: PluginState = PluginState.LOADED
    error: str = ""
    runtime: Optional[PluginRuntime] = None
    start_called: bool = False
    stop_called: bool = False
    tool_names: tuple[str, ...] = ()


def _bounded_error(error: object) -> str:
    rendered = " ".join(str(error).split())
    return rendered[:MAX_ERROR_LENGTH]


class PluginManager:
    """Discover, start, diagnose, and deterministically stop plugins."""

    def __init__(
        self,
        paths: Iterable[Path | str],
        *,
        configuration_provider: Callable[[str], dict],
        status_provider: Callable[[], RuntimeStatus],
        dispatcher,
        subscriber,
        tool_registrar: Callable[[Sequence[BaseTool]], None],
        tool_unregistrar: Callable[[Sequence[str]], None],
        publisher,
        lifecycle_timeout: float = 5.0,
        event_queue_size: int = 256,
        max_workers: int = 8,
        advice_registrar=None,
        advice_unregistrar=None,
    ) -> None:
        builtin_path = Path(__file__).resolve().parent / "builtin"
        discovered_paths = [builtin_path]
        discovered_paths.extend(Path(path).expanduser() for path in paths)
        self._paths = tuple(dict.fromkeys(discovered_paths))
        self._configuration_provider = configuration_provider
        self._status_provider = status_provider
        self._dispatcher = dispatcher
        self._subscriber = subscriber
        self._tool_registrar = tool_registrar
        self._tool_unregistrar = tool_unregistrar
        self._advice_registrar = advice_registrar
        self._advice_unregistrar = advice_unregistrar
        self._publisher = publisher
        self._lifecycle_timeout = max(0.1, float(lifecycle_timeout))
        self._event_queue_size = event_queue_size
        self._max_workers = max_workers
        self._records: list[_PluginRecord] = []
        self._diagnostics: list[PluginDiagnostic] = []
        self._discovered = False
        self._started = False
        self._stopped = False
        self._lock = threading.RLock()

    def diagnostics(self) -> tuple[PluginDiagnostic, ...]:
        with self._lock:
            service_diagnostics = [
                PluginDiagnostic(
                    name=record.metadata.name,
                    version=record.metadata.version,
                    plugin_type=record.metadata.plugin_type,
                    state=record.state,
                    error=record.error,
                )
                for record in self._records
            ]
            return tuple(self._diagnostics + service_diagnostics)

    def discover(self) -> None:
        with self._lock:
            if self._discovered:
                return
            self._discovered = True

        names: set[str] = set()
        for file_path in iter_plugin_files(self._paths):
            try:
                module = load_plugin_module(file_path)
            except Exception as error:
                diagnostic = PluginDiagnostic(
                    name=file_path.stem,
                    version="",
                    plugin_type="unknown",
                    state=PluginState.FAILED,
                    error=_bounded_error(error),
                )
                self._append_diagnostic(diagnostic)
                self._publish_discovery_failure(diagnostic)
                continue

            factory = getattr(module, "create_plugin", None)
            if factory is None:
                if hasattr(module, "register_tools") or hasattr(module, "register_skills"):
                    self._append_diagnostic(
                        PluginDiagnostic(
                            name=file_path.stem,
                            version=str(getattr(module, "__version__", ""))[:64],
                            plugin_type="tool",
                            state=PluginState.LOADED,
                        )
                    )
                continue

            try:
                instance = factory()
                metadata = getattr(instance, "metadata", None)
                if not isinstance(metadata, PluginMetadata):
                    raise TypeError("service plugin metadata must be a PluginMetadata instance")

                default_enabled = getattr(instance, "enabled_by_default", True)
                if not isinstance(default_enabled, bool):
                    raise TypeError("enabled_by_default must be a boolean")
                configuration = self._configuration_provider(metadata.name)
                enabled = configuration.get("enabled", default_enabled)
                if not isinstance(enabled, bool):
                    raise TypeError("plugin enabled setting must be a boolean")
                if not enabled:
                    continue

                if metadata.name in names:
                    raise ValueError(f"duplicate service plugin name {metadata.name!r}")
                names.add(metadata.name)
                record = _PluginRecord(
                    path=file_path,
                    metadata=metadata,
                    instance=instance,
                )
                if metadata.api_version != PLUGIN_API_VERSION:
                    record.state = PluginState.INCOMPATIBLE
                    record.error = _bounded_error(
                        f"plugin API {metadata.api_version!r} is incompatible "
                        f"with {PLUGIN_API_VERSION!r}"
                    )
                with self._lock:
                    self._records.append(record)
            except Exception as error:
                diagnostic = PluginDiagnostic(
                    name=file_path.stem,
                    version="",
                    plugin_type="service",
                    state=PluginState.FAILED,
                    error=_bounded_error(error),
                )
                self._append_diagnostic(diagnostic)
                self._publish_discovery_failure(diagnostic)

    async def start(self) -> None:
        with self._lock:
            if self._started:
                return
            self._started = True
            self._stopped = False
        self.discover()

        for record in tuple(self._records):
            if record.state is PluginState.INCOMPATIBLE:
                continue
            await self._start_record(record)

    async def stop(self) -> None:
        with self._lock:
            if self._stopped:
                return
            self._stopped = True
        for record in reversed(tuple(self._records)):
            await self._stop_record(record)

    async def _start_record(self, record: _PluginRecord) -> None:
        runtime = PluginRuntime(
            plugin_name=record.metadata.name,
            configuration=self._configuration_provider(record.metadata.name),
            status_provider=self._status_provider,
            dispatcher=self._dispatcher,
            subscriber=self._subscriber,
            failure_callback=lambda message, item=record: self._runtime_failed(item, message),
            default_event_queue_size=self._event_queue_size,
            max_workers=self._max_workers,
            worker_join_timeout=self._lifecycle_timeout,
            advice_registrar=self._advice_registrar,
            advice_unregistrar=self._advice_unregistrar,
        )
        record.runtime = runtime

        try:
            tools_method = getattr(record.instance, "tools", None)
            tools = (
                tuple(await self._call_plugin(tools_method))
                if callable(tools_method)
                else ()
            )
            if any(not isinstance(tool, BaseTool) for tool in tools):
                raise TypeError("service plugin tools() must return LangChain BaseTool instances")
            if tools:
                self._tool_registrar(tools)
                record.tool_names = tuple(tool.name for tool in tools)

            start_method = getattr(record.instance, "start", None)
            if not callable(start_method):
                raise TypeError("service plugin must define start(runtime)")
            record.start_called = True
            await self._call_lifecycle(start_method, runtime)
        except Exception as error:
            self._mark_failed(record, f"startup failed: {error}")
            await self._stop_record(record, preserve_failure=True)
            return

        with self._lock:
            if record.state is not PluginState.FAILED:
                record.state = PluginState.RUNNING
                record.error = ""

    async def _stop_record(
        self,
        record: _PluginRecord,
        *,
        preserve_failure: bool = False,
    ) -> None:
        if record.stop_called or not record.start_called:
            if record.runtime is not None:
                record.runtime._shutdown()
            self._remove_tools(record)
            return
        record.stop_called = True
        previous_failure = record.error

        stop_method = getattr(record.instance, "stop", None)
        if callable(stop_method):
            try:
                await self._call_lifecycle(stop_method)
            except Exception as error:
                self._mark_failed(record, f"shutdown failed: {error}")
                preserve_failure = True
        if record.runtime is not None:
            record.runtime._shutdown()
        self._remove_tools(record)

        with self._lock:
            if not preserve_failure and record.state is not PluginState.FAILED:
                record.state = PluginState.STOPPED
                record.error = ""
            elif preserve_failure and previous_failure and not record.error:
                record.error = previous_failure

    async def _call_plugin(self, method, *args):
        if inspect.iscoroutinefunction(method):
            operation = method(*args)
        else:
            operation = asyncio.to_thread(method, *args)
        return await asyncio.wait_for(operation, timeout=self._lifecycle_timeout)

    async def _call_lifecycle(self, method, *args) -> None:
        await self._call_plugin(method, *args)

    def _remove_tools(self, record: _PluginRecord) -> None:
        if not record.tool_names:
            return
        names = record.tool_names
        record.tool_names = ()
        try:
            self._tool_unregistrar(names)
        except Exception:
            logger.debug("Could not unregister tools for %s", record.metadata.name, exc_info=True)

    def _runtime_failed(self, record: _PluginRecord, message: str) -> None:
        self._mark_failed(record, f"runtime failed: {message}")

    def _mark_failed(self, record: _PluginRecord, message: str) -> None:
        bounded = _bounded_error(message)
        with self._lock:
            record.state = PluginState.FAILED
            record.error = bounded
        logger.warning("Plugin %s failed: %s", record.metadata.name, bounded)
        try:
            self._publisher(
                events.RuntimeError(
                    reason=bounded,
                    fatal=False,
                    label=f"plugin:{record.metadata.name}",
                )
            )
        except Exception:
            logger.debug("Could not publish plugin failure", exc_info=True)

    def _append_diagnostic(self, diagnostic: PluginDiagnostic) -> None:
        with self._lock:
            self._diagnostics.append(diagnostic)

    def _publish_discovery_failure(self, diagnostic: PluginDiagnostic) -> None:
        try:
            self._publisher(
                events.RuntimeError(
                    reason=diagnostic.error,
                    fatal=False,
                    label=f"plugin:{diagnostic.name}",
                )
            )
        except Exception:
            logger.debug("Could not publish plugin discovery failure", exc_info=True)


__all__ = ["PluginDiagnostic", "PluginManager", "PluginState"]