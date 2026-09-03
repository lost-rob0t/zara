"""Dedicated-thread lifecycle and command boundary for the Zara runtime."""

from __future__ import annotations

import asyncio
import concurrent.futures
import enum
import logging
import threading
from pathlib import Path
from typing import Callable, Iterable, Optional

from zara.actors import (
    CancelTurn as ActorCancelTurn,
    Drain,
    Event as ActorEvent,
    StartTurn,
    TurnCancelledReply,
    TurnCoordinator,
    TurnStartedReply,
)
from zara.latency import JSONLMetricsSink, LatencyTrace, metrics_path
from zara.plugins.api import RuntimeStatus
from zara.plugins.manager import PluginDiagnostic, PluginManager

from . import bridge, events
from .backend import AgentRuntimeBackend, RuntimeBackend, RuntimeTurnResult, UnsupportedRuntimeCommand
from .clarification import ClarificationCoordinator, SessionCloseReason
from .commands import (
    ApproveTool,
    CancelTurn,
    CommandReceipt,
    MuteSpeech,
    RejectTool,
    RestartRuntime,
    RuntimeCommand,
    ShutdownRuntime,
    StartVoice,
    StopVoice,
    SubmitTurn,
)

logger = logging.getLogger(__name__)


class RuntimeHostError(RuntimeError):
    pass


class RuntimeNotReady(RuntimeHostError):
    pass


class RuntimeHostState(str, enum.Enum):
    NEW = "new"
    STARTING = "starting"
    RUNNING = "running"
    DEGRADED = "degraded"
    STOPPING = "stopping"
    STOPPED = "stopped"
    FAILED = "failed"


BackendFactory = Callable[[], RuntimeBackend]
EventPublisher = Callable[[events.RuntimeEvent], bridge.EventEnvelope]
EventSubscriber = Callable[..., bridge.RuntimeEventSubscription]


def _completed_future(value=None) -> concurrent.futures.Future:
    future: concurrent.futures.Future = concurrent.futures.Future()
    future.set_result(value)
    return future


def _failed_future(error: BaseException) -> concurrent.futures.Future:
    future: concurrent.futures.Future = concurrent.futures.Future()
    future.set_exception(error)
    return future


class RuntimeHost:
    """Own Zara application services on a dedicated asyncio worker thread.

    ``RuntimeHost`` itself is Qt-neutral. Public methods are thread-safe and
    return ``concurrent.futures.Future`` objects so a desktop adapter can react
    asynchronously without blocking the GUI thread.
    """

    def __init__(
        self,
        backend_factory: Optional[BackendFactory] = None,
        *,
        publisher: EventPublisher = bridge.publish,
        subscriber: EventSubscriber = bridge.subscribe,
        shutdown_timeout: float = 5.0,
        plugin_paths: Optional[Iterable[Path | str]] = None,
        config=None,
        task_store=None,
    ) -> None:
        self._backend_factory = backend_factory or (lambda: AgentRuntimeBackend())
        self._publisher = publisher
        self._subscriber = subscriber
        self._shutdown_timeout = max(0.1, float(shutdown_timeout))
        self._manage_plugins = backend_factory is None or plugin_paths is not None
        self._plugin_paths = None if plugin_paths is None else tuple(plugin_paths)
        self._config = config
        self._task_store = task_store

        self._state_lock = threading.RLock()
        self._state = RuntimeHostState.NEW
        self._thread: Optional[threading.Thread] = None
        self._thread_id: Optional[int] = None
        self._loop: Optional[asyncio.AbstractEventLoop] = None
        self._async_stop: Optional[asyncio.Event] = None
        self._startup_future: concurrent.futures.Future = concurrent.futures.Future()
        self._startup_shutdown_requests: list[
            tuple[ShutdownRuntime, concurrent.futures.Future]
        ] = []

        self._backend: Optional[RuntimeBackend] = None
        self._coordinator = None
        self._turn_tasks: dict[str, asyncio.Task] = {}
        self._clarifications = ClarificationCoordinator()
        self._plugin_manager: Optional[PluginManager] = None
        self._last_plugin_diagnostics: tuple[PluginDiagnostic, ...] = ()
        self._api_service = None
        self._task_runner = None

    @property
    def state(self) -> RuntimeHostState:
        with self._state_lock:
            return self._state

    @property
    def clarifications(self) -> ClarificationCoordinator:
        return self._clarifications

    @property
    def thread_id(self) -> Optional[int]:
        return self._thread_id

    @property
    def is_alive(self) -> bool:
        thread = self._thread
        return bool(thread and thread.is_alive())

    def plugin_diagnostics(self) -> tuple[PluginDiagnostic, ...]:
        manager = self._plugin_manager
        if manager is not None:
            return manager.diagnostics()
        return self._last_plugin_diagnostics

    @property
    def plan_service(self):
        """The api_service plan execution service, or None when disabled."""
        return self._api_service

    @property
    def task_runner(self):
        """The long-horizon task runner, or None when disabled."""
        return self._task_runner

    def run_coroutine(self, coroutine) -> concurrent.futures.Future:
        """Schedule one coroutine on the runtime loop from any thread."""
        future: concurrent.futures.Future = concurrent.futures.Future()
        with self._state_lock:
            state = self._state
            loop = self._loop
        if state is not RuntimeHostState.RUNNING or loop is None or loop.is_closed():
            future.set_exception(RuntimeNotReady(f"runtime is not ready: {state.value}"))
            return future
        try:
            return asyncio.run_coroutine_threadsafe(coroutine, loop)
        except RuntimeError as error:
            future.set_exception(RuntimeNotReady(str(error)))
            return future

    def start(self) -> concurrent.futures.Future:
        """Start the runtime worker without blocking the caller."""
        with self._state_lock:
            if self._state is RuntimeHostState.RUNNING:
                return _completed_future(None)
            if self._state in {RuntimeHostState.STARTING, RuntimeHostState.DEGRADED} and self.is_alive:
                return self._startup_future
            if self._state is RuntimeHostState.STOPPING:
                return _failed_future(RuntimeNotReady("runtime is stopping"))

            self._state = RuntimeHostState.STARTING
            self._startup_future = concurrent.futures.Future()
            self._startup_shutdown_requests.clear()
            self._thread = threading.Thread(
                target=self._thread_main,
                name="zara-runtime-host",
                daemon=True,
            )
            self._thread.start()
            return self._startup_future

    def submit(self, command: RuntimeCommand) -> concurrent.futures.Future:
        """Schedule one application command on the runtime loop."""
        if not isinstance(command, RuntimeCommand):
            return _failed_future(TypeError("RuntimeHost accepts RuntimeCommand instances only"))

        with self._state_lock:
            state = self._state
            loop = self._loop
            if state is RuntimeHostState.STARTING and isinstance(command, ShutdownRuntime):
                future: concurrent.futures.Future = concurrent.futures.Future()
                self._startup_shutdown_requests.append((command, future))
                return future

        recovery_command = isinstance(command, (RestartRuntime, ShutdownRuntime))
        if state is RuntimeHostState.RUNNING:
            pass
        elif state is RuntimeHostState.DEGRADED and recovery_command:
            pass
        else:
            return _failed_future(RuntimeNotReady(f"runtime is not ready: {state.value}"))

        if loop is None or loop.is_closed():
            return _failed_future(RuntimeNotReady("runtime event loop is unavailable"))
        try:
            return asyncio.run_coroutine_threadsafe(self._dispatch(command), loop)
        except RuntimeError as error:
            return _failed_future(RuntimeNotReady(str(error)))

    def shutdown(self, reason: str = "host shutdown") -> concurrent.futures.Future:
        command = ShutdownRuntime(reason=reason)
        with self._state_lock:
            state = self._state
        if state in {RuntimeHostState.NEW, RuntimeHostState.STOPPED}:
            return _completed_future(
                CommandReceipt(request_id=command.request_id, detail="already stopped")
            )
        return self.submit(command)

    def join(self, timeout: Optional[float] = None) -> None:
        thread = self._thread
        if thread is not None:
            thread.join(timeout=timeout)

    def _thread_main(self) -> None:
        self._thread_id = threading.get_ident()
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        with self._state_lock:
            self._loop = loop

        try:
            loop.run_until_complete(self._run())
        except Exception as error:  # pragma: no cover - last-resort guard
            logger.exception("RuntimeHost worker crashed")
            self._publisher(events.RuntimeError(reason=str(error), fatal=True, label="runtime-host"))
            with self._state_lock:
                self._state = RuntimeHostState.FAILED
            if not self._startup_future.done():
                self._startup_future.set_exception(error)
            self._fail_startup_shutdown_requests(error)
        finally:
            if self._plugin_manager is not None and not loop.is_closed():
                try:
                    loop.run_until_complete(self._stop_plugins())
                except Exception:
                    logger.warning("Service plugin cleanup after runtime exit failed", exc_info=True)
            if self._task_runner is not None and not loop.is_closed():
                try:
                    loop.run_until_complete(self._stop_task_runner())
                except Exception:
                    logger.warning("Task runner cleanup after runtime exit failed", exc_info=True)
            pending = [task for task in asyncio.all_tasks(loop) if not task.done()]
            for task in pending:
                task.cancel()
            if pending:
                loop.run_until_complete(asyncio.gather(*pending, return_exceptions=True))
            loop.close()
            with self._state_lock:
                self._loop = None
                if self._state is RuntimeHostState.STOPPING:
                    self._state = RuntimeHostState.STOPPED
                elif self._state not in {RuntimeHostState.FAILED, RuntimeHostState.STOPPED}:
                    self._state = RuntimeHostState.STOPPED
            self._thread_id = None

    async def _run(self) -> None:
        self._async_stop = asyncio.Event()
        try:
            self._coordinator = TurnCoordinator.start()
        except Exception as error:
            with self._state_lock:
                self._state = RuntimeHostState.FAILED
            self._publisher(
                events.RuntimeError(
                    reason=f"turn coordinator startup failed: {error}",
                    fatal=True,
                    label="runtime-host",
                )
            )
            if not self._startup_future.done():
                self._startup_future.set_exception(error)
            self._fail_startup_shutdown_requests(error)
            return

        startup_error: Optional[Exception] = None
        try:
            await self._start_backend()
        except Exception as error:
            startup_error = error
            with self._state_lock:
                self._state = RuntimeHostState.DEGRADED
                shutdown_requests = self._take_startup_shutdown_requests_locked()
            self._publisher(
                events.RuntimeError(
                    reason=f"runtime startup failed: {error}",
                    fatal=False,
                    label="runtime-host",
                )
            )
            if not self._startup_future.done():
                self._startup_future.set_exception(error)
        else:
            with self._state_lock:
                shutdown_requests = self._take_startup_shutdown_requests_locked()
                if not shutdown_requests:
                    self._state = RuntimeHostState.RUNNING
            if not shutdown_requests:
                await self._start_api_service()
                await self._start_plugins()
                await self._start_task_runner()

        if shutdown_requests:
            if startup_error is None and not self._startup_future.done():
                self._startup_future.set_exception(
                    RuntimeNotReady("runtime shutdown requested during startup")
                )
            await self._complete_startup_shutdown_requests(shutdown_requests)
            await self._async_stop.wait()
            return

        if startup_error is None:
            self._publisher(events.RuntimeStarted(label="runtime-host"))
            if not self._startup_future.done():
                self._startup_future.set_result(None)
        await self._async_stop.wait()

    async def _start_backend(self) -> None:
        candidate = self._backend_factory()
        candidate.bind_event_publisher(self._publisher)
        try:
            await candidate.start()
        except BaseException:
            try:
                await candidate.stop()
            except Exception:
                logger.debug("Runtime backend cleanup after failed start failed", exc_info=True)
            raise
        self._backend = candidate

    async def _start_api_service(self) -> None:
        try:
            config = self._config
            if config is None:
                from zara.config import get_config
                config = get_config()
            service_config = config.get_api_service_config()
            if not service_config["enabled"]:
                return
            from zara.runtime.api_service import build_api_service
            self._api_service = await asyncio.to_thread(
                build_api_service,
                service_config,
                admin_restart_hook=self._submit_admin_restart,
            )
            logger.info("[ApiService] plan service started: %s", self._api_service.registry.provider_ids())
        except Exception as error:
            self._api_service = None
            logger.warning("API service startup failed", exc_info=True)
            self._publisher(
                events.RuntimeError(
                    reason=f"api service startup failed: {error}",
                    fatal=False,
                    label="api-service",
                )
            )

    def _submit_admin_restart(self, reason: str) -> None:
        future = self.submit(RestartRuntime(reason=f"api_service: {reason}"))
        logger.info("[ApiService] admin restart requested (%s)", reason)
        future.add_done_callback(
            lambda done: done.exception()
            and logger.warning("Admin restart failed: %s", done.exception())
        )

    def _stop_api_service(self) -> None:
        if self._api_service is not None:
            logger.info("[ApiService] plan service stopped")
        self._api_service = None

    async def _start_task_runner(self) -> None:
        try:
            config = self._config
            if config is None:
                from zara.config import get_config
                config = get_config()
            get_tasks_config = getattr(config, "get_tasks_config", None)
            tasks_config = get_tasks_config() if callable(get_tasks_config) else {"enabled": False}
            if not tasks_config.get("enabled", False):
                return

            from zara.agent.tools.builtin_tools import build_task_tools
            from zara.tasks.runner import TaskRunner
            from zara.tasks.store import TaskStore

            store = self._task_store
            if store is None:
                from zara.database import get_database
                store = TaskStore(
                    get_database(),
                    step_log_chars=tasks_config["step_log_chars"],
                )
            runner = TaskRunner(
                store=store,
                submit_turn=self._task_submit_turn,
                allocate_turn_id=self._allocate_task_turn_id,
                cancel_turn=self._cancel_task_turn,
                publisher=self._publisher,
                principal_id=self._require_backend().principal_id,
                max_concurrent=tasks_config["max_concurrent"],
                default_max_task_steps=tasks_config["max_task_steps"],
                wall_clock_seconds=tasks_config["wall_clock_minutes"] * 60.0,
                step_log_chars=tasks_config["step_log_chars"],
            )
            await runner.start()
            backend = self._require_backend()
            backend.register_tools(build_task_tools(runner))
            backend.bind_event_publisher(runner.observing_publisher(self._publisher))
            self._task_runner = runner
            logger.info("[TaskRunner] started (max_concurrent=%d)", tasks_config["max_concurrent"])
        except Exception as error:
            self._task_runner = None
            logger.warning("Task runner startup failed", exc_info=True)
            self._publisher(
                events.RuntimeError(
                    reason=f"task runner startup failed: {error}",
                    fatal=False,
                    label="task-runner",
                )
            )

    async def _stop_task_runner(self) -> None:
        runner = self._task_runner
        self._task_runner = None
        if runner is None:
            return
        try:
            from zara.agent.tools.builtin_tools import TASK_TOOL_NAMES
            backend = self._backend
            if backend is not None:
                backend.unregister_tools(list(TASK_TOOL_NAMES))
                backend.bind_event_publisher(self._publisher)
        except Exception:
            logger.debug("Task tool unregistration failed", exc_info=True)
        try:
            await runner.stop()
        except Exception:
            logger.warning("Task runner stop failed", exc_info=True)

    async def _task_submit_turn(
        self, text, *, turn_id, conversation_id, system_context, latency_trace
    ):
        return await self._require_backend().submit_turn(
            text,
            turn_id=turn_id,
            conversation_id=conversation_id,
            system_context=system_context,
            conversation_history=[],
            latency_trace=latency_trace,
        )

    async def _allocate_task_turn_id(self) -> str:
        reply = await self._coordinator_ask(StartTurn())
        if not isinstance(reply, TurnStartedReply):
            raise RuntimeHostError(f"unexpected turn coordinator reply: {reply!r}")
        return reply.turn_id

    async def _cancel_task_turn(self, turn_id: str) -> None:
        try:
            await self._coordinator_ask(ActorCancelTurn(turn_id=turn_id))
        except Exception:
            logger.debug(
                "Coordinator cancellation failed for task turn %s",
                turn_id,
                exc_info=True,
            )
        backend = self._backend
        if backend is not None:
            try:
                await backend.cancel_turn(turn_id)
            except Exception:
                logger.debug(
                    "Backend cancellation hook failed for task turn %s",
                    turn_id,
                    exc_info=True,
                )

    async def _start_plugins(self) -> None:
        if not self._manage_plugins:
            return
        try:
            config = self._config
            if config is None:
                from zara.config import get_config
                config = get_config()
            paths = (
                self._plugin_paths
                if self._plugin_paths is not None
                else tuple(config.get_module_search_paths())
            )
            plugin_config = config.get_plugin_runtime_config()
            backend = self._require_backend()
            manager = PluginManager(
                paths,
                configuration_provider=config.get_plugin_config,
                status_provider=self._plugin_runtime_status,
                dispatcher=self.submit,
                subscriber=self._subscriber,
                tool_registrar=backend.register_tools,
                tool_unregistrar=backend.unregister_tools,
                publisher=self._publisher,
                lifecycle_timeout=plugin_config["lifecycle_timeout"],
                event_queue_size=plugin_config["event_queue_size"],
                max_workers=plugin_config["max_managed_workers"],
                advice_registrar=backend.register_agent_loop_advice,
                advice_unregistrar=backend.unregister_agent_loop_advice,
            )
            self._plugin_manager = manager
            self._last_plugin_diagnostics = ()
            await manager.start()
        except Exception as error:
            logger.warning("Service plugin manager startup failed", exc_info=True)
            self._publisher(
                events.RuntimeError(
                    reason=f"service plugin manager startup failed: {error}",
                    fatal=False,
                    label="plugin-manager",
                )
            )

    async def _stop_plugins(self) -> None:
        manager = self._plugin_manager
        self._plugin_manager = None
        if manager is not None:
            await manager.stop()
            self._last_plugin_diagnostics = manager.diagnostics()

    def _plugin_runtime_status(self) -> RuntimeStatus:
        return RuntimeStatus(
            state=self.state.value,
            alive=self.is_alive,
            thread_id=self.thread_id,
        )

    async def _complete_startup_shutdown_requests(
        self,
        requests: list[tuple[ShutdownRuntime, concurrent.futures.Future]],
    ) -> None:
        command = requests[0][0]
        try:
            receipt = await self._shutdown(command)
        except Exception as error:
            for _, future in requests:
                if not future.done():
                    future.set_exception(error)
            raise
        for request, future in requests:
            if not future.done():
                future.set_result(
                    CommandReceipt(request_id=request.request_id, detail=receipt.detail)
                )

    def _take_startup_shutdown_requests_locked(
        self,
    ) -> list[tuple[ShutdownRuntime, concurrent.futures.Future]]:
        requests = self._startup_shutdown_requests
        self._startup_shutdown_requests = []
        return requests

    def _take_startup_shutdown_requests(
        self,
    ) -> list[tuple[ShutdownRuntime, concurrent.futures.Future]]:
        with self._state_lock:
            return self._take_startup_shutdown_requests_locked()

    def _fail_startup_shutdown_requests(self, error: BaseException) -> None:
        for _, future in self._take_startup_shutdown_requests():
            if not future.done():
                future.set_exception(error)

    async def _dispatch(self, command: RuntimeCommand) -> CommandReceipt:
        try:
            if isinstance(command, SubmitTurn):
                return await self._submit_turn(command)
            if isinstance(command, CancelTurn):
                return await self._cancel_turn(command)
            if isinstance(command, StartVoice):
                await self._require_backend().start_voice()
                return CommandReceipt(request_id=command.request_id, detail="voice started")
            if isinstance(command, StopVoice):
                await self._require_backend().stop_voice()
                return CommandReceipt(request_id=command.request_id, detail="voice stopped")
            if isinstance(command, MuteSpeech):
                await self._require_backend().mute_speech(command.enabled)
                return CommandReceipt(request_id=command.request_id, detail="speech mute updated")
            if isinstance(command, ApproveTool):
                await self._require_backend().approve_tool(command.tool_run_id)
                return CommandReceipt(request_id=command.request_id, detail="tool approved")
            if isinstance(command, RejectTool):
                await self._require_backend().reject_tool(command.tool_run_id, command.reason)
                return CommandReceipt(request_id=command.request_id, detail="tool rejected")
            if isinstance(command, RestartRuntime):
                return await self._restart(command)
            if isinstance(command, ShutdownRuntime):
                return await self._shutdown(command)
            raise TypeError(f"unsupported runtime command type: {type(command).__name__}")
        except UnsupportedRuntimeCommand:
            raise
        except Exception as error:
            if not isinstance(command, SubmitTurn):
                self._publisher(
                    events.RuntimeError(
                        reason=str(error),
                        fatal=False,
                        label=type(command).__name__,
                    )
                )
            raise

    async def _submit_turn(self, command: SubmitTurn) -> CommandReceipt:
        reply = await self._coordinator_ask(StartTurn())
        if not isinstance(reply, TurnStartedReply):
            raise RuntimeHostError(f"unexpected turn coordinator reply: {reply!r}")
        turn_id = reply.turn_id
        self._publisher(
            events.TurnStarted(
                turn_id=turn_id,
                conversation_id=command.conversation_id,
                label="runtime-host",
            )
        )
        task = asyncio.create_task(
            self._run_turn(command, turn_id),
            name=f"zara-turn-{turn_id}",
        )
        self._turn_tasks[turn_id] = task
        task.add_done_callback(lambda _task, tid=turn_id: self._turn_tasks.pop(tid, None))
        return CommandReceipt(request_id=command.request_id, turn_id=turn_id, detail="turn accepted")

    def _build_turn_latency_trace(self, command: SubmitTurn):
        config = self._config
        if config is None:
            try:
                from zara.config import get_config
                config = get_config()
            except Exception:
                return None
        try:
            latency_config = config.get_latency_config()
        except Exception:
            return None
        if not latency_config.get("enabled", False):
            return LatencyTrace(trace_id=command.request_id)
        return LatencyTrace(
            trace_id=command.request_id,
            sink=JSONLMetricsSink(metrics_path(latency_config)),
        )

    async def _run_turn(self, command: SubmitTurn, turn_id: str) -> None:
        backend = self._require_backend()
        self._publisher(
            events.AgentStarted(
                turn_id=turn_id,
                conversation_id=command.conversation_id,
                label="agent",
            )
        )
        latency_trace = self._build_turn_latency_trace(command)
        try:
            result = await backend.submit_turn(
                command.text,
                turn_id=turn_id,
                conversation_id=command.conversation_id,
                context_ids=command.context_ids,
                latency_trace=latency_trace,
            )
        except asyncio.CancelledError:
            raise
        except Exception as error:
            if await self._turn_is_active(turn_id):
                self._publisher(
                    events.AgentFailed(
                        turn_id=turn_id,
                        conversation_id=command.conversation_id,
                        label="agent",
                        reason=str(error),
                    )
                )
            return
        finally:
            if latency_trace is not None:
                latency_trace.flush()

        if not isinstance(result, RuntimeTurnResult):
            if await self._turn_is_active(turn_id):
                self._publisher(
                    events.AgentFailed(
                        turn_id=turn_id,
                        conversation_id=command.conversation_id,
                        label="agent",
                        reason="runtime backend returned an invalid turn result",
                    )
                )
            return
        if not await self._turn_is_active(turn_id):
            logger.debug("Suppressing stale result for cancelled turn %s", turn_id)
            return
        if result.response:
            self._publisher(
                events.ResponseText(
                    turn_id=turn_id,
                    conversation_id=command.conversation_id,
                    label="Zara",
                    text=result.response,
                    truncated=False,
                )
            )
        self._publisher(
            events.AgentCompleted(
                turn_id=turn_id,
                conversation_id=command.conversation_id,
                label="agent",
                success=True,
            )
        )
        self._publisher(
            events.OutputReady(
                turn_id=turn_id,
                conversation_id=command.conversation_id,
                label="Zara",
            )
        )

    async def _cancel_turn(self, command: CancelTurn) -> CommandReceipt:
        reply = await self._coordinator_ask(ActorCancelTurn(turn_id=command.turn_id))
        if not isinstance(reply, TurnCancelledReply):
            raise RuntimeHostError(f"unexpected turn cancellation reply: {reply!r}")
        task = self._turn_tasks.get(command.turn_id)
        if task is not None and not task.done():
            task.cancel()
        if not reply.was_already_cancelled:
            self._publisher(
                events.TurnCancelled(
                    turn_id=command.turn_id,
                    label="runtime-host",
                    reason="cancel command",
                )
            )
            backend = self._backend
            if backend is not None:
                try:
                    await backend.cancel_turn(command.turn_id)
                except Exception:
                    logger.warning(
                        "Backend cancellation hook failed for turn %s",
                        command.turn_id,
                        exc_info=True,
                    )
        detail = "turn already cancelled" if reply.was_already_cancelled else "turn cancelled"
        return CommandReceipt(
            request_id=command.request_id,
            turn_id=command.turn_id,
            detail=detail,
        )

    async def _restart(self, command: RestartRuntime) -> CommandReceipt:
        with self._state_lock:
            self._state = RuntimeHostState.STARTING
        await self._cancel_all_turns(reason="runtime restart")
        await self._stop_task_runner()
        await self._stop_backend()
        if self._coordinator is not None:
            await self._coordinator_ask(Drain())
        self._clarifications.drop_all(reason=SessionCloseReason.RESTART)
        self._publisher(events.RuntimeStopped(reason=command.reason, label="runtime-host"))
        self._stop_api_service()
        await self._stop_plugins()

        try:
            await self._start_backend()
        except Exception as error:
            with self._state_lock:
                self._state = RuntimeHostState.DEGRADED
                shutdown_requests = self._take_startup_shutdown_requests_locked()
            if shutdown_requests:
                await self._complete_startup_shutdown_requests(shutdown_requests)
            raise error

        with self._state_lock:
            shutdown_requests = self._take_startup_shutdown_requests_locked()
            if not shutdown_requests:
                self._state = RuntimeHostState.RUNNING
        if shutdown_requests:
            await self._complete_startup_shutdown_requests(shutdown_requests)
            return CommandReceipt(
                request_id=command.request_id,
                detail="runtime restart interrupted by shutdown",
            )

        await self._start_api_service()
        await self._start_plugins()
        await self._start_task_runner()
        self._publisher(events.RuntimeStarted(label="runtime-host"))
        return CommandReceipt(request_id=command.request_id, detail="runtime restarted")

    async def _shutdown(self, command: ShutdownRuntime) -> CommandReceipt:
        with self._state_lock:
            self._state = RuntimeHostState.STOPPING
        await self._cancel_all_turns(reason="runtime shutdown")
        await self._stop_task_runner()
        await self._stop_backend()
        self._clarifications.drop_all(reason=SessionCloseReason.SHUTDOWN)
        self._publisher(events.RuntimeStopped(reason=command.reason, label="runtime-host"))
        self._stop_api_service()
        await self._stop_plugins()
        await self._stop_coordinator()
        if self._async_stop is not None:
            self._async_stop.set()
        return CommandReceipt(request_id=command.request_id, detail="runtime stopped")

    async def _turn_is_active(self, turn_id: str) -> bool:
        if self._coordinator is None:
            return False
        try:
            return bool(await self._coordinator_ask(ActorEvent(turn_id=turn_id)))
        except Exception:
            return False

    async def _cancel_all_turns(self, *, reason: str) -> None:
        turn_ids = tuple(self._turn_tasks)
        for turn_id in turn_ids:
            try:
                reply = await self._coordinator_ask(ActorCancelTurn(turn_id=turn_id))
            except Exception:
                logger.debug("Coordinator cancellation failed for %s", turn_id, exc_info=True)
                reply = None
            task = self._turn_tasks.get(turn_id)
            if task is not None and not task.done():
                task.cancel()
            if isinstance(reply, TurnCancelledReply) and not reply.was_already_cancelled:
                self._publisher(
                    events.TurnCancelled(turn_id=turn_id, reason=reason, label="runtime-host")
                )

        backend = self._backend
        if backend is not None:
            for turn_id in turn_ids:
                try:
                    await backend.cancel_turn(turn_id)
                except Exception:
                    logger.debug("Backend cancellation hook failed for %s", turn_id, exc_info=True)

        tasks = [task for task in self._turn_tasks.values() if not task.done()]
        if tasks:
            _, pending = await asyncio.wait(tasks, timeout=self._shutdown_timeout)
            for task in pending:
                task.cancel()
            if pending:
                logger.warning("%d runtime turn task(s) did not drain before timeout", len(pending))
        self._turn_tasks.clear()

    async def _stop_backend(self) -> None:
        backend = self._backend
        self._backend = None
        if backend is None:
            return
        try:
            await asyncio.wait_for(backend.stop(), timeout=self._shutdown_timeout)
        except asyncio.TimeoutError:
            logger.warning("Runtime backend stop timed out")
        except Exception:
            logger.warning("Runtime backend stop failed", exc_info=True)

    async def _stop_coordinator(self) -> None:
        coordinator = self._coordinator
        self._coordinator = None
        if coordinator is None:
            return
        try:
            await asyncio.to_thread(coordinator.ask, Drain(), timeout=self._shutdown_timeout)
        except Exception:
            logger.debug("Turn coordinator drain failed", exc_info=True)
        try:
            await asyncio.to_thread(
                coordinator.stop,
                block=True,
                timeout=self._shutdown_timeout,
            )
        except Exception:
            logger.debug("Turn coordinator stop failed", exc_info=True)

    async def _coordinator_ask(self, message):
        coordinator = self._coordinator
        if coordinator is None:
            raise RuntimeNotReady("turn coordinator is unavailable")
        return await asyncio.to_thread(
            coordinator.ask,
            message,
            timeout=self._shutdown_timeout,
        )

    def _require_backend(self) -> RuntimeBackend:
        backend = self._backend
        if backend is None:
            raise RuntimeNotReady("runtime backend is unavailable")
        return backend
