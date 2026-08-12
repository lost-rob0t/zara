"""Dedicated-thread lifecycle and command boundary for the Zara runtime."""

from __future__ import annotations

import asyncio
import concurrent.futures
import enum
import logging
import threading
from typing import Callable, Optional

from zara.actors import (
    CancelTurn as ActorCancelTurn,
    Drain,
    Event as ActorEvent,
    StartTurn,
    TurnCancelledReply,
    TurnCoordinator,
    TurnStartedReply,
)

from . import bridge, events
from .backend import AgentRuntimeBackend, RuntimeBackend, RuntimeTurnResult, UnsupportedRuntimeCommand
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
        shutdown_timeout: float = 5.0,
    ) -> None:
        self._backend_factory = backend_factory or (lambda: AgentRuntimeBackend())
        self._publisher = publisher
        self._shutdown_timeout = max(0.1, float(shutdown_timeout))

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

        # The fields below are owned by the runtime thread after startup.
        self._backend: Optional[RuntimeBackend] = None
        self._coordinator = None
        self._turn_tasks: dict[str, asyncio.Task] = {}

    @property
    def state(self) -> RuntimeHostState:
        with self._state_lock:
            return self._state

    @property
    def thread_id(self) -> Optional[int]:
        return self._thread_id

    @property
    def is_alive(self) -> bool:
        thread = self._thread
        return bool(thread and thread.is_alive())

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

            # Explicit Quit must remain reliable even before backend startup
            # completes. Queue it for the initial startup coroutine instead of
            # making the Qt caller poll or block. During a later runtime
            # restart, _startup_future is already complete and this shortcut is
            # intentionally not used.
            if (
                state is RuntimeHostState.STARTING
                and isinstance(command, ShutdownRuntime)
                and not self._startup_future.done()
            ):
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

    # ------------------------------------------------------------------
    # Runtime-thread lifecycle

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
            self._publisher(
                events.RuntimeError(reason=str(error), fatal=True, label="runtime-host")
            )
            with self._state_lock:
                self._state = RuntimeHostState.FAILED
            if not self._startup_future.done():
                self._startup_future.set_exception(error)
            self._fail_startup_shutdown_requests(error)
        finally:
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
            self._publisher(
                events.RuntimeError(
                    reason=f"runtime startup failed: {error}",
                    fatal=False,
                    label="runtime-host",
                )
            )
            if not self._startup_future.done():
                self._startup_future.set_exception(error)

        shutdown_requests = self._take_startup_shutdown_requests()
        if shutdown_requests:
            if startup_error is None and not self._startup_future.done():
                self._startup_future.set_exception(
                    RuntimeNotReady("runtime shutdown requested during startup")
                )
            await self._complete_startup_shutdown_requests(shutdown_requests)
            await self._async_stop.wait()
            return

        if startup_error is None:
            with self._state_lock:
                self._state = RuntimeHostState.RUNNING
            self._publisher(events.RuntimeStarted(label="runtime-host"))
            if not self._startup_future.done():
                self._startup_future.set_result(None)

        await self._async_stop.wait()

    async def _start_backend(self) -> None:
        candidate = self._backend_factory()
        try:
            await candidate.start()
        except BaseException:
            try:
                await candidate.stop()
            except Exception:
                logger.debug("Runtime backend cleanup after failed start failed", exc_info=True)
            raise
        self._backend = candidate

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
                    CommandReceipt(
                        request_id=request.request_id,
                        detail=receipt.detail,
                    )
                )

    def _take_startup_shutdown_requests(
        self,
    ) -> list[tuple[ShutdownRuntime, concurrent.futures.Future]]:
        with self._state_lock:
            requests = self._startup_shutdown_requests
            self._startup_shutdown_requests = []
            return requests

    def _fail_startup_shutdown_requests(self, error: BaseException) -> None:
        for _, future in self._take_startup_shutdown_requests():
            if not future.done():
                future.set_exception(error)

    # ------------------------------------------------------------------
    # Command dispatch

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
            # Turn execution failures are emitted by _run_turn. Synchronous
            # command/lifecycle failures become explicit runtime status.
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

    async def _run_turn(self, command: SubmitTurn, turn_id: str) -> None:
        backend = self._require_backend()
        self._publisher(
            events.AgentStarted(
                turn_id=turn_id,
                conversation_id=command.conversation_id,
                label="agent",
            )
        )

        try:
            result = await backend.submit_turn(
                command.text,
                turn_id=turn_id,
                conversation_id=command.conversation_id,
                context_ids=command.context_ids,
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

        # This coordinator check is the stale-result gate. A backend may ignore
        # or swallow asyncio cancellation; it still cannot publish a completed
        # result after the canonical turn has been cancelled.
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
        await self._stop_backend()
        if self._coordinator is not None:
            await self._coordinator_ask(Drain())
        self._publisher(events.RuntimeStopped(reason=command.reason, label="runtime-host"))

        try:
            await self._start_backend()
        except Exception:
            with self._state_lock:
                self._state = RuntimeHostState.DEGRADED
            raise

        with self._state_lock:
            self._state = RuntimeHostState.RUNNING
        self._publisher(events.RuntimeStarted(label="runtime-host"))
        return CommandReceipt(request_id=command.request_id, detail="runtime restarted")

    async def _shutdown(self, command: ShutdownRuntime) -> CommandReceipt:
        with self._state_lock:
            self._state = RuntimeHostState.STOPPING

        await self._cancel_all_turns(reason="runtime shutdown")
        await self._stop_backend()
        await self._stop_coordinator()
        self._publisher(events.RuntimeStopped(reason=command.reason, label="runtime-host"))

        if self._async_stop is not None:
            self._async_stop.set()
        return CommandReceipt(request_id=command.request_id, detail="runtime stopped")

    # ------------------------------------------------------------------
    # Turn/lifecycle helpers

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
