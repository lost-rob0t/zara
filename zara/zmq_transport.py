"""Owned-thread ZeroMQ transport for the local-first ZARA/1 protocol.

Issue #129 intentionally keeps authentication out of this module. ROUTER
routing identities are delivery metadata only; every request is dispatched to
the explicitly injected local-owner principal until #130 binds authenticated
transport identity to principal ownership.
"""

from __future__ import annotations

import concurrent.futures
import enum
import queue
import threading
import time
import uuid
from dataclasses import dataclass
from typing import Optional

import zmq

from zara.client import ZaraClient, ZaraClientState
from zara.protocol import (
    ProtocolLimits,
    ProtocolMessage,
    ProtocolValidationError,
    ZaraProtocolError,
    decode_message,
    encode_message,
)
from zara.protocol_runtime import RuntimeCodecError, command_from_message, runtime_event_to_message
from zara.runtime import bridge, events
from zara.runtime.commands import CancelTurn, CommandReceipt, RuntimeCommand, SubmitTurn
from zara.server import PrincipalContext


class ClientNotReady(RuntimeError):
    pass


class ClientDisconnected(RuntimeError):
    pass


class ProtocolRemoteError(RuntimeError):
    def __init__(self, code: str, message: str, *, retryable: bool = False) -> None:
        super().__init__(message)
        self.code = code
        self.retryable = retryable


@dataclass(frozen=True)
class TransportConfig:
    sndhwm: int = 256
    rcvhwm: int = 256
    max_message_bytes: int = 5 * 1024 * 1024
    heartbeat_interval_ms: int = 1_000
    heartbeat_timeout_ms: int = 3_000
    linger_ms: int = 0
    request_timeout: float = 5.0
    poll_interval_ms: int = 10
    event_queue_size: int = 256

    def __post_init__(self) -> None:
        for name in (
            "sndhwm",
            "rcvhwm",
            "max_message_bytes",
            "heartbeat_interval_ms",
            "heartbeat_timeout_ms",
            "poll_interval_ms",
            "event_queue_size",
        ):
            value = getattr(self, name)
            if type(value) is not int or value <= 0:
                raise ValueError(f"{name} must be a positive integer")
        if type(self.linger_ms) is not int or self.linger_ms < 0:
            raise ValueError("linger_ms must be a non-negative integer")
        if not isinstance(self.request_timeout, (int, float)) or self.request_timeout <= 0:
            raise ValueError("request_timeout must be positive")


def apply_socket_options(socket: zmq.Socket, config: TransportConfig, *, router: bool) -> None:
    socket.setsockopt(zmq.SNDHWM, config.sndhwm)
    socket.setsockopt(zmq.RCVHWM, config.rcvhwm)
    socket.setsockopt(zmq.MAXMSGSIZE, config.max_message_bytes)
    socket.setsockopt(zmq.LINGER, config.linger_ms)
    if hasattr(zmq, "HEARTBEAT_IVL"):
        socket.setsockopt(zmq.HEARTBEAT_IVL, config.heartbeat_interval_ms)
    if hasattr(zmq, "HEARTBEAT_TIMEOUT"):
        socket.setsockopt(zmq.HEARTBEAT_TIMEOUT, config.heartbeat_timeout_ms)
    if router:
        socket.setsockopt(zmq.ROUTER_MANDATORY, 1)


def _message_id() -> str:
    return uuid.uuid4().hex


def _now_ns() -> int:
    return time.time_ns()


def _protocol_error(
    *,
    reply_to: Optional[str],
    code: str,
    message: str,
    retryable: bool,
) -> ProtocolMessage:
    return ProtocolMessage(
        type="protocol.error",
        id=_message_id(),
        reply_to=reply_to,
        timestamp_ns=_now_ns(),
        payload_count=0,
        body={"code": code, "message": message, "retryable": retryable},
    )


@dataclass
class _RouteState:
    session_id: str
    ready: bool = False
    conversation_id: Optional[str] = None


class ZaraZmqGateway:
    """ROUTER owner loop bridging ZARA/1 requests to RuntimeSupervisor."""

    def __init__(
        self,
        endpoint: str,
        *,
        supervisor,
        principal: PrincipalContext,
        context: Optional[zmq.Context] = None,
        config: Optional[TransportConfig] = None,
        limits: Optional[ProtocolLimits] = None,
    ) -> None:
        if not isinstance(endpoint, str) or not endpoint.strip():
            raise ValueError("endpoint must be a non-empty string")
        if not isinstance(principal, PrincipalContext):
            raise TypeError("gateway requires PrincipalContext")
        self._endpoint = endpoint
        self._supervisor = supervisor
        self._principal = principal
        self._context = context or zmq.Context()
        self._owns_context = context is None
        self._config = config or TransportConfig()
        self._limits = limits or ProtocolLimits()
        self._thread: Optional[threading.Thread] = None
        self._stop = threading.Event()
        self._started: concurrent.futures.Future = concurrent.futures.Future()
        self._outbound: queue.Queue[tuple[bytes, ProtocolMessage]] = queue.Queue(
            maxsize=self._config.event_queue_size
        )
        self._routes: dict[bytes, _RouteState] = {}
        self._turn_routes: dict[str, bytes] = {}
        self._event_subscription = None
        self._lock = threading.RLock()

    @property
    def is_alive(self) -> bool:
        thread = self._thread
        return bool(thread and thread.is_alive())

    def start(self) -> concurrent.futures.Future:
        with self._lock:
            if self.is_alive:
                return self._started
            self._stop.clear()
            self._started = concurrent.futures.Future()
            self._thread = threading.Thread(
                target=self._run,
                name="zara-zmq-gateway",
                daemon=True,
            )
            self._thread.start()
            return self._started

    def _run(self) -> None:
        socket = self._context.socket(zmq.ROUTER)
        apply_socket_options(socket, self._config, router=True)
        try:
            socket.bind(self._endpoint)
            self._event_subscription = self._supervisor.subscribe(
                self._principal,
                maxsize=self._config.event_queue_size,
            )
            self._started.set_result(True)
            poller = zmq.Poller()
            poller.register(socket, zmq.POLLIN)
            while not self._stop.is_set():
                self._drain_runtime_events(socket)
                self._drain_outbound(socket)
                ready = dict(poller.poll(self._config.poll_interval_ms))
                if ready.get(socket) == zmq.POLLIN:
                    self._receive(socket)
        except BaseException as error:
            if not self._started.done():
                self._started.set_exception(error)
        finally:
            subscription = self._event_subscription
            self._event_subscription = None
            if subscription is not None:
                subscription.close()
            socket.close(self._config.linger_ms)

    def _receive(self, socket: zmq.Socket) -> None:
        frames = socket.recv_multipart()
        if len(frames) < 2:
            return
        route, app_frames = frames[0], frames[1:]
        try:
            decoded = decode_message(app_frames, limits=self._limits)
        except ZaraProtocolError:
            self._send(
                socket,
                route,
                _protocol_error(
                    reply_to=None,
                    code="invalid_message",
                    message="invalid protocol message",
                    retryable=False,
                ),
            )
            return

        message = decoded.message
        state = self._routes.get(route)
        if message.type == "hello":
            self._handle_hello(socket, route, message)
            return
        if state is None or not state.ready:
            self._send(
                socket,
                route,
                _protocol_error(
                    reply_to=message.id,
                    code="handshake_required",
                    message="handshake required",
                    retryable=True,
                ),
            )
            return
        if message.type == "ping":
            self._send(
                socket,
                route,
                ProtocolMessage(
                    type="pong",
                    id=_message_id(),
                    reply_to=message.id,
                    session_id=state.session_id,
                    timestamp_ns=_now_ns(),
                    payload_count=0,
                ),
            )
            return
        if message.type == "conversation.open":
            conversation_id = message.conversation_id or _message_id()
            state.conversation_id = conversation_id
            self._send(
                socket,
                route,
                ProtocolMessage(
                    type="conversation.opened",
                    id=_message_id(),
                    reply_to=message.id,
                    session_id=state.session_id,
                    conversation_id=conversation_id,
                    timestamp_ns=_now_ns(),
                    payload_count=0,
                ),
            )
            return
        if message.type in {"turn.submit", "turn.cancel"}:
            self._dispatch_runtime(socket, route, state, message)
            return
        self._send(
            socket,
            route,
            _protocol_error(
                reply_to=message.id,
                code="not_implemented",
                message="message is not implemented",
                retryable=False,
            ),
        )

    def _handle_hello(self, socket: zmq.Socket, route: bytes, message: ProtocolMessage) -> None:
        body = message.body or {}
        versions = body.get("versions", [1])
        if (
            not isinstance(versions, list)
            or not versions
            or any(type(version) is not int or version < 0 for version in versions)
            or 1 not in versions
        ):
            self._send(
                socket,
                route,
                _protocol_error(
                    reply_to=message.id,
                    code="unsupported_version",
                    message="no supported protocol version",
                    retryable=False,
                ),
            )
            return
        session_id = _message_id()
        self._routes[route] = _RouteState(session_id=session_id, ready=True)
        self._send(
            socket,
            route,
            ProtocolMessage(
                type="hello.ok",
                id=_message_id(),
                reply_to=message.id,
                session_id=session_id,
                timestamp_ns=_now_ns(),
                payload_count=0,
                body={
                    "version": 1,
                    "max_payload_frames": self._limits.max_payload_frames,
                    "max_payload_frame_bytes": self._limits.max_payload_frame_bytes,
                    "max_payload_bytes": self._limits.max_payload_bytes,
                },
            ),
        )

    def _dispatch_runtime(
        self,
        socket: zmq.Socket,
        route: bytes,
        state: _RouteState,
        message: ProtocolMessage,
    ) -> None:
        if message.type == "turn.submit" and message.conversation_id is None:
            message = ProtocolMessage(
                type=message.type,
                id=message.id,
                timestamp_ns=message.timestamp_ns,
                payload_count=message.payload_count,
                reply_to=message.reply_to,
                session_id=message.session_id,
                conversation_id=state.conversation_id,
                turn_id=message.turn_id,
                stream_id=message.stream_id,
                seq=message.seq,
                trace_id=message.trace_id,
                content_type=message.content_type,
                flags=message.flags,
                body=message.body,
            )
        try:
            command = command_from_message(message)
            future = self._supervisor.submit(self._principal, command)
        except (RuntimeCodecError, KeyError, RuntimeError):
            self._send(
                socket,
                route,
                _protocol_error(
                    reply_to=message.id,
                    code="invalid_command",
                    message="invalid runtime command",
                    retryable=False,
                ),
            )
            return

        def completed(done: concurrent.futures.Future) -> None:
            try:
                receipt = done.result()
                if not isinstance(receipt, CommandReceipt):
                    raise TypeError("runtime returned invalid receipt")
                if receipt.turn_id:
                    self._turn_routes[receipt.turn_id] = route
                response_type = (
                    "turn.accepted" if message.type == "turn.submit" else "turn.cancel.accepted"
                )
                response = ProtocolMessage(
                    type=response_type,
                    id=_message_id(),
                    reply_to=message.id,
                    session_id=state.session_id,
                    conversation_id=message.conversation_id,
                    turn_id=receipt.turn_id,
                    timestamp_ns=_now_ns(),
                    payload_count=0,
                )
            except BaseException:
                response = _protocol_error(
                    reply_to=message.id,
                    code="runtime_error",
                    message="runtime command failed",
                    retryable=False,
                )
            try:
                self._outbound.put_nowait((route, response))
            except queue.Full:
                self._routes.pop(route, None)

        future.add_done_callback(completed)

    def _drain_runtime_events(self, socket: zmq.Socket) -> None:
        subscription = self._event_subscription
        if subscription is None:
            return
        for envelope in subscription.drain(limit=32):
            route = None
            event = envelope.event
            if event.turn_id:
                route = self._turn_routes.get(event.turn_id)
            if route is None and event.conversation_id:
                matches = [
                    candidate
                    for candidate, state in self._routes.items()
                    if state.ready and state.conversation_id == event.conversation_id
                ]
                if len(matches) == 1:
                    route = matches[0]
            if route is None:
                continue
            try:
                message = runtime_event_to_message(
                    envelope,
                    message_id=_message_id(),
                    timestamp_ns=_now_ns(),
                )
            except RuntimeCodecError:
                continue
            self._send(socket, route, message)

    def _drain_outbound(self, socket: zmq.Socket) -> None:
        for _ in range(32):
            try:
                route, message = self._outbound.get_nowait()
            except queue.Empty:
                return
            self._send(socket, route, message)

    def _send(self, socket: zmq.Socket, route: bytes, message: ProtocolMessage) -> None:
        try:
            socket.send_multipart([route, *encode_message(message, limits=self._limits)], flags=zmq.NOBLOCK)
        except zmq.Again:
            self._routes.pop(route, None)
        except zmq.ZMQError:
            self._routes.pop(route, None)

    def close(self, timeout: Optional[float] = None) -> None:
        self._stop.set()
        thread = self._thread
        if thread is not None:
            thread.join(self._config.request_timeout if timeout is None else max(0.0, timeout))
            if thread.is_alive():
                raise TimeoutError("gateway owner thread did not stop")
        if self._owns_context:
            self._context.term()


class _PendingKind(str, enum.Enum):
    HELLO = "hello"
    PING = "ping"
    CONVERSATION = "conversation"
    COMMAND = "command"


@dataclass
class _Pending:
    kind: _PendingKind
    future: concurrent.futures.Future


class ZmqZaraClient(ZaraClient):
    """Thread-owned DEALER client implementing the existing ZaraClient seam."""

    def __init__(
        self,
        endpoint: str,
        *,
        context: Optional[zmq.Context] = None,
        config: Optional[TransportConfig] = None,
        limits: Optional[ProtocolLimits] = None,
    ) -> None:
        if not isinstance(endpoint, str) or not endpoint.strip():
            raise ValueError("endpoint must be a non-empty string")
        self._endpoint = endpoint
        self._context = context or zmq.Context()
        self._owns_context = context is None
        self._config = config or TransportConfig()
        self._limits = limits or ProtocolLimits()
        self._bus = bridge.RuntimeEventBus()
        self._state = ZaraClientState.NEW
        self._state_lock = threading.RLock()
        self._thread: Optional[threading.Thread] = None
        self._stop = threading.Event()
        self._outbound: queue.Queue[ProtocolMessage] = queue.Queue(maxsize=self._config.sndhwm)
        self._pending: dict[str, _Pending] = {}
        self._pending_lock = threading.RLock()
        self._session_id: Optional[str] = None
        self._started: concurrent.futures.Future = concurrent.futures.Future()

    @property
    def state(self) -> ZaraClientState:
        with self._state_lock:
            return self._state

    @property
    def session_id(self) -> Optional[str]:
        return self._session_id

    @property
    def is_alive(self) -> bool:
        thread = self._thread
        return bool(thread and thread.is_alive())

    def start(self) -> concurrent.futures.Future:
        with self._state_lock:
            if self._state is ZaraClientState.READY:
                completed = concurrent.futures.Future()
                completed.set_result(True)
                return completed
            if self._state is ZaraClientState.STARTING:
                return self._started
            if self._state not in {ZaraClientState.NEW, ZaraClientState.STOPPED}:
                raise ClientNotReady(f"client cannot start from {self._state.value}")
            self._state = ZaraClientState.STARTING
        self._stop.clear()
        self._started = concurrent.futures.Future()
        self._thread = threading.Thread(target=self._run, name="zara-zmq-client", daemon=True)
        self._thread.start()
        return self._started

    def _run(self) -> None:
        socket = self._context.socket(zmq.DEALER)
        apply_socket_options(socket, self._config, router=False)
        poller = zmq.Poller()
        try:
            socket.connect(self._endpoint)
            poller.register(socket, zmq.POLLIN)
            hello_id = _message_id()
            self._pending[hello_id] = _Pending(_PendingKind.HELLO, self._started)
            socket.send_multipart(
                encode_message(
                    ProtocolMessage(
                        type="hello",
                        id=hello_id,
                        timestamp_ns=_now_ns(),
                        payload_count=0,
                        body={"versions": [1]},
                    ),
                    limits=self._limits,
                )
            )
            deadline = time.monotonic() + self._config.request_timeout
            while not self._stop.is_set():
                self._expire_start(deadline)
                self._drain_client_outbound(socket)
                ready = dict(poller.poll(self._config.poll_interval_ms))
                if ready.get(socket) == zmq.POLLIN:
                    self._receive(socket)
        except BaseException as error:
            if not self._started.done():
                self._started.set_exception(error)
            with self._state_lock:
                if self._state is not ZaraClientState.STOPPING:
                    self._state = ZaraClientState.FAILED
        finally:
            socket.close(self._config.linger_ms)
            self._fail_pending(ClientDisconnected("client transport stopped"))
            with self._state_lock:
                if self._state is ZaraClientState.STOPPING:
                    self._state = ZaraClientState.STOPPED

    def _expire_start(self, deadline: float) -> None:
        if self.state is not ZaraClientState.STARTING or time.monotonic() < deadline:
            return
        error = ClientDisconnected("ZARA/1 handshake timed out")
        if not self._started.done():
            self._started.set_exception(error)
        with self._state_lock:
            self._state = ZaraClientState.FAILED
        self._stop.set()

    def _drain_client_outbound(self, socket: zmq.Socket) -> None:
        for _ in range(32):
            try:
                message = self._outbound.get_nowait()
            except queue.Empty:
                return
            socket.send_multipart(encode_message(message, limits=self._limits))

    def _receive(self, _socket: zmq.Socket) -> None:
        frames = _socket.recv_multipart()
        decoded = decode_message(frames, limits=self._limits)
        message = decoded.message
        if message.reply_to:
            with self._pending_lock:
                pending = self._pending.pop(message.reply_to, None)
            if pending is None:
                return
            self._resolve_pending(pending, message)
            return
        self._publish_runtime_event(message)

    def _resolve_pending(self, pending: _Pending, message: ProtocolMessage) -> None:
        if message.type == "protocol.error":
            body = message.body or {}
            pending.future.set_exception(
                ProtocolRemoteError(
                    str(body.get("code", "protocol_error")),
                    str(body.get("message", "remote protocol error")),
                    retryable=bool(body.get("retryable", False)),
                )
            )
            if pending.kind is _PendingKind.HELLO:
                with self._state_lock:
                    self._state = ZaraClientState.FAILED
                self._stop.set()
            return
        if pending.kind is _PendingKind.HELLO:
            if message.type != "hello.ok" or not message.session_id:
                pending.future.set_exception(ProtocolValidationError("invalid hello response"))
                self._stop.set()
                return
            self._session_id = message.session_id
            with self._state_lock:
                self._state = ZaraClientState.READY
            pending.future.set_result(True)
            return
        if pending.kind is _PendingKind.PING:
            if message.type != "pong":
                pending.future.set_exception(ProtocolValidationError("invalid ping response"))
            else:
                pending.future.set_result(message)
            return
        if pending.kind is _PendingKind.CONVERSATION:
            if message.type != "conversation.opened" or not message.conversation_id:
                pending.future.set_exception(ProtocolValidationError("invalid conversation response"))
            else:
                pending.future.set_result(message.conversation_id)
            return
        if pending.kind is _PendingKind.COMMAND:
            if message.type not in {"turn.accepted", "turn.cancel.accepted"}:
                pending.future.set_exception(ProtocolValidationError("invalid command response"))
            else:
                pending.future.set_result(
                    CommandReceipt(request_id=message.reply_to or "", turn_id=message.turn_id)
                )

    def _publish_runtime_event(self, message: ProtocolMessage) -> None:
        body = message.body or {}
        common = {
            "turn_id": message.turn_id,
            "conversation_id": message.conversation_id,
        }
        event = None
        if message.type == "turn.started":
            event = events.TurnStarted(**common)
        elif message.type == "turn.cancelled":
            event = events.TurnCancelled(reason=str(body.get("reason", "")), **common)
        elif message.type == "assistant.started":
            event = events.AssistantStarted(**common)
        elif message.type == "assistant.delta":
            event = events.AssistantDelta(text=str(body.get("text", "")), **common)
        elif message.type == "assistant.completed":
            event = events.AssistantComplete(
                text=str(body.get("text", "")),
                success=bool(body.get("success", False)),
                **common,
            )
        elif message.type == "assistant.response":
            event = events.ResponseText(
                text=str(body.get("text", "")),
                truncated=bool(body.get("truncated", False)),
                **common,
            )
        elif message.type == "runtime.error":
            event = events.RuntimeError(
                reason=str(body.get("reason", "")),
                fatal=bool(body.get("fatal", False)),
                **common,
            )
        elif message.type == "runtime.stopped":
            event = events.RuntimeStopped(reason=str(body.get("reason", "")), **common)
        if event is not None:
            self._bus.publish(event)

    def _request(self, message: ProtocolMessage, kind: _PendingKind) -> concurrent.futures.Future:
        if self.state is not ZaraClientState.READY:
            raise ClientNotReady("client handshake is not ready")
        future = concurrent.futures.Future()
        with self._pending_lock:
            self._pending[message.id] = _Pending(kind, future)
        try:
            self._outbound.put_nowait(message)
        except queue.Full as error:
            with self._pending_lock:
                self._pending.pop(message.id, None)
            raise ClientNotReady("client outbound queue is full") from error
        return future

    def ping(self) -> concurrent.futures.Future:
        return self._request(
            ProtocolMessage(
                type="ping",
                id=_message_id(),
                session_id=self._session_id,
                timestamp_ns=_now_ns(),
                payload_count=0,
            ),
            _PendingKind.PING,
        )

    def open_conversation(self, conversation_id: Optional[str] = None) -> concurrent.futures.Future:
        return self._request(
            ProtocolMessage(
                type="conversation.open",
                id=_message_id(),
                session_id=self._session_id,
                conversation_id=conversation_id,
                timestamp_ns=_now_ns(),
                payload_count=0,
            ),
            _PendingKind.CONVERSATION,
        )

    def submit(self, command: RuntimeCommand) -> concurrent.futures.Future:
        if self.state is not ZaraClientState.READY:
            raise ClientNotReady("client handshake is not ready")
        if isinstance(command, SubmitTurn):
            message = ProtocolMessage(
                type="turn.submit",
                id=command.request_id,
                session_id=self._session_id,
                conversation_id=command.conversation_id,
                timestamp_ns=_now_ns(),
                payload_count=0,
                body={"text": command.text, "context_ids": list(command.context_ids)},
            )
        elif isinstance(command, CancelTurn):
            message = ProtocolMessage(
                type="turn.cancel",
                id=command.request_id,
                session_id=self._session_id,
                turn_id=command.turn_id,
                timestamp_ns=_now_ns(),
                payload_count=0,
            )
        else:
            raise TypeError("ZmqZaraClient supports SubmitTurn and CancelTurn in ZARA/1 v1")
        return self._request(message, _PendingKind.COMMAND)

    def subscribe(self, *, maxsize: int = 0) -> bridge.RuntimeEventSubscription:
        return self._bus.subscribe(maxsize=maxsize)

    def shutdown(self, reason: str = "client shutdown") -> concurrent.futures.Future:
        future = concurrent.futures.Future()
        self._stop.set()
        with self._state_lock:
            if self._state in {ZaraClientState.NEW, ZaraClientState.STOPPED}:
                self._state = ZaraClientState.STOPPED
            else:
                self._state = ZaraClientState.STOPPING
        future.set_result(reason)
        return future

    def _fail_pending(self, error: BaseException) -> None:
        with self._pending_lock:
            pending = tuple(self._pending.values())
            self._pending.clear()
        for item in pending:
            if not item.future.done():
                item.future.set_exception(error)

    def close(self, timeout: Optional[float] = None) -> None:
        self.shutdown()
        thread = self._thread
        if thread is not None:
            thread.join(self._config.request_timeout if timeout is None else max(0.0, timeout))
            if thread.is_alive():
                raise TimeoutError("client owner thread did not stop")
        with self._state_lock:
            self._state = ZaraClientState.STOPPED
        if self._owns_context:
            self._context.term()


__all__ = [
    "ClientDisconnected",
    "ClientNotReady",
    "ProtocolRemoteError",
    "TransportConfig",
    "ZaraZmqGateway",
    "ZmqZaraClient",
    "apply_socket_options",
]
