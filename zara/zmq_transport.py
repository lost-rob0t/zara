"""Owned-thread ZeroMQ transport for the local-first ZARA/1 protocol.

Issue #129 intentionally keeps authentication out of this module. ROUTER
routing identities are delivery metadata only; every request is dispatched to
the explicitly injected local-owner principal until #130 binds authenticated
transport identity to principal ownership.
"""

from __future__ import annotations

import concurrent.futures
import enum
import logging
import queue
import threading
import time
import uuid
from collections import OrderedDict
from dataclasses import dataclass, field
from typing import Optional

import zmq

from zara.client import ZaraClient, ZaraClientState
from zara.protocol import (
    AUDIO_INPUT_CHANNELS,
    AUDIO_INPUT_CODEC,
    AUDIO_INPUT_CONTENT_TYPE,
    AUDIO_INPUT_FRAME_SAMPLES,
    AUDIO_INPUT_SAMPLE_RATE,
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


logger = logging.getLogger(__name__)


class ClientNotReady(RuntimeError):
    pass


class ClientDisconnected(RuntimeError):
    pass


class ClientBackpressureError(RuntimeError):
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
    pending_request_limit: int = 256
    idempotency_cache_size: int = 512

    def __post_init__(self) -> None:
        for name in (
            "sndhwm",
            "rcvhwm",
            "max_message_bytes",
            "heartbeat_interval_ms",
            "heartbeat_timeout_ms",
            "poll_interval_ms",
            "event_queue_size",
            "pending_request_limit",
            "idempotency_cache_size",
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
class _AudioInputState:
    conversation_id: Optional[str]
    trace_id: Optional[str]
    next_seq: int = 0


@dataclass
class _RouteState:
    session_id: str
    ready: bool = False
    conversation_id: Optional[str] = None
    audio_inputs: dict[str, _AudioInputState] = field(default_factory=dict)


@dataclass(frozen=True)
class _ReplayEntry:
    command: RuntimeCommand
    response: ProtocolMessage


@dataclass
class _InflightEntry:
    command: RuntimeCommand
    routes: list[bytes]


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
        voice_ingress=None,
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
        self._voice_ingress = voice_ingress
        self._thread: Optional[threading.Thread] = None
        self._stop = threading.Event()
        self._started: concurrent.futures.Future = concurrent.futures.Future()
        self._route_outbound: OrderedDict[bytes, queue.Queue[ProtocolMessage]] = OrderedDict()
        self._routes: dict[bytes, _RouteState] = {}
        self._turn_routes: dict[str, bytes] = {}
        self._replay: OrderedDict[tuple[str, str], _ReplayEntry] = OrderedDict()
        self._inflight: dict[tuple[str, str], _InflightEntry] = {}
        self._event_subscription = None
        self._lock = threading.RLock()
        self._generation = 0

    @property
    def is_alive(self) -> bool:
        thread = self._thread
        return bool(thread and thread.is_alive())

    def start(self) -> concurrent.futures.Future:
        with self._lock:
            if self.is_alive:
                return self._started
            if self._owns_context and self._context.closed:
                self._context = zmq.Context()
            self._generation += 1
            self._routes.clear()
            self._turn_routes.clear()
            self._replay.clear()
            self._inflight.clear()
            self._route_outbound.clear()
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

    def _voice_ingress_context(
        self,
        stream_id: str,
        stream: _AudioInputState,
    ) -> dict[str, object]:
        return {
            "principal": self._principal,
            "conversation_id": stream.conversation_id,
            "stream_id": stream_id,
            "trace_id": stream.trace_id,
        }

    def _send_audio_ingress_error(
        self,
        socket: zmq.Socket,
        route: bytes,
        message: ProtocolMessage,
    ) -> None:
        self._send(
            socket,
            route,
            _protocol_error(
                reply_to=message.id,
                code="audio_ingress_error",
                message="audio input runtime is unavailable",
                retryable=True,
            ),
        )

    def _cancel_audio_inputs(self, state: Optional[_RouteState]) -> None:
        if state is None:
            return
        streams = tuple(state.audio_inputs.items())
        state.audio_inputs.clear()
        if self._voice_ingress is None:
            return
        for stream_id, stream in streams:
            try:
                self._voice_ingress.cancel(
                    **self._voice_ingress_context(stream_id, stream)
                )
            except BaseException:
                logger.exception("Failed to cancel voice ingress stream %s", stream_id)

    def _drop_route_locked(self, route: bytes) -> Optional[_RouteState]:
        state = self._routes.pop(route, None)
        self._route_outbound.pop(route, None)
        for turn_id, candidate in tuple(self._turn_routes.items()):
            if candidate == route:
                self._turn_routes.pop(turn_id, None)
        for inflight in self._inflight.values():
            inflight.routes[:] = [candidate for candidate in inflight.routes if candidate != route]
        return state

    def _drop_route(self, route: bytes) -> None:
        with self._lock:
            state = self._drop_route_locked(route)
        self._cancel_audio_inputs(state)

    def _enqueue_outbound(self, route: bytes, message: ProtocolMessage) -> bool:
        dropped_state = None
        with self._lock:
            if route not in self._routes:
                return False
            outbound = self._route_outbound.get(route)
            if outbound is None:
                outbound = queue.Queue(maxsize=self._config.event_queue_size)
                self._route_outbound[route] = outbound
            try:
                outbound.put_nowait(message)
            except queue.Full:
                dropped_state = self._drop_route_locked(route)
        if dropped_state is not None:
            self._cancel_audio_inputs(dropped_state)
            return False
        return True

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
        if message.type == "runtime.status":
            supervisor_state = self._supervisor.state
            status = supervisor_state.value if isinstance(supervisor_state, enum.Enum) else str(supervisor_state)
            self._send(
                socket,
                route,
                ProtocolMessage(
                    type="runtime.status.ok",
                    id=_message_id(),
                    reply_to=message.id,
                    session_id=state.session_id,
                    timestamp_ns=_now_ns(),
                    payload_count=0,
                    body={"state": status},
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
        if message.type in {
            "audio.input.start",
            "audio.input.chunk",
            "audio.input.commit",
            "audio.input.cancel",
        }:
            self._handle_audio_input(socket, route, state, message, decoded.payloads)
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

    def _handle_audio_input(
        self,
        socket: zmq.Socket,
        route: bytes,
        state: _RouteState,
        message: ProtocolMessage,
        payloads: tuple[bytes, ...],
    ) -> None:
        stream_id = message.stream_id
        if stream_id is None:
            self._send(
                socket,
                route,
                _protocol_error(
                    reply_to=message.id,
                    code="invalid_message",
                    message="audio input requires stream id",
                    retryable=False,
                ),
            )
            return

        stream = state.audio_inputs.get(stream_id)
        if message.type == "audio.input.start":
            if stream is not None:
                self._send(
                    socket,
                    route,
                    _protocol_error(
                        reply_to=message.id,
                        code="audio_stream_already_open",
                        message="audio input stream is already open",
                        retryable=False,
                    ),
                )
                return
            stream = _AudioInputState(
                conversation_id=state.conversation_id,
                trace_id=message.trace_id,
            )
            if self._voice_ingress is not None:
                try:
                    self._voice_ingress.start(
                        **self._voice_ingress_context(stream_id, stream)
                    )
                except Exception:
                    logger.exception("Voice ingress start failed for stream %s", stream_id)
                    self._send_audio_ingress_error(socket, route, message)
                    return
            state.audio_inputs[stream_id] = stream
            response_type = "audio.input.started"
        elif message.type == "audio.input.chunk":
            if stream is None:
                self._send(
                    socket,
                    route,
                    _protocol_error(
                        reply_to=message.id,
                        code="audio_stream_not_open",
                        message="audio input stream is not open",
                        retryable=False,
                    ),
                )
                return
            if message.seq != stream.next_seq:
                self._send(
                    socket,
                    route,
                    _protocol_error(
                        reply_to=message.id,
                        code="audio_sequence_error",
                        message="audio input sequence is not contiguous",
                        retryable=False,
                    ),
                )
                return
            if self._voice_ingress is not None:
                try:
                    self._voice_ingress.chunk(
                        payloads[0],
                        **self._voice_ingress_context(stream_id, stream),
                        seq=message.seq,
                    )
                except queue.Full:
                    self._send(
                        socket,
                        route,
                        _protocol_error(
                            reply_to=message.id,
                            code="audio_backpressure",
                            message="audio input is temporarily backpressured",
                            retryable=True,
                        ),
                    )
                    return
                except Exception:
                    logger.exception("Voice ingress chunk failed for stream %s", stream_id)
                    self._send_audio_ingress_error(socket, route, message)
                    return
            stream.next_seq += 1
            response_type = "audio.input.accepted"
        else:
            if stream is None:
                self._send(
                    socket,
                    route,
                    _protocol_error(
                        reply_to=message.id,
                        code="audio_stream_not_open",
                        message="audio input stream is not open",
                        retryable=False,
                    ),
                )
                return
            if self._voice_ingress is not None:
                ingress_context = self._voice_ingress_context(stream_id, stream)
                try:
                    if message.type == "audio.input.cancel":
                        self._voice_ingress.cancel(**ingress_context)
                    else:
                        self._voice_ingress.commit(**ingress_context)
                except Exception:
                    logger.exception(
                        "Voice ingress terminal operation failed for stream %s",
                        stream_id,
                    )
                    self._send_audio_ingress_error(socket, route, message)
                    return
            state.audio_inputs.pop(stream_id, None)
            response_type = (
                "audio.input.cancelled"
                if message.type == "audio.input.cancel"
                else "audio.input.committed"
            )

        self._send(
            socket,
            route,
            ProtocolMessage(
                type=response_type,
                id=_message_id(),
                reply_to=message.id,
                session_id=state.session_id,
                conversation_id=state.conversation_id,
                stream_id=stream_id,
                seq=message.seq if message.type == "audio.input.chunk" else None,
                timestamp_ns=_now_ns(),
                payload_count=0,
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
        with self._lock:
            previous_state = self._drop_route_locked(route)
            self._routes[route] = _RouteState(session_id=session_id, ready=True)
        self._cancel_audio_inputs(previous_state)
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

    def _idempotency_key(self, message: ProtocolMessage) -> tuple[str, str]:
        return self._principal.principal_id, message.id

    def _response_for_route(self, response: ProtocolMessage, route: bytes) -> ProtocolMessage:
        state = self._routes.get(route)
        session_id = state.session_id if state is not None and state.ready else response.session_id
        return ProtocolMessage(
            type=response.type,
            id=response.id,
            timestamp_ns=response.timestamp_ns,
            payload_count=response.payload_count,
            reply_to=response.reply_to,
            session_id=session_id,
            conversation_id=response.conversation_id,
            turn_id=response.turn_id,
            stream_id=response.stream_id,
            seq=response.seq,
            trace_id=response.trace_id,
            content_type=response.content_type,
            flags=response.flags,
            body=response.body,
        )

    def _send_idempotency_conflict(
        self,
        socket: zmq.Socket,
        route: bytes,
        message: ProtocolMessage,
    ) -> None:
        self._send(
            socket,
            route,
            _protocol_error(
                reply_to=message.id,
                code="idempotency_conflict",
                message="request id was already used for a different command",
                retryable=False,
            ),
        )

    def _remember_response(
        self,
        key: tuple[str, str],
        command: RuntimeCommand,
        response: ProtocolMessage,
    ) -> None:
        self._replay[key] = _ReplayEntry(command=command, response=response)
        self._replay.move_to_end(key)
        while len(self._replay) > self._config.idempotency_cache_size:
            self._replay.popitem(last=False)

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

        replay_key = self._idempotency_key(message)
        with self._lock:
            replay = self._replay.get(replay_key)
            if replay is not None:
                if replay.command != command:
                    self._send_idempotency_conflict(socket, route, message)
                    return
                self._replay.move_to_end(replay_key)
                if replay.response.turn_id:
                    self._turn_routes[replay.response.turn_id] = route
                self._send(socket, route, self._response_for_route(replay.response, route))
                return

            inflight = self._inflight.get(replay_key)
            if inflight is not None:
                if inflight.command != command:
                    self._send_idempotency_conflict(socket, route, message)
                    return
                if route not in inflight.routes:
                    inflight.routes.append(route)
                return

            if len(self._inflight) >= self._config.pending_request_limit:
                self._send(
                    socket,
                    route,
                    _protocol_error(
                        reply_to=message.id,
                        code="server_backpressure",
                        message="too many runtime commands are pending",
                        retryable=True,
                    ),
                )
                return
            self._inflight[replay_key] = _InflightEntry(command=command, routes=[route])

        try:
            future = self._supervisor.submit(self._principal, command)
        except (RuntimeCodecError, KeyError, RuntimeError):
            with self._lock:
                self._inflight.pop(replay_key, None)
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

        generation = self._generation

        def completed(done: concurrent.futures.Future) -> None:
            try:
                receipt = done.result()
                if not isinstance(receipt, CommandReceipt):
                    raise TypeError("runtime returned invalid receipt")
                response_type = (
                    "turn.accepted" if message.type == "turn.submit" else "turn.cancel.accepted"
                )
                response = ProtocolMessage(
                    type=response_type,
                    id=_message_id(),
                    reply_to=message.id,
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

            with self._lock:
                if generation != self._generation or self._stop.is_set():
                    self._inflight.pop(replay_key, None)
                    return
                inflight = self._inflight.pop(replay_key, None)
                routes = list(inflight.routes) if inflight is not None else [route]
                live_routes = [candidate for candidate in routes if candidate in self._routes]
                if response.turn_id and live_routes:
                    self._turn_routes[response.turn_id] = live_routes[-1]
                self._remember_response(replay_key, command, response)

            for candidate in routes:
                self._enqueue_outbound(candidate, self._response_for_route(response, candidate))

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
        sent = 0
        while sent < 32:
            with self._lock:
                if not self._route_outbound:
                    return
                route = next(iter(self._route_outbound))
                outbound = self._route_outbound[route]
                self._route_outbound.move_to_end(route)
                try:
                    message = outbound.get_nowait()
                except queue.Empty:
                    self._route_outbound.pop(route, None)
                    continue
                if outbound.empty():
                    self._route_outbound.pop(route, None)
            self._send(socket, route, message)
            sent += 1

    def _send(self, socket: zmq.Socket, route: bytes, message: ProtocolMessage) -> None:
        try:
            socket.send_multipart([route, *encode_message(message, limits=self._limits)], flags=zmq.NOBLOCK)
        except (zmq.Again, zmq.ZMQError):
            self._drop_route(route)

    def close(self, timeout: Optional[float] = None) -> None:
        self._stop.set()
        thread = self._thread
        if thread is not None:
            thread.join(self._config.request_timeout if timeout is None else max(0.0, timeout))
            if thread.is_alive():
                raise TimeoutError("gateway owner thread did not stop")
        if self._owns_context and not self._context.closed:
            self._context.term()


class _PendingKind(str, enum.Enum):
    HELLO = "hello"
    PING = "ping"
    STATUS = "status"
    CONVERSATION = "conversation"
    COMMAND = "command"
    AUDIO = "audio"


@dataclass
class _Pending:
    kind: _PendingKind
    future: concurrent.futures.Future


@dataclass(frozen=True)
class _ClientOutbound:
    message: ProtocolMessage
    payloads: tuple[bytes, ...] = ()


class ZmqZaraClient(ZaraClient):
    """Thread-owned DEALER client implementing the existing ZaraClient seam."""

    def __init__(
        self,
        endpoint: str,
        *,
        context: Optional[zmq.Context] = None,
        config: Optional[TransportConfig] = None,
        limits: Optional[ProtocolLimits] = None,
        voice_output=None,
    ) -> None:
        if not isinstance(endpoint, str) or not endpoint.strip():
            raise ValueError("endpoint must be a non-empty string")
        self._endpoint = endpoint
        self._context = context or zmq.Context()
        self._owns_context = context is None
        self._config = config or TransportConfig()
        self._limits = limits or ProtocolLimits()
        self._voice_output = voice_output
        self._bus = bridge.RuntimeEventBus()
        self._state = ZaraClientState.NEW
        self._state_lock = threading.RLock()
        self._thread: Optional[threading.Thread] = None
        self._stop = threading.Event()
        self._outbound: queue.Queue[_ClientOutbound] = queue.Queue(maxsize=self._config.sndhwm)
        self._pending: dict[str, _Pending] = {}
        self._pending_lock = threading.RLock()
        self._session_id: Optional[str] = None
        self._started: concurrent.futures.Future = concurrent.futures.Future()
        self._active_voice_outputs: dict[str, dict[str, object]] = {}
        self._cancelled_voice_turns: OrderedDict[str, None] = OrderedDict()

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
            if self._owns_context and self._context.closed:
                self._context = zmq.Context()
            self._state = ZaraClientState.STARTING
        self._session_id = None
        self._outbound = queue.Queue(maxsize=self._config.sndhwm)
        self._active_voice_outputs.clear()
        self._cancelled_voice_turns.clear()
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
            with self._pending_lock:
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
                outbound = self._outbound.get_nowait()
            except queue.Empty:
                return
            socket.send_multipart(
                encode_message(
                    outbound.message,
                    payloads=outbound.payloads,
                    limits=self._limits,
                )
            )

    def _receive(self, socket: zmq.Socket) -> None:
        frames = socket.recv_multipart()
        decoded = decode_message(frames, limits=self._limits)
        message = decoded.message
        if message.reply_to:
            with self._pending_lock:
                pending = self._pending.pop(message.reply_to, None)
            if pending is None:
                return
            self._resolve_pending(pending, message)
            return
        if self._handle_voice_output(message, decoded.payloads):
            return
        if message.type == "turn.cancelled":
            self._cancel_voice_output(message)
        self._publish_runtime_event(message)

    def _remember_cancelled_voice_turn(self, turn_id: str) -> None:
        self._cancelled_voice_turns[turn_id] = None
        self._cancelled_voice_turns.move_to_end(turn_id)
        while len(self._cancelled_voice_turns) > self._config.idempotency_cache_size:
            self._cancelled_voice_turns.popitem(last=False)

    def _cancel_voice_output(self, message: ProtocolMessage) -> None:
        turn_id = message.turn_id
        if not turn_id:
            return
        self._stop_voice_output_turn(turn_id)

    def _stop_voice_output_turn(self, turn_id: str) -> None:
        common = self._active_voice_outputs.pop(turn_id, None)
        self._remember_cancelled_voice_turn(turn_id)
        if common is not None and self._voice_output is not None:
            try:
                self._voice_output.cancel(**common)
            except Exception:
                logger.exception("Voice output cancel failed for turn %s", turn_id)

    def _barge_in_active_voice_outputs(self) -> None:
        for turn_id in tuple(self._active_voice_outputs):
            self._stop_voice_output_turn(turn_id)
            self.submit(CancelTurn(turn_id=turn_id))

    def _handle_voice_output(
        self,
        message: ProtocolMessage,
        payloads: tuple[bytes, ...],
    ) -> bool:
        if message.type not in {
            "audio.output.start",
            "audio.output.chunk",
            "audio.output.done",
        }:
            return False
        turn_id = message.turn_id
        if turn_id and turn_id in self._cancelled_voice_turns:
            return True
        if self._voice_output is None:
            return True
        common = {
            "conversation_id": message.conversation_id,
            "turn_id": turn_id,
            "stream_id": message.stream_id,
            "trace_id": message.trace_id,
        }
        if message.type == "audio.output.start":
            if turn_id:
                self._active_voice_outputs[turn_id] = common
            self._voice_output.start(format=dict(message.body or {}), **common)
        elif message.type == "audio.output.chunk":
            self._voice_output.chunk(payloads[0], seq=message.seq, **common)
        else:
            if turn_id:
                self._active_voice_outputs.pop(turn_id, None)
            self._voice_output.finish(**common)
        return True

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
        if pending.kind is _PendingKind.STATUS:
            body = message.body or {}
            state = body.get("state")
            if message.type != "runtime.status.ok" or not isinstance(state, str) or not state:
                pending.future.set_exception(ProtocolValidationError("invalid runtime status response"))
            else:
                pending.future.set_result(state)
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
            return
        if pending.kind is _PendingKind.AUDIO:
            if message.type not in {
                "audio.input.started",
                "audio.input.accepted",
                "audio.input.committed",
                "audio.input.cancelled",
            }:
                pending.future.set_exception(ProtocolValidationError("invalid audio input response"))
            else:
                pending.future.set_result(message)

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
        elif message.type == "turn.completed":
            event = events.AgentCompleted(success=bool(body.get("success", False)), **common)
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

    def _request(
        self,
        message: ProtocolMessage,
        kind: _PendingKind,
        *,
        payloads: tuple[bytes, ...] = (),
    ) -> concurrent.futures.Future:
        if self.state is not ZaraClientState.READY:
            raise ClientNotReady("client handshake is not ready")
        future = concurrent.futures.Future()
        with self._pending_lock:
            if message.id in self._pending:
                raise ClientBackpressureError("request id is already pending")
            if len(self._pending) >= self._config.pending_request_limit:
                raise ClientBackpressureError("client pending request limit reached")
            self._pending[message.id] = _Pending(kind, future)
        try:
            self._outbound.put_nowait(_ClientOutbound(message, payloads))
        except queue.Full as error:
            with self._pending_lock:
                self._pending.pop(message.id, None)
            raise ClientBackpressureError("client outbound queue is full") from error
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

    def runtime_status(self) -> concurrent.futures.Future:
        return self._request(
            ProtocolMessage(
                type="runtime.status",
                id=_message_id(),
                session_id=self._session_id,
                timestamp_ns=_now_ns(),
                payload_count=0,
            ),
            _PendingKind.STATUS,
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

    def start_audio_input(
        self,
        stream_id: str,
        *,
        trace_id: Optional[str] = None,
    ) -> concurrent.futures.Future:
        self._barge_in_active_voice_outputs()
        return self._request(
            ProtocolMessage(
                type="audio.input.start",
                id=_message_id(),
                session_id=self._session_id,
                stream_id=stream_id,
                trace_id=trace_id,
                timestamp_ns=_now_ns(),
                payload_count=0,
                body={
                    "codec": AUDIO_INPUT_CODEC,
                    "sample_rate": AUDIO_INPUT_SAMPLE_RATE,
                    "channels": AUDIO_INPUT_CHANNELS,
                    "frame_samples": AUDIO_INPUT_FRAME_SAMPLES,
                },
            ),
            _PendingKind.AUDIO,
        )

    def send_audio_input(
        self,
        stream_id: str,
        *,
        seq: int,
        pcm: bytes,
        trace_id: Optional[str] = None,
    ) -> concurrent.futures.Future:
        return self._request(
            ProtocolMessage(
                type="audio.input.chunk",
                id=_message_id(),
                session_id=self._session_id,
                stream_id=stream_id,
                seq=seq,
                trace_id=trace_id,
                content_type=AUDIO_INPUT_CONTENT_TYPE,
                timestamp_ns=_now_ns(),
                payload_count=1,
            ),
            _PendingKind.AUDIO,
            payloads=(pcm,),
        )

    def commit_audio_input(
        self,
        stream_id: str,
        *,
        trace_id: Optional[str] = None,
    ) -> concurrent.futures.Future:
        return self._request(
            ProtocolMessage(
                type="audio.input.commit",
                id=_message_id(),
                session_id=self._session_id,
                stream_id=stream_id,
                trace_id=trace_id,
                timestamp_ns=_now_ns(),
                payload_count=0,
            ),
            _PendingKind.AUDIO,
        )

    def cancel_audio_input(
        self,
        stream_id: str,
        *,
        trace_id: Optional[str] = None,
    ) -> concurrent.futures.Future:
        return self._request(
            ProtocolMessage(
                type="audio.input.cancel",
                id=_message_id(),
                session_id=self._session_id,
                stream_id=stream_id,
                trace_id=trace_id,
                timestamp_ns=_now_ns(),
                payload_count=0,
            ),
            _PendingKind.AUDIO,
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

    def reconnect(self) -> concurrent.futures.Future:
        self.shutdown(reason="client reconnect")
        thread = self._thread
        if thread is not None:
            thread.join(self._config.request_timeout)
            if thread.is_alive():
                raise TimeoutError("client owner thread did not stop for reconnect")
        self._session_id = None
        with self._state_lock:
            self._state = ZaraClientState.STOPPED
        return self.start()

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
        if self._owns_context and not self._context.closed:
            self._context.term()


__all__ = [
    "ClientBackpressureError",
    "ClientDisconnected",
    "ClientNotReady",
    "ProtocolRemoteError",
    "TransportConfig",
    "ZaraZmqGateway",
    "ZmqZaraClient",
    "apply_socket_options",
]
