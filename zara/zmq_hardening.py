"""Reliability and low-latency policy for the ZARA/1 ZeroMQ transport.

This module deliberately layers policy over :mod:`zara.zmq_transport` instead
of changing the ZARA/1 wire format. Existing clients remain compatible while
production gets bounded request lifetimes, non-blocking FIFO sends, stricter
multipart guards, and a lower-latency owner loop configuration.
"""

from __future__ import annotations

import concurrent.futures
import inspect
import logging
import queue
import time
from typing import Optional

import zmq

from zara.protocol import PROTOCOL_MARKER, ProtocolMessage, encode_message
from zara.zmq_transport import (
    TransportConfig,
    ZaraZmqGateway,
    ZmqZaraClient,
    _ClientOutbound,
    _protocol_error,
)

logger = logging.getLogger(__name__)

_MAX_ROUTING_ID_BYTES = 255
_OUTBOUND_BATCH = 64


class ClientRequestTimeout(TimeoutError):
    """A ZARA/1 request exceeded the configured transport deadline."""


def hardened_transport_config() -> TransportConfig:
    """Return the production default tuned for bounded latency and frame size.

    ZARA/1 already caps a single payload frame at 1 MiB, so allowing a 5 MiB
    ZeroMQ frame only expands the pre-parser attack/memory surface. Voice uses
    fixed 1 KiB PCM frames and benefits more from a short poll interval than
    from larger messages.
    """

    return TransportConfig(
        sndhwm=256,
        rcvhwm=256,
        max_message_bytes=1024 * 1024,
        heartbeat_interval_ms=5_000,
        heartbeat_timeout_ms=15_000,
        linger_ms=0,
        request_timeout=5.0,
        poll_interval_ms=1,
        event_queue_size=256,
        pending_request_limit=256,
        idempotency_cache_size=512,
    )


def _accepts_keyword(callable_, name: str) -> bool:
    """Return whether *callable_* accepts a named keyword or arbitrary kwargs."""

    try:
        parameters = inspect.signature(callable_).parameters.values()
    except (TypeError, ValueError):
        return True
    return any(
        parameter.kind is inspect.Parameter.VAR_KEYWORD or parameter.name == name
        for parameter in parameters
    )


def _outer_frame_limit(limits) -> int:
    # route + protocol marker + envelope + bounded payload frames
    return 1 + 2 + limits.max_payload_frames


def _outer_byte_limit(limits) -> int:
    # Routing ids are bounded by libzmq. The application budget then mirrors the
    # protocol's strict marker/envelope/aggregate-payload ceilings.
    return (
        _MAX_ROUTING_ID_BYTES
        + len(PROTOCOL_MARKER)
        + limits.max_envelope_bytes
        + limits.max_payload_bytes
    )


def _recv_bounded_multipart(
    socket,
    *,
    max_frames: int,
    max_bytes: int,
):
    """Receive one multipart without retaining bytes beyond hard protocol caps.

    ``recv_multipart`` materializes every part before application code can count
    it. A hostile peer can therefore turn a tiny-frame multipart into a memory
    spike despite per-frame MAXMSGSIZE. Read parts incrementally instead: retain
    only the bounded prefix, drain the rest so the ZMQ socket remains aligned,
    and return ``overflow=True`` for fail-closed handling.
    """

    frames = []
    total_bytes = 0
    overflow = False
    while True:
        frame = socket.recv(copy=False)
        frame_bytes = len(frame)
        if not overflow and len(frames) < max_frames and total_bytes + frame_bytes <= max_bytes:
            frames.append(frame)
            total_bytes += frame_bytes
        else:
            overflow = True
        if not socket.getsockopt(zmq.RCVMORE):
            break
    return frames, overflow


class _PreloadedSocket:
    """Pass one validated outer multipart into the existing ZARA/1 parser."""

    def __init__(self, socket: zmq.Socket, frames: list[bytes]) -> None:
        self._socket = socket
        self._frames = frames

    def recv_multipart(self):
        return self._frames

    def send_multipart(self, *args, **kwargs):
        return self._socket.send_multipart(*args, **kwargs)


class HardenedZaraZmqGateway(ZaraZmqGateway):
    """Fail-closed ROUTER with bounded outer framing and no silent overflow."""

    def __init__(self, endpoint: str, **kwargs) -> None:
        parent_init = super().__init__
        if _accepts_keyword(parent_init, "config"):
            if kwargs.get("config") is None:
                kwargs["config"] = hardened_transport_config()
        else:
            kwargs.pop("config", None)
        parent_init(endpoint, **kwargs)

    def _receive(self, socket: zmq.Socket) -> None:
        raw_frames, overflow = _recv_bounded_multipart(
            socket,
            max_frames=_outer_frame_limit(self._limits),
            max_bytes=_outer_byte_limit(self._limits),
        )
        if not raw_frames:
            logger.warning("dropping empty ZARA/1 multipart")
            return

        route = bytes(raw_frames[0])
        if not route or len(route) > _MAX_ROUTING_ID_BYTES:
            logger.warning("dropping invalid ZMQ routing id (%d bytes)", len(route))
            return
        if overflow:
            self._send(
                socket,
                route,
                _protocol_error(
                    reply_to=None,
                    code="invalid_message",
                    message="protocol multipart exceeds transport limits",
                    retryable=False,
                ),
            )
            return
        if len(raw_frames) < 2:
            logger.warning("dropping ZARA/1 multipart without application frames")
            return

        frames = [route, *(bytes(frame) for frame in raw_frames[1:])]
        try:
            super()._receive(_PreloadedSocket(socket, frames))
        except RecursionError:
            self._send(
                socket,
                route,
                _protocol_error(
                    reply_to=None,
                    code="invalid_message",
                    message="protocol message nesting is too deep",
                    retryable=False,
                ),
            )

    def _enqueue_outbound(
        self,
        route: bytes,
        message: ProtocolMessage,
        payloads=(),
    ) -> bool:
        """Never silently corrupt an event stream when a route is saturated."""

        with self._lock:
            outbound = self._route_outbound.get(route)
            if route not in self._routes:
                return False
            overflow = outbound is not None and len(outbound) >= self._config.event_queue_size
            if not overflow:
                # Keep admission and append atomic. The lock is re-entrant, so a
                # concurrent producer cannot trigger the legacy drop-oldest path.
                return super()._enqueue_outbound(route, message, payloads)

        logger.error(
            "ZARA/1 route overflow; failing route instead of dropping %s",
            message.type,
        )
        self._drop_route(route)
        return False


class HardenedZmqZaraClient(ZmqZaraClient):
    """DEALER client with bounded request lifetime and non-blocking FIFO sends."""

    def __init__(self, endpoint: str, **kwargs) -> None:
        if kwargs.get("config") is None:
            kwargs["config"] = hardened_transport_config()
        super().__init__(endpoint, **kwargs)
        self._retry_outbound: Optional[_ClientOutbound] = None
        self._request_deadlines: dict[str, float] = {}

    def start(self) -> concurrent.futures.Future:
        # Base start() is intentionally idempotent for READY/STARTING clients.
        # Reset retry/deadline state only for a genuinely fresh owner loop;
        # otherwise an innocent repeated start() can unbound in-flight requests.
        if not self.is_alive:
            self._retry_outbound = None
            with self._pending_lock:
                self._request_deadlines.clear()
        return super().start()

    def _request(self, message, kind, *, payloads=()):
        # Validate at the caller boundary so malformed local input cannot poison
        # the owner thread and fail unrelated requests.
        encode_message(message, payloads=payloads, limits=self._limits)
        future = super()._request(message, kind, payloads=payloads)
        with self._pending_lock:
            self._request_deadlines[message.id] = (
                time.monotonic() + float(self._config.request_timeout)
            )
        return future

    def _expire_pending_requests(self) -> None:
        now = time.monotonic()
        expired: list[concurrent.futures.Future] = []
        with self._pending_lock:
            for request_id, deadline in tuple(self._request_deadlines.items()):
                pending = self._pending.get(request_id)
                if pending is None:
                    self._request_deadlines.pop(request_id, None)
                    continue
                if now < deadline:
                    continue
                self._request_deadlines.pop(request_id, None)
                self._pending.pop(request_id, None)
                expired.append(pending.future)
        for future in expired:
            if not future.done():
                future.set_exception(
                    ClientRequestTimeout(
                        f"ZARA/1 request timed out after {self._config.request_timeout:g}s"
                    )
                )

    def _request_is_live(self, request_id: str) -> bool:
        with self._pending_lock:
            return request_id in self._pending

    def _drain_client_outbound(self, socket: zmq.Socket) -> None:
        self._expire_pending_requests()
        for _ in range(_OUTBOUND_BATCH):
            outbound = self._retry_outbound
            if outbound is None:
                try:
                    outbound = self._outbound.get_nowait()
                except queue.Empty:
                    return

            # A queued request can expire while the owner actor is backpressured.
            # Do not transmit stale work after its caller has already failed it.
            if not self._request_is_live(outbound.message.id):
                self._retry_outbound = None
                continue

            frames = encode_message(
                outbound.message,
                payloads=outbound.payloads,
                limits=self._limits,
            )
            try:
                socket.send_multipart(frames, flags=zmq.NOBLOCK)
            except zmq.Again:
                # Preserve exact FIFO ordering. The owner loop returns to poll,
                # then retries this same item before dequeuing another request.
                self._retry_outbound = outbound
                return
            self._retry_outbound = None


__all__ = [
    "ClientRequestTimeout",
    "HardenedZaraZmqGateway",
    "HardenedZmqZaraClient",
    "hardened_transport_config",
]
