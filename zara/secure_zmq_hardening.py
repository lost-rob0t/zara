"""Fail-closed transport policy for authenticated remote ZARA/1 traffic."""

from __future__ import annotations

import logging

import zmq

from zara.security import KeyNotActive, QuotaExceeded
from zara.security_gateway import SecureZaraZmqGateway, _security_error
from zara.security_transport import AuthenticationRequired, authenticated_user_id
from zara.zmq_hardening import (
    _MAX_ROUTING_ID_BYTES,
    _outer_byte_limit,
    _outer_frame_limit,
    _recv_bounded_multipart,
)

logger = logging.getLogger(__name__)


class _PreloadedRawSocket:
    """Replay already-received pyzmq frames into the authenticated parser."""

    def __init__(self, socket: zmq.Socket, frames) -> None:
        self._socket = socket
        self._frames = frames

    def recv_multipart(self, *, copy=False):
        assert copy is False
        return self._frames

    def send_multipart(self, *args, **kwargs):
        return self._socket.send_multipart(*args, **kwargs)


class HardenedSecureZaraZmqGateway(SecureZaraZmqGateway):
    """CURVE/ZAP gateway with bounded multipart allocation and fail-closed queues."""

    def _receive(self, socket: zmq.Socket) -> None:
        raw_frames, overflow = _recv_bounded_multipart(
            socket,
            max_frames=_outer_frame_limit(self._limits),
            max_bytes=_outer_byte_limit(self._limits),
        )
        if not raw_frames:
            logger.warning("dropping empty authenticated ZARA/1 multipart")
            return

        route = bytes(raw_frames[0])
        if not route or len(route) > _MAX_ROUTING_ID_BYTES:
            logger.warning("dropping invalid authenticated routing id (%d bytes)", len(route))
            return
        if not overflow:
            if len(raw_frames) < 2:
                return
            return super()._receive(_PreloadedRawSocket(socket, raw_frames))

        # ZAP has authenticated the connection, but retain the existing live
        # registry lookup and malformed-request rate accounting before replying.
        try:
            user_id = authenticated_user_id(raw_frames)
            enrolled = self._security_registry.resolve_user_id(user_id)
        except (AuthenticationRequired, KeyNotActive, ValueError, TypeError):
            self._send(
                socket,
                route,
                _security_error(
                    reply_to=None,
                    code="authentication_required",
                    message="authentication required",
                ),
            )
            return

        try:
            self._quotas.consume_request_rate(enrolled.principal.principal_id)
        except QuotaExceeded:
            self._send(
                socket,
                route,
                _security_error(
                    reply_to=None,
                    code="quota_exceeded",
                    message="resource quota exceeded",
                ),
            )
            return

        self._send(
            socket,
            route,
            _security_error(
                reply_to=None,
                code="invalid_message",
                message="protocol multipart exceeds transport limits",
            ),
        )

    def _enqueue_outbound(self, route: bytes, message) -> bool:
        with self._lock:
            outbound = self._route_outbound.get(route)
            overflow = (
                route in self._routes
                and outbound is not None
                and len(outbound) >= self._config.event_queue_size
            )
            if not overflow:
                return super()._enqueue_outbound(route, message)

        # Secure gateway completion paths normally release the request hold in
        # _remember_response. Keep its fallback release before failing the route.
        self._release_runtime_quota(route, message)
        logger.error(
            "authenticated ZARA/1 route overflow; failing route instead of dropping %s",
            message.type,
        )
        self._drop_route(route)
        return False


__all__ = ["HardenedSecureZaraZmqGateway"]
