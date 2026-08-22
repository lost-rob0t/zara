"""Authenticated CURVE/ZAP gateway for remote Zara service traffic.

The secure gateway reuses the ZARA/1 ROUTER implementation from issue #129,
but transport routing ids never become principals. ZAP ``User-Id`` metadata
is resolved through the live security registry on every application message so
revocation takes effect without restarting the daemon.
"""

from __future__ import annotations

import concurrent.futures
import time
from typing import Optional

import zmq

from zara.protocol import ProtocolMessage
from zara.security import (
    AuthorizationDenied,
    Capability,
    KeyNotActive,
    QuotaExceeded,
    SecurityAuditLog,
    SecurityAuditRecord,
    SecurityLimits,
    SecurityRegistry,
    authorize,
    validate_listener_security,
)
from zara.security_transport import (
    AuthenticationRequired,
    CurveServerConfig,
    RegistryAuthenticator,
    authenticated_user_id,
    configure_curve_server_socket,
)
from zara.server import PrincipalContext
from zara.zmq_transport import TransportConfig, ZaraZmqGateway, apply_socket_options


class _PreloadedSocket:
    """Pass one already-received message through the base gateway parser."""

    def __init__(self, socket: zmq.Socket, frames: list[bytes]) -> None:
        self._socket = socket
        self._frames = frames

    def recv_multipart(self):
        return self._frames

    def send_multipart(self, *args, **kwargs):
        return self._socket.send_multipart(*args, **kwargs)


def _security_error(
    *,
    reply_to: Optional[str],
    code: str,
    message: str,
) -> ProtocolMessage:
    from zara.zmq_transport import _message_id, _now_ns

    return ProtocolMessage(
        type="protocol.error",
        id=_message_id(),
        reply_to=reply_to,
        timestamp_ns=_now_ns(),
        payload_count=0,
        body={"code": code, "message": message, "retryable": False},
    )


class SecureZaraZmqGateway(ZaraZmqGateway):
    """ROUTER gateway with mandatory CURVE/ZAP identity and live policy checks."""

    def __init__(
        self,
        endpoint: str,
        *,
        supervisor,
        security_registry: SecurityRegistry,
        curve_server: CurveServerConfig,
        security_limits: Optional[SecurityLimits] = None,
        audit_log: Optional[SecurityAuditLog] = None,
        context: Optional[zmq.Context] = None,
        config: Optional[TransportConfig] = None,
        limits=None,
    ) -> None:
        if not isinstance(security_registry, SecurityRegistry):
            raise TypeError("security_registry must be SecurityRegistry")
        if not isinstance(curve_server, CurveServerConfig):
            raise TypeError("curve_server must be CurveServerConfig")
        if audit_log is not None and not isinstance(audit_log, SecurityAuditLog):
            raise TypeError("audit_log must be SecurityAuditLog")
        validate_listener_security(endpoint, curve_enabled=True, zap_enabled=True)
        super().__init__(
            endpoint,
            supervisor=supervisor,
            principal=PrincipalContext("secure:unbound", kind="internal"),
            context=context,
            config=config,
            limits=limits,
        )
        self._security_registry = security_registry
        self._curve_server = curve_server
        self._security_limits = security_limits or SecurityLimits()
        self._quotas = self._security_limits.new_quota_manager()
        self._audit_log = audit_log or SecurityAuditLog()
        self._route_user_ids: dict[bytes, str] = {}
        self._route_principal_ids: dict[bytes, str] = {}
        self._runtime_quota_holds: set[tuple[str, str]] = set()

    @staticmethod
    def _capability_for(message_type: str) -> Capability:
        if message_type in {"hello", "ping", "conversation.open"}:
            return Capability.SESSION_BASIC
        if message_type == "runtime.status":
            return Capability.RUNTIME_STATUS
        if message_type == "turn.submit":
            return Capability.TURN_SUBMIT
        if message_type == "turn.cancel":
            return Capability.TURN_CANCEL
        return Capability.SESSION_BASIC

    def _audit(
        self,
        *,
        enrolled,
        message: ProtocolMessage,
        decision: str,
        error_class: Optional[str],
        started_ns: int,
    ) -> None:
        self._audit_log.append(
            SecurityAuditRecord(
                timestamp_ns=time.time_ns(),
                principal_id=enrolled.principal.principal_id,
                device_id=enrolled.device_id,
                session_id=message.session_id,
                request_id=message.id,
                turn_id=message.turn_id,
                action=message.type,
                decision=decision,
                error_class=error_class,
                duration_ns=max(0, time.monotonic_ns() - started_ns),
            )
        )

    def _release_runtime_quota(self, route: bytes, message: ProtocolMessage) -> None:
        if message.reply_to is None:
            return
        principal_id = self._route_principal_ids.get(route)
        if principal_id is None:
            return
        key = (principal_id, message.reply_to)
        with self._lock:
            if key not in self._runtime_quota_holds:
                return
            self._runtime_quota_holds.remove(key)
        self._quotas.release_request(principal_id)

    def _enqueue_outbound(self, route: bytes, message: ProtocolMessage) -> bool:
        # Runtime completions are delivered asynchronously by the base gateway.
        # Release the principal's concurrent-request slot exactly once when that
        # completion becomes available, even when the route has since stalled.
        self._release_runtime_quota(route, message)
        return super()._enqueue_outbound(route, message)

    def _run(self) -> None:
        authenticator = RegistryAuthenticator(
            context=self._context,
            registry=self._security_registry,
            domain=self._curve_server.zap_domain,
        )
        socket = self._context.socket(zmq.ROUTER)
        apply_socket_options(socket, self._config, router=True)
        configure_curve_server_socket(socket, self._curve_server)
        try:
            authenticator.start()
            socket.bind(self._endpoint)
            self._started.set_result(True)
            poller = zmq.Poller()
            poller.register(socket, zmq.POLLIN)
            while not self._stop.is_set():
                self._drain_outbound(socket)
                ready = dict(poller.poll(self._config.poll_interval_ms))
                if ready.get(socket) == zmq.POLLIN:
                    self._receive(socket)
        except BaseException as error:
            if not self._started.done():
                self._started.set_exception(error)
        finally:
            for principal_id in tuple(self._route_principal_ids.values()):
                self._quotas.release_connection(principal_id)
            for principal_id, _request_id in tuple(self._runtime_quota_holds):
                self._quotas.release_request(principal_id)
            self._runtime_quota_holds.clear()
            self._route_user_ids.clear()
            self._route_principal_ids.clear()
            socket.close(self._config.linger_ms)
            authenticator.stop()

    def _drop_route_locked(self, route: bytes) -> None:
        principal_id = self._route_principal_ids.pop(route, None)
        self._route_user_ids.pop(route, None)
        if principal_id is not None:
            self._quotas.release_connection(principal_id)
        super()._drop_route_locked(route)

    def _receive(self, socket: zmq.Socket) -> None:
        raw_frames = socket.recv_multipart(copy=False)
        if len(raw_frames) < 2:
            return
        frames = [bytes(frame) for frame in raw_frames]
        route = frames[0]

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

        principal_id = enrolled.principal.principal_id
        from zara.protocol import ZaraProtocolError, decode_message

        try:
            message = decode_message(frames[1:], limits=self._limits).message
        except ZaraProtocolError:
            try:
                self._quotas.acquire_request(principal_id)
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
            self._quotas.release_request(principal_id)
            super()._receive(_PreloadedSocket(socket, frames))
            return

        started_ns = time.monotonic_ns()
        try:
            self._quotas.acquire_request(principal_id)
        except QuotaExceeded:
            self._audit(
                enrolled=enrolled,
                message=message,
                decision="deny",
                error_class="quota_exceeded",
                started_ns=started_ns,
            )
            self._send(
                socket,
                route,
                _security_error(
                    reply_to=message.id,
                    code="quota_exceeded",
                    message="resource quota exceeded",
                ),
            )
            return

        try:
            authorize(enrolled, self._capability_for(message.type))
        except (AuthorizationDenied, KeyNotActive):
            self._quotas.release_request(principal_id)
            self._audit(
                enrolled=enrolled,
                message=message,
                decision="deny",
                error_class="authorization_denied",
                started_ns=started_ns,
            )
            self._send(
                socket,
                route,
                _security_error(
                    reply_to=message.id,
                    code="authorization_denied",
                    message="request is not authorized",
                ),
            )
            return

        if message.type == "hello" and route not in self._route_principal_ids:
            try:
                self._quotas.acquire_connection(principal_id)
            except QuotaExceeded:
                self._quotas.release_request(principal_id)
                self._audit(
                    enrolled=enrolled,
                    message=message,
                    decision="deny",
                    error_class="quota_exceeded",
                    started_ns=started_ns,
                )
                self._send(
                    socket,
                    route,
                    _security_error(
                        reply_to=message.id,
                        code="quota_exceeded",
                        message="resource quota exceeded",
                    ),
                )
                return
            self._route_user_ids[route] = enrolled.user_id
            self._route_principal_ids[route] = principal_id
        else:
            bound_user_id = self._route_user_ids.get(route)
            if bound_user_id is not None and bound_user_id != enrolled.user_id:
                self._quotas.release_request(principal_id)
                self._drop_route(route)
                self._audit(
                    enrolled=enrolled,
                    message=message,
                    decision="deny",
                    error_class="authentication_required",
                    started_ns=started_ns,
                )
                self._send(
                    socket,
                    route,
                    _security_error(
                        reply_to=message.id,
                        code="authentication_required",
                        message="authentication required",
                    ),
                )
                return

        previous_principal = self._principal
        self._principal = enrolled.principal
        try:
            super()._receive(_PreloadedSocket(socket, frames))
        finally:
            self._principal = previous_principal

        replay_key = (principal_id, message.id)
        with self._lock:
            runtime_pending = (
                message.type in {"turn.submit", "turn.cancel"}
                and replay_key in self._inflight
            )
            if runtime_pending:
                self._runtime_quota_holds.add(replay_key)

        if not runtime_pending:
            self._quotas.release_request(principal_id)

        self._audit(
            enrolled=enrolled,
            message=message,
            decision="allow",
            error_class=None,
            started_ns=started_ns,
        )


__all__ = ["SecureZaraZmqGateway"]
