"""Authenticated CURVE/ZAP gateway for remote Zara service traffic.

The secure gateway reuses the ZARA/1 ROUTER implementation from issue #129,
but transport routing ids never become principals. ZAP ``User-Id`` metadata
is resolved through the live security registry on every application message so
revocation takes effect without restarting the daemon.
"""

from __future__ import annotations

import concurrent.futures
import logging
import time
from typing import Optional

import zmq

from zara.node import NodeAuthorityError, ZaraNode, verify_authenticated_node
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
from zara.zmq_transport import (
    TransportConfig,
    ZaraZmqGateway,
    _ApprovalOwner,
    apply_socket_options,
)

logger = logging.getLogger(__name__)


class _PreloadedSocket:
    """Pass one already-received message through the base gateway parser."""

    def __init__(self, socket: zmq.Socket, frames: list[bytes], *, before_send=None) -> None:
        self._socket = socket
        self._frames = frames
        self._before_send = before_send

    def recv_multipart(self):
        return self._frames

    def send_multipart(self, *args, **kwargs):
        if self._before_send is not None:
            self._before_send()
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
        voice_ingress=None,
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
            voice_ingress=voice_ingress,
        )
        self._security_registry = security_registry
        self._curve_server = curve_server
        self._security_limits = security_limits or SecurityLimits()
        self._quotas = self._security_limits.new_quota_manager()
        self._audit_log = audit_log or SecurityAuditLog()
        self._route_user_ids: dict[bytes, str] = {}
        self._route_principal_ids: dict[bytes, str] = {}
        self._route_nodes: dict[bytes, ZaraNode] = {}
        self._pending_hello_nodes: dict[bytes, Optional[ZaraNode]] = {}
        self._runtime_quota_holds: set[tuple[str, str]] = set()
        self._hello_route_resets: set[bytes] = set()
        self._principal_subscriptions = {}

    @staticmethod
    def _capability_for(message_type: str) -> Capability:
        if message_type in {
            "hello",
            "ping",
            "conversation.open",
            "capability.snapshot",
            "device.action.accepted",
            "device.action.result",
            "device.action.error",
        }:
            return Capability.SESSION_BASIC
        if message_type == "runtime.status":
            return Capability.RUNTIME_STATUS
        if message_type in {
            "turn.submit",
            "audio.input.start",
            "audio.input.chunk",
            "audio.input.commit",
        }:
            return Capability.TURN_SUBMIT
        if message_type in {"turn.cancel", "audio.input.cancel"}:
            return Capability.TURN_CANCEL
        if message_type in {"tool.approve", "tool.reject"}:
            return Capability.TOOL_APPROVE
        raise AuthorizationDenied("unknown daemon message capability")

    def _ensure_principal_subscription(self, principal: PrincipalContext) -> None:
        principal_id = principal.principal_id
        if principal_id in self._principal_subscriptions:
            return
        try:
            subscription = self._supervisor.subscribe(
                principal,
                maxsize=self._config.event_queue_size,
            )
        except KeyError:
            self._supervisor.open_principal(principal)
            subscription = self._supervisor.subscribe(
                principal,
                maxsize=self._config.event_queue_size,
            )
        self._principal_subscriptions[principal_id] = subscription

    def _route_ready(self, route, _state) -> None:
        self._ensure_principal_subscription(self._principal)
        with self._lock:
            if route not in self._pending_hello_nodes:
                return
            node = self._pending_hello_nodes[route]
            if node is None:
                self._route_nodes.pop(route, None)
            else:
                self._route_nodes[route] = node

    def node_for_session(self, principal_id: str, session_id: str) -> Optional[ZaraNode]:
        """Return peer metadata only for one live authenticated principal/session."""

        with self._lock:
            match = self._route_for_session_locked(principal_id, session_id)
            if match is None:
                return None
            route, _state = match
            node = self._route_nodes.get(route)
            user_id = self._route_user_ids.get(route)
        if node is None or user_id is None:
            return None

        try:
            enrolled = self._security_registry.resolve_user_id(user_id)
            verify_authenticated_node(node, enrolled)
        except (KeyNotActive, NodeAuthorityError, TypeError, ValueError):
            return None

        # Do not hold the gateway lock while resolving the registry: inbound
        # authentication takes the registry lock before touching route state.
        # Re-check the route after resolution so a concurrent reconnect cannot
        # make a formerly valid node record authoritative for a new session.
        with self._lock:
            current = self._route_for_session_locked(principal_id, session_id)
            if current is None or current[0] != route:
                return None
            if self._route_nodes.get(route) != node:
                return None
        return node

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

    def _release_runtime_quota_key(self, key: tuple[str, str]) -> None:
        with self._lock:
            if key not in self._runtime_quota_holds:
                return
            self._runtime_quota_holds.remove(key)
        self._quotas.release_request(key[0])

    def _release_runtime_quota(self, route: bytes, message: ProtocolMessage) -> None:
        if message.reply_to is None:
            return
        principal_id = self._route_principal_ids.get(route)
        if principal_id is None:
            return
        self._release_runtime_quota_key((principal_id, message.reply_to))

    def _remember_response(self, key, command, response) -> None:
        # Completion is authoritative for the concurrent-request hold even when
        # every requesting route disappeared before the result became deliverable.
        # Releasing only from _enqueue_outbound leaks quota forever in that case.
        self._release_runtime_quota_key(key)
        super()._remember_response(key, command, response)

    def _enqueue_outbound(
        self,
        route: bytes,
        message: ProtocolMessage,
        payloads=(),
    ) -> bool:
        # Runtime completions are delivered asynchronously by the base gateway.
        # The _remember_response completion hook releases the quota even when no
        # route survives; this remains as a harmless fallback for old paths.
        self._release_runtime_quota(route, message)
        return super()._enqueue_outbound(route, message, payloads)

    def _release_turn_events_after_accept(self, route: bytes, turn_id: str) -> None:
        """Bind buffered approval events to the authenticated session before release."""

        with self._lock:
            state = self._routes.get(route)
            if state is None or not state.ready:
                return
            key = (state.principal_id, turn_id)
            if self._turn_routes.get(key) != route:
                return
            held = self._early_turn_events.get(key)
            if held is None:
                return super()._release_turn_events_after_accept(route, turn_id)

            filtered = []
            terminal_types = {
                "tool.started",
                "tool.completed",
                "tool.failed",
                "tool.cancelled",
            }
            for held_message, held_payloads in held:
                body = held_message.body or {}
                tool_run_id = body.get("tool_run_id")
                if held_message.type == "tool.waiting" and isinstance(tool_run_id, str):
                    owner_key = (state.principal_id, tool_run_id)
                    if (
                        owner_key not in self._approval_owners
                        and len(self._approval_owners) >= self._config.pending_request_limit
                    ):
                        continue
                    self._approval_owners[owner_key] = _ApprovalOwner(
                        route=route,
                        session_id=state.session_id,
                    )
                elif held_message.type in terminal_types and isinstance(tool_run_id, str):
                    self._approval_owners.pop((state.principal_id, tool_run_id), None)
                filtered.append((held_message, held_payloads))
            held.clear()
            held.extend(filtered)

        super()._release_turn_events_after_accept(route, turn_id)

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
                for principal_id, subscription in tuple(self._principal_subscriptions.items()):
                    self._drain_runtime_subscription(
                        socket,
                        subscription,
                        principal_id=principal_id,
                    )
                self._drain_outbound(socket)
                ready = dict(poller.poll(self._config.poll_interval_ms))
                if ready.get(socket) == zmq.POLLIN:
                    self._receive(socket)
        except BaseException as error:
            logger.exception("secure gateway poller crashed")
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
            self._route_nodes.clear()
            self._pending_hello_nodes.clear()
            self._hello_route_resets.clear()
            for subscription in tuple(self._principal_subscriptions.values()):
                subscription.close()
            self._principal_subscriptions.clear()
            socket.close(self._config.linger_ms)
            authenticator.stop()

    def _drop_route_locked(self, route: bytes):
        resetting = route in self._hello_route_resets
        principal_id = self._route_principal_ids.get(route)
        self._route_nodes.pop(route, None)
        if not resetting:
            principal_id = self._route_principal_ids.pop(route, None)
            self._route_user_ids.pop(route, None)
            if principal_id is not None:
                self._quotas.release_connection(principal_id)
        state = super()._drop_route_locked(route)
        if (
            not resetting
            and principal_id is not None
            and not any(
                route_state.principal_id == principal_id
                for route_state in self._routes.values()
            )
        ):
            subscription = self._principal_subscriptions.pop(principal_id, None)
            if subscription is not None:
                subscription.close()
        return state

    def _handle_hello(self, socket: zmq.Socket, route: bytes, message: ProtocolMessage) -> None:
        # The base gateway resets application/session state on every hello so an
        # abandoned voice stream is cancelled. A secure route is already bound
        # and charged to its authenticated principal before that reset. Preserve
        # the security binding/quota while allowing only the base route state to
        # be replaced.
        self._hello_route_resets.add(route)
        try:
            super()._handle_hello(socket, route, message)
        finally:
            self._hello_route_resets.discard(route)

    def _replace_prior_credential_routes(self, route: bytes, user_id: str) -> None:
        # ROUTER does not provide a portable routing-id-aware disconnect event.
        # A restarted device may therefore arrive with a new route while its old
        # route is still present in application bookkeeping. The cryptographic
        # ZAP User-Id is the stronger identity: one enrolled credential may own
        # only one current route. A fresh authenticated hello supersedes stale
        # routes for that same credential before connection quota is charged.
        stale_routes = [
            candidate
            for candidate, bound_user_id in tuple(self._route_user_ids.items())
            if candidate != route and bound_user_id == user_id
        ]
        for candidate in stale_routes:
            self._drop_route(candidate)

    @staticmethod
    def _node_from_hello(message: ProtocolMessage, enrolled) -> Optional[ZaraNode]:
        body = message.body or {}
        if "node" not in body:
            return None
        raw_node = body["node"]
        if not isinstance(raw_node, dict):
            raise ValueError("peer node descriptor must be an object")
        return verify_authenticated_node(ZaraNode.from_mapping(raw_node), enrolled)

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
        except (ZaraProtocolError, RecursionError):
            try:
                self._quotas.consume_request_rate(principal_id)
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
                    message="invalid protocol message",
                ),
            )
            return

        started_ns = time.monotonic_ns()
        request_slot_acquired = message.type != "hello"
        try:
            if request_slot_acquired:
                self._quotas.acquire_request(principal_id)
            else:
                # Reconnect must remain possible while an earlier runtime turn
                # still owns the principal's concurrent slot. The handshake is
                # still rate-limited, and connection quota is enforced below.
                self._quotas.consume_request_rate(principal_id)
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
            if request_slot_acquired:
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

        peer_node: Optional[ZaraNode] = None
        if message.type == "hello":
            try:
                peer_node = self._node_from_hello(message, enrolled)
            except NodeAuthorityError:
                self._audit(
                    enrolled=enrolled,
                    message=message,
                    decision="deny",
                    error_class="node_authority_mismatch",
                    started_ns=started_ns,
                )
                self._send(
                    socket,
                    route,
                    _security_error(
                        reply_to=message.id,
                        code="node_authority_mismatch",
                        message="peer node does not match authenticated identity",
                    ),
                )
                return
            except (TypeError, ValueError):
                self._audit(
                    enrolled=enrolled,
                    message=message,
                    decision="deny",
                    error_class="invalid_node",
                    started_ns=started_ns,
                )
                self._send(
                    socket,
                    route,
                    _security_error(
                        reply_to=message.id,
                        code="invalid_node",
                        message="peer node descriptor is invalid",
                    ),
                )
                return

        if message.type == "hello" and route not in self._route_principal_ids:
            self._replace_prior_credential_routes(route, enrolled.user_id)
            try:
                self._quotas.acquire_connection(principal_id)
            except QuotaExceeded:
                if request_slot_acquired:
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
                if request_slot_acquired:
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

        previous_session_id = None
        if message.type == "hello":
            with self._lock:
                previous_state = self._routes.get(route)
                if previous_state is not None:
                    previous_session_id = previous_state.session_id

        if message.type == "hello":
            with self._lock:
                self._pending_hello_nodes[route] = peer_node

        def commit_peer_node() -> None:
            if message.type != "hello":
                return
            with self._lock:
                current = self._routes.get(route)
                if (
                    current is None
                    or not current.ready
                    or current.session_id == previous_session_id
                ):
                    return
                if peer_node is None:
                    self._route_nodes.pop(route, None)
                else:
                    self._route_nodes[route] = peer_node

        previous_principal = self._principal
        self._principal = enrolled.principal
        try:
            super()._receive(
                _PreloadedSocket(
                    socket,
                    frames,
                    before_send=commit_peer_node if message.type == "hello" else None,
                )
            )
        finally:
            if message.type == "hello":
                with self._lock:
                    self._pending_hello_nodes.pop(route, None)
            self._principal = previous_principal

        replay_key = (principal_id, message.id)
        with self._lock:
            runtime_pending = (
                message.type in {"turn.submit", "turn.cancel", "tool.approve", "tool.reject"}
                and replay_key in self._inflight
            )
            if runtime_pending:
                self._runtime_quota_holds.add(replay_key)

        if request_slot_acquired and not runtime_pending:
            self._quotas.release_request(principal_id)

        self._audit(
            enrolled=enrolled,
            message=message,
            decision="allow",
            error_class=None,
            started_ns=started_ns,
        )


__all__ = ["SecureZaraZmqGateway"]
