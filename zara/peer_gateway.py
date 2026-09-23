"""Peer-call lifecycle mixin for the existing authenticated ZARA/1 gateway.

This module owns no socket, auth database, runtime, scheduler, or durable task
state.  It composes the existing SecureZaraZmqGateway route/session authority,
ZaraZmqGateway replay/inflight maps, and RuntimeSupervisor command/event seams.
"""

from __future__ import annotations

import concurrent.futures
import time
from dataclasses import dataclass
from typing import Optional

from zara.node import NodeAuthorityError
from zara.peer_authority import peer_authority_from_session
from zara.peer_protocol import (
    PeerAuthority,
    PeerCallAdmissionError,
    PeerCallRequest,
    PeerCancelRequest,
    PeerRemoteError,
    PeerResult,
    admit_peer_call,
    project_remote_error,
    require_live_runtime_budget,
)
from zara.protocol import ProtocolMessage
from zara.runtime import events
from zara.runtime.commands import CancelTurn, CommandReceipt, SubmitTurn
from zara.security import KeyNotActive
from zara.server import PrincipalContext
from zara.zmq_transport import (
    _InflightEntry,
    _RequestRoute,
    _message_id,
    _now_ns,
)


@dataclass
class _PeerCallState:
    replay_key: tuple[str, str]
    command: SubmitTurn
    request: PeerCallRequest
    authority: PeerAuthority
    principal: PrincipalContext
    session_id: str
    route: bytes
    turn_id: str
    deadline_ns: int
    gateway_generation: int
    final_text: str = ""
    streamed_bytes: int = 0


class PeerCallGatewayMixin:
    """Authenticated peer-call projection over one existing ZARA/1 gateway."""

    def _init_peer_gateway(
        self,
        *,
        curve_public_key: str | bytes,
        local_node_id: Optional[str] = None,
        runtime_id: str = "zara-runtime",
    ) -> None:
        identity = local_node_id
        if identity is None:
            identity = curve_public_key.decode("ascii") if isinstance(curve_public_key, bytes) else curve_public_key
        self._peer_local_node_id = self._peer_token("local_node_id", identity)
        self._peer_runtime_id = self._peer_token("runtime_id", runtime_id)
        self._peer_calls_by_turn: dict[tuple[str, str], _PeerCallState] = {}
        self._peer_calls_by_request: dict[tuple[str, str], _PeerCallState] = {}

    @staticmethod
    def _peer_token(name: str, value: object) -> str:
        if (
            not isinstance(value, str)
            or not value
            or value != value.strip()
            or any(character.isspace() for character in value)
            or any(ord(character) < 0x20 or ord(character) == 0x7F for character in value)
            or len(value.encode("utf-8")) > 128
        ):
            raise ValueError(f"{name} must be a bounded canonical token")
        return value

    def _peer_authority_for_route(self, state, route: bytes) -> PeerAuthority:
        node = self.node_for_session(state.principal_id, state.session_id)
        if node is None:
            raise PeerCallAdmissionError("authenticated peer node is unavailable")
        user_id = self._route_user_ids.get(route)
        if user_id is None:
            raise PeerCallAdmissionError("authenticated peer credential is unavailable")
        try:
            enrolled = self._security_registry.resolve_user_id(user_id)
            return peer_authority_from_session(
                node=node,
                enrolled=enrolled,
                session_id=state.session_id,
            )
        except (KeyNotActive, NodeAuthorityError, TypeError, ValueError) as error:
            raise PeerCallAdmissionError("authenticated peer authority is stale") from error

    def _peer_result_message(
        self,
        state: _PeerCallState,
        result: PeerResult | PeerRemoteError,
    ) -> ProtocolMessage:
        return ProtocolMessage(
            type="node.result" if isinstance(result, PeerResult) else "node.error",
            id=_message_id(),
            reply_to=state.request.request_id,
            conversation_id=state.command.conversation_id,
            timestamp_ns=_now_ns(),
            payload_count=0,
            body=result.to_wire_body(),
        )

    def _peer_error(
        self,
        *,
        request_id: str,
        trace_id: str,
        code: str,
        message: str,
        retryable: bool = False,
        generation: Optional[int] = None,
    ) -> PeerRemoteError:
        return PeerRemoteError(
            request_id=request_id,
            code=code,
            message=message,
            retryable=retryable,
            source_node_id=self._peer_local_node_id,
            runtime_id=self._peer_runtime_id,
            runtime_generation=self._generation if generation is None else generation,
            trace_id=trace_id,
        )

    def _send_peer_error(
        self,
        socket,
        route: bytes,
        message: ProtocolMessage,
        *,
        code: str,
        detail: str,
        retryable: bool = False,
        trace_id: Optional[str] = None,
    ) -> None:
        error = self._peer_error(
            request_id=message.id,
            trace_id=trace_id or message.id,
            code=code,
            message=detail,
            retryable=retryable,
        )
        self._send(
            socket,
            route,
            ProtocolMessage(
                type="node.error",
                id=_message_id(),
                reply_to=message.id,
                session_id=message.session_id,
                timestamp_ns=_now_ns(),
                payload_count=0,
                body=error.to_wire_body(),
            ),
        )

    def _finish_peer_call(
        self,
        state: _PeerCallState,
        result: PeerResult | PeerRemoteError,
    ) -> None:
        response = self._peer_result_message(state, result)
        key = (state.principal.principal_id, state.turn_id)
        with self._lock:
            if self._peer_calls_by_turn.get(key) is not state:
                return
            inflight = self._inflight.get(state.replay_key)
            routes = list(inflight.routes) if inflight is not None else []
            live_routes = [
                candidate
                for candidate in routes
                if (
                    (current := self._routes.get(candidate.route)) is not None
                    and current.ready
                    and current.principal_id == candidate.principal_id
                    and current.session_id == candidate.session_id
                )
            ]
            self._peer_calls_by_turn.pop(key, None)
            self._peer_calls_by_request.pop(state.replay_key, None)
            self._inflight.pop(state.replay_key, None)
            self._turn_routes.pop(key, None)
            self._remember_response(state.replay_key, state.command, response)
        for candidate in live_routes:
            self._enqueue_outbound(
                candidate.route,
                self._response_for_route(response, candidate.route),
            )

    def _dispatch_peer_call(self, socket, route: bytes, route_state, message: ProtocolMessage) -> None:
        if message.session_id != route_state.session_id:
            self._send_peer_error(
                socket,
                route,
                message,
                code="unauthorized",
                detail="peer session is stale",
            )
            return
        try:
            request = PeerCallRequest.from_wire(message.type, message.id, message.body or {})
            authority = self._peer_authority_for_route(route_state, route)
            admitted = admit_peer_call(
                request,
                authority=authority,
                local_node_id=self._peer_local_node_id,
                now_ns=time.time_ns(),
            )
            require_live_runtime_budget(request)
            command = admitted.to_runtime_command(
                conversation_id=message.conversation_id or route_state.conversation_id
            )
        except PeerCallAdmissionError as error:
            text = str(error)
            budget = "budget" in text
            authority_failure = "capability" in text or "authority" in text
            self._send_peer_error(
                socket,
                route,
                message,
                code="budget_exceeded" if budget else "capability_denied" if authority_failure else "invalid_request",
                detail=(
                    "peer runtime budget is not enforceable"
                    if budget
                    else "peer runtime request is not authorized"
                    if authority_failure
                    else "peer runtime request is invalid"
                ),
                trace_id=(
                    request.trace_id
                    if "request" in locals() and isinstance(request, PeerCallRequest)
                    else message.id
                ),
            )
            return

        replay_key = self._idempotency_key(message)
        request_route = _RequestRoute(
            route=route,
            principal_id=route_state.principal_id,
            session_id=route_state.session_id,
        )
        with self._lock:
            replay = self._replay.get(replay_key)
            if replay is not None:
                if replay.command != command:
                    self._send_idempotency_conflict(socket, route, message)
                    return
                self._replay.move_to_end(replay_key)
                self._send(socket, route, self._response_for_route(replay.response, route))
                return
            inflight = self._inflight.get(replay_key)
            if inflight is not None:
                if inflight.command != command:
                    self._send_idempotency_conflict(socket, route, message)
                    return
                if request_route not in inflight.routes:
                    inflight.routes.append(request_route)
                return
            if len(self._inflight) >= self._config.pending_request_limit:
                self._send_peer_error(
                    socket,
                    route,
                    message,
                    code="unavailable",
                    detail="peer runtime is temporarily unavailable",
                    retryable=True,
                    trace_id=request.trace_id,
                )
                return
            self._inflight[replay_key] = _InflightEntry(
                command=command,
                routes=[request_route],
            )

        principal = self._principal
        generation = self._generation
        try:
            future = self._supervisor.submit(principal, command)
        except BaseException as error:
            future = concurrent.futures.Future()
            future.set_exception(error)

        deadline_ns = min(
            request.deadline_ns,
            time.time_ns() + request.budget.wall_time_ms * 1_000_000,
        )

        def completed(done: concurrent.futures.Future) -> None:
            try:
                receipt = done.result()
                if not isinstance(receipt, CommandReceipt) or not receipt.turn_id:
                    raise TypeError("runtime returned invalid peer receipt")
            except BaseException as error:
                projected = project_remote_error(
                    error,
                    request_id=request.request_id,
                    source_node_id=self._peer_local_node_id,
                    runtime_id=self._peer_runtime_id,
                    runtime_generation=generation,
                    trace_id=request.trace_id,
                )
                response = ProtocolMessage(
                    type="node.error",
                    id=_message_id(),
                    reply_to=request.request_id,
                    timestamp_ns=_now_ns(),
                    payload_count=0,
                    body=projected.to_wire_body(),
                )
                with self._lock:
                    self._inflight.pop(replay_key, None)
                    self._remember_response(replay_key, command, response)
                self._enqueue_outbound(route, self._response_for_route(response, route))
                return

            with self._lock:
                if generation != self._generation or self._stop.is_set():
                    self._inflight.pop(replay_key, None)
                    self._release_runtime_quota_key(replay_key)
                    return
                current = self._routes.get(route)
                if (
                    current is None
                    or current.session_id != route_state.session_id
                    or current.principal_id != route_state.principal_id
                ):
                    self._inflight.pop(replay_key, None)
                    self._release_runtime_quota_key(replay_key)
                    return
                state = _PeerCallState(
                    replay_key=replay_key,
                    command=command,
                    request=request,
                    authority=authority,
                    principal=principal,
                    session_id=route_state.session_id,
                    route=route,
                    turn_id=receipt.turn_id,
                    deadline_ns=deadline_ns,
                    gateway_generation=generation,
                )
                key = (principal.principal_id, receipt.turn_id)
                self._peer_calls_by_turn[key] = state
                self._peer_calls_by_request[replay_key] = state
                self._turn_routes[key] = route

        future.add_done_callback(completed)

    def _dispatch_peer_cancel(self, socket, route: bytes, route_state, message: ProtocolMessage) -> None:
        if message.session_id != route_state.session_id:
            self._send_peer_error(
                socket,
                route,
                message,
                code="unauthorized",
                detail="peer session is stale",
            )
            return
        try:
            request = PeerCancelRequest.from_wire(message.id, message.body or {})
            authority = self._peer_authority_for_route(route_state, route)
            command = request.to_runtime_command(authority=authority)
        except PeerCallAdmissionError:
            self._send_peer_error(
                socket,
                route,
                message,
                code="unauthorized",
                detail="peer cancellation is not authorized",
            )
            return

        with self._lock:
            call = self._peer_calls_by_request.get(
                (route_state.principal_id, request.call_id)
            )
            if (
                call is None
                or call.turn_id != request.turn_id
                or call.session_id != route_state.session_id
            ):
                self._send_peer_error(
                    socket,
                    route,
                    message,
                    code="invalid_request",
                    detail="peer cancellation target is unknown or stale",
                )
                return

        replay_key = self._idempotency_key(message)
        request_route = _RequestRoute(
            route=route,
            principal_id=route_state.principal_id,
            session_id=route_state.session_id,
        )
        with self._lock:
            replay = self._replay.get(replay_key)
            if replay is not None:
                if replay.command != command:
                    self._send_idempotency_conflict(socket, route, message)
                    return
                self._send(socket, route, self._response_for_route(replay.response, route))
                return
            inflight = self._inflight.get(replay_key)
            if inflight is not None:
                if inflight.command != command:
                    self._send_idempotency_conflict(socket, route, message)
                    return
                if request_route not in inflight.routes:
                    inflight.routes.append(request_route)
                return
            self._inflight[replay_key] = _InflightEntry(
                command=command,
                routes=[request_route],
            )

        generation = self._generation
        try:
            future = self._supervisor.submit(self._principal, command)
        except BaseException as error:
            future = concurrent.futures.Future()
            future.set_exception(error)

        def completed(done: concurrent.futures.Future) -> None:
            try:
                receipt = done.result()
                if not isinstance(receipt, CommandReceipt):
                    raise TypeError("runtime returned invalid cancel receipt")
                response = ProtocolMessage(
                    type="node.cancel.accepted",
                    id=_message_id(),
                    reply_to=message.id,
                    session_id=route_state.session_id,
                    turn_id=request.turn_id,
                    timestamp_ns=_now_ns(),
                    payload_count=0,
                )
            except BaseException as error:
                projected = project_remote_error(
                    error,
                    request_id=message.id,
                    source_node_id=self._peer_local_node_id,
                    runtime_id=self._peer_runtime_id,
                    runtime_generation=generation,
                    trace_id=message.id,
                )
                response = ProtocolMessage(
                    type="node.error",
                    id=_message_id(),
                    reply_to=message.id,
                    timestamp_ns=_now_ns(),
                    payload_count=0,
                    body=projected.to_wire_body(),
                )
            with self._lock:
                self._inflight.pop(replay_key, None)
                self._remember_response(replay_key, command, response)
            self._enqueue_outbound(route, self._response_for_route(response, route))

        future.add_done_callback(completed)

    def _dispatch_runtime(self, socket, route, state, message) -> None:
        if message.type in {"node.ask", "node.delegate"}:
            self._dispatch_peer_call(socket, route, state, message)
            return
        if message.type == "node.cancel":
            self._dispatch_peer_cancel(socket, route, state, message)
            return
        super()._dispatch_runtime(socket, route, state, message)

    def _cancel_peer_runtime(self, state: _PeerCallState) -> None:
        try:
            self._supervisor.submit(
                state.principal,
                CancelTurn(
                    request_id=f"peer-cancel:{state.request.request_id}",
                    turn_id=state.turn_id,
                ),
            )
        except BaseException:
            return

    def _expire_peer_calls(self) -> None:
        now_ns = time.time_ns()
        with self._lock:
            expired = [
                state
                for state in tuple(self._peer_calls_by_turn.values())
                if state.deadline_ns <= now_ns
            ]
        for state in expired:
            self._cancel_peer_runtime(state)
            self._finish_peer_call(
                state,
                self._peer_error(
                    request_id=state.request.request_id,
                    trace_id=state.request.trace_id,
                    code="timeout",
                    message="peer runtime timed out",
                    retryable=True,
                    generation=state.gateway_generation,
                ),
            )

    def _allow_runtime_event(self, principal_id, envelope, route) -> bool:
        event = envelope.event
        turn_id = getattr(event, "turn_id", None)
        if not turn_id:
            return super()._allow_runtime_event(principal_id, envelope, route)
        with self._lock:
            state = self._peer_calls_by_turn.get((principal_id, turn_id))
        if state is None:
            return super()._allow_runtime_event(principal_id, envelope, route)

        node = self.node_for_session(principal_id, state.session_id)
        if (
            state.gateway_generation != self._generation
            or node is None
            or node.node_id != state.authority.source_node_id
            or node.enrollment_generation != state.authority.enrollment_generation
        ):
            self._cancel_peer_runtime(state)
            with self._lock:
                self._peer_calls_by_turn.pop((principal_id, turn_id), None)
                self._peer_calls_by_request.pop(state.replay_key, None)
                self._inflight.pop(state.replay_key, None)
                self._turn_routes.pop((principal_id, turn_id), None)
                self._release_runtime_quota_key(state.replay_key)
            return False

        if isinstance(event, events.AssistantDelta):
            state.streamed_bytes += len(event.text.encode("utf-8"))
            if state.streamed_bytes > state.request.budget.max_output_bytes:
                self._cancel_peer_runtime(state)
                self._finish_peer_call(
                    state,
                    self._peer_error(
                        request_id=state.request.request_id,
                        trace_id=state.request.trace_id,
                        code="budget_exceeded",
                        message="peer runtime output budget exceeded",
                        generation=state.gateway_generation,
                    ),
                )
                return False

        if isinstance(event, (events.ResponseText, events.AssistantComplete)):
            text = str(getattr(event, "text", ""))
            if len(text.encode("utf-8")) > state.request.budget.max_output_bytes:
                self._cancel_peer_runtime(state)
                self._finish_peer_call(
                    state,
                    self._peer_error(
                        request_id=state.request.request_id,
                        trace_id=state.request.trace_id,
                        code="budget_exceeded",
                        message="peer runtime output budget exceeded",
                        generation=state.gateway_generation,
                    ),
                )
                return False
            state.final_text = text

        if isinstance(event, events.TurnCancelled):
            self._finish_peer_call(
                state,
                self._peer_error(
                    request_id=state.request.request_id,
                    trace_id=state.request.trace_id,
                    code="cancelled",
                    message="peer runtime request was cancelled",
                    generation=state.gateway_generation,
                ),
            )
        elif isinstance(event, events.AgentFailed):
            self._finish_peer_call(
                state,
                self._peer_error(
                    request_id=state.request.request_id,
                    trace_id=state.request.trace_id,
                    code="runtime_error",
                    message="peer runtime failed",
                    generation=state.gateway_generation,
                ),
            )
        elif isinstance(event, events.AgentCompleted):
            self._finish_peer_call(
                state,
                PeerResult.completed(
                    request_id=state.request.request_id,
                    source_node_id=self._peer_local_node_id,
                    runtime_id=self._peer_runtime_id,
                    runtime_generation=state.gateway_generation,
                    text=state.final_text,
                    trace_id=state.request.trace_id,
                ),
            )
        return super()._allow_runtime_event(principal_id, envelope, route)

    def _drop_peer_session(self, principal_id: str, session_id: str) -> None:
        with self._lock:
            calls = [
                state
                for state in tuple(self._peer_calls_by_turn.values())
                if state.principal.principal_id == principal_id
                and state.session_id == session_id
            ]
        for state in calls:
            self._cancel_peer_runtime(state)
            key = (principal_id, state.turn_id)
            with self._lock:
                self._peer_calls_by_turn.pop(key, None)
                self._peer_calls_by_request.pop(state.replay_key, None)
                self._inflight.pop(state.replay_key, None)
                self._turn_routes.pop(key, None)
                self._release_runtime_quota_key(state.replay_key)

    def _clear_peer_gateway(self) -> None:
        with self._lock:
            self._peer_calls_by_turn.clear()
            self._peer_calls_by_request.clear()
