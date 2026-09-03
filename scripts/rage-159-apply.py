#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path


def replace_once(path: str, old: str, new: str) -> None:
    target = Path(path)
    text = target.read_text()
    count = text.count(old)
    if count != 1:
        raise SystemExit(f"{path}: expected exactly one source anchor, found {count}")
    target.write_text(text.replace(old, new, 1))


replace_once(
    "zara/protocol.py",
    '''        "tool.approve",\n        "tool.reject",\n    }\n)''',
    '''        "tool.approve",\n        "tool.reject",\n        "capability.snapshot",\n        "device.action.accepted",\n        "device.action.result",\n        "device.action.error",\n    }\n)''',
)
replace_once(
    "zara/protocol.py",
    '''        "tool.cancelled",\n        "assistant.started",''',
    '''        "tool.cancelled",\n        "capability.snapshot.ok",\n        "device.action.request",\n        "device.action.cancel",\n        "assistant.started",''',
)
replace_once(
    "zara/protocol.py",
    '''_TOOL_NAME_RE = re.compile(r"^[A-Za-z0-9_.:-]+$")\n''',
    '''_TOOL_NAME_RE = re.compile(r"^[A-Za-z0-9_.:-]+$")\nDEVICE_CAPABILITIES = frozenset({"open_app", "open_uri"})\n_DEVICE_CAPABILITY_LIMIT = 32\n_DEVICE_ACTION_IDEMPOTENCY = frozenset({"at_most_once", "idempotent"})\n_DEVICE_ACTION_ERROR_CODES = frozenset(\n    {"permission_denied", "unavailable", "invalid_arguments", "failed", "cancelled"}\n)\n''',
)
replace_once(
    "zara/protocol.py",
    '''\ndef _message_from_mapping(data: Mapping[str, Any], limits: ProtocolLimits) -> ProtocolMessage:\n''',
    r'''
def _validate_device_capability_id(value: Any) -> str:
    capability = _validate_ascii_token("capability", value, max_bytes=64)
    if capability not in DEVICE_CAPABILITIES:
        raise ProtocolValidationError("unknown device capability")
    return capability


def _validate_capability_snapshot(body: Mapping[str, Any]) -> None:
    if set(body) != {"capabilities"}:
        raise ProtocolValidationError("capability snapshot body has invalid fields")
    capabilities = body["capabilities"]
    if not isinstance(capabilities, list):
        raise ProtocolValidationError("capabilities must be a list")
    if len(capabilities) > _DEVICE_CAPABILITY_LIMIT:
        raise ProtocolValidationError("capability count exceeds limit")
    seen: set[str] = set()
    for entry in capabilities:
        if not isinstance(entry, dict) or set(entry) != {"id", "version"}:
            raise ProtocolValidationError("capability entry has invalid fields")
        capability = _validate_device_capability_id(entry["id"])
        if entry["version"] != 1:
            raise ProtocolValidationError("unsupported device capability version")
        if capability in seen:
            raise ProtocolValidationError("duplicate device capability")
        seen.add(capability)


def _validate_device_action_args(capability: str, value: Any) -> None:
    if not isinstance(value, dict):
        raise ProtocolValidationError("device action args must be an object")
    if capability == "open_uri":
        if set(value) != {"uri"}:
            raise ProtocolValidationError("open_uri args have invalid fields")
        uri = _bounded_safe_text("uri", value["uri"], max_bytes=2048)
        if not uri:
            raise ProtocolValidationError("uri must not be empty")
        return
    if capability == "open_app":
        if set(value) != {"app"}:
            raise ProtocolValidationError("open_app args have invalid fields")
        app = _bounded_safe_text("app", value["app"], max_bytes=128)
        if not app:
            raise ProtocolValidationError("app must not be empty")
        return
    raise ProtocolValidationError("unknown device capability")


def _validate_device_common(message: ProtocolMessage) -> dict[str, Any]:
    if message.session_id is None:
        raise ProtocolValidationError(f"{message.type} requires session_id")
    if message.payload_count != 0:
        raise ProtocolValidationError(f"{message.type} does not accept payload frames")
    if any(
        value is not None
        for value in (
            message.conversation_id,
            message.turn_id,
            message.stream_id,
            message.seq,
            message.content_type,
        )
    ):
        raise ProtocolValidationError(f"{message.type} has invalid correlation fields")
    if message.flags:
        raise ProtocolValidationError(f"{message.type} does not accept flags")
    return dict(message.body or {})


def _validate_device_envelope(message: ProtocolMessage) -> None:
    if message.type in {"capability.snapshot", "capability.snapshot.ok"}:
        body = _validate_device_common(message)
        if message.trace_id is not None:
            raise ProtocolValidationError(f"{message.type} does not accept trace_id")
        if message.type == "capability.snapshot":
            if message.reply_to is not None:
                raise ProtocolValidationError("capability.snapshot does not accept reply_to")
        elif message.reply_to is None:
            raise ProtocolValidationError("capability.snapshot.ok requires reply_to")
        _validate_capability_snapshot(body)
        return

    if message.type == "device.action.request":
        body = _validate_device_common(message)
        if message.reply_to is not None:
            raise ProtocolValidationError("device.action.request does not accept reply_to")
        required = {"action_id", "capability", "args", "deadline_ns", "idempotency"}
        if set(body) != required:
            raise ProtocolValidationError("device.action.request body has invalid fields")
        _validate_ascii_token("action_id", body["action_id"], max_bytes=128)
        capability = _validate_device_capability_id(body["capability"])
        _validate_device_action_args(capability, body["args"])
        deadline_ns = body["deadline_ns"]
        if type(deadline_ns) is not int or deadline_ns <= 0:
            raise ProtocolValidationError("device action deadline_ns must be positive")
        if body["idempotency"] not in _DEVICE_ACTION_IDEMPOTENCY:
            raise ProtocolValidationError("invalid device action idempotency")
        return

    if message.type == "device.action.cancel":
        body = _validate_device_common(message)
        if message.reply_to is not None or message.trace_id is not None:
            raise ProtocolValidationError("device.action.cancel has invalid correlation")
        if set(body) not in ({"action_id"}, {"action_id", "reason"}):
            raise ProtocolValidationError("device.action.cancel body has invalid fields")
        _validate_ascii_token("action_id", body["action_id"], max_bytes=128)
        if "reason" in body:
            _bounded_safe_text("reason", body["reason"], max_bytes=256)
        return

    if message.type == "device.action.accepted":
        body = _validate_device_common(message)
        if message.reply_to is not None or message.trace_id is not None:
            raise ProtocolValidationError("device.action.accepted has invalid correlation")
        if set(body) != {"action_id"}:
            raise ProtocolValidationError("device.action.accepted body has invalid fields")
        _validate_ascii_token("action_id", body["action_id"], max_bytes=128)
        return

    if message.type == "device.action.result":
        body = _validate_device_common(message)
        if message.reply_to is not None or message.trace_id is not None:
            raise ProtocolValidationError("device.action.result has invalid correlation")
        if set(body) != {"action_id", "outcome"} or body.get("outcome") != "completed":
            raise ProtocolValidationError("device.action.result body has invalid fields")
        _validate_ascii_token("action_id", body["action_id"], max_bytes=128)
        return

    if message.type == "device.action.error":
        body = _validate_device_common(message)
        if message.reply_to is not None or message.trace_id is not None:
            raise ProtocolValidationError("device.action.error has invalid correlation")
        if set(body) not in ({"action_id", "code"}, {"action_id", "code", "message"}):
            raise ProtocolValidationError("device.action.error body has invalid fields")
        _validate_ascii_token("action_id", body["action_id"], max_bytes=128)
        if body["code"] not in _DEVICE_ACTION_ERROR_CODES:
            raise ProtocolValidationError("unknown device action error code")
        if "message" in body:
            _bounded_safe_text("message", body["message"], max_bytes=256)


def _message_from_mapping(data: Mapping[str, Any], limits: ProtocolLimits) -> ProtocolMessage:
''',
)
replace_once(
    "zara/protocol.py",
    '''    _validate_visible_stt_envelope(message)\n    _validate_tool_envelope(message)\n    return message\n''',
    '''    _validate_visible_stt_envelope(message)\n    _validate_tool_envelope(message)\n    _validate_device_envelope(message)\n    return message\n''',
)

replace_once(
    "zara/zmq_transport.py",
    '''class ProtocolRemoteError(RuntimeError):\n    def __init__(self, code: str, message: str, *, retryable: bool = False) -> None:\n        super().__init__(message)\n        self.code = code\n        self.retryable = retryable\n\n\n''',
    '''class ProtocolRemoteError(RuntimeError):\n    def __init__(self, code: str, message: str, *, retryable: bool = False) -> None:\n        super().__init__(message)\n        self.code = code\n        self.retryable = retryable\n\n\nclass DeviceCapabilityUnavailable(RuntimeError):\n    pass\n\n\nclass DeviceActionCancelled(RuntimeError):\n    pass\n\n\nclass DeviceActionRemoteError(RuntimeError):\n    def __init__(self, code: str, message: str = "device action failed") -> None:\n        super().__init__(message)\n        self.code = code\n\n\n@dataclass(frozen=True)\nclass DeviceActionResult:\n    action_id: str\n    capability: str\n    outcome: str\n\n\n''',
)
replace_once(
    "zara/zmq_transport.py",
    '''    audio_output: bool = False\n    audio_inputs: dict[str, _AudioInputState] = field(default_factory=dict)\n''',
    '''    audio_output: bool = False\n    audio_inputs: dict[str, _AudioInputState] = field(default_factory=dict)\n    capabilities: frozenset[str] = frozenset()\n''',
)
replace_once(
    "zara/zmq_transport.py",
    '''@dataclass(frozen=True)\nclass _ApprovalOwner:\n    route: bytes\n    session_id: str\n\n\nclass ZaraZmqGateway:''',
    '''@dataclass(frozen=True)\nclass _ApprovalOwner:\n    route: bytes\n    session_id: str\n\n\n@dataclass\nclass _DeviceActionPending:\n    route: bytes\n    principal_id: str\n    session_id: str\n    capability: str\n    future: concurrent.futures.Future\n    accepted: bool = False\n\n\nclass ZaraZmqGateway:''',
)
replace_once(
    "zara/zmq_transport.py",
    '''        self._inflight: dict[tuple[str, str], _InflightEntry] = {}\n        self._event_subscription = None\n''',
    '''        self._inflight: dict[tuple[str, str], _InflightEntry] = {}\n        self._device_actions: dict[str, _DeviceActionPending] = {}\n        self._event_subscription = None\n''',
)
replace_once(
    "zara/zmq_transport.py",
    '''            self._inflight.clear()\n            self._route_outbound.clear()\n''',
    '''            self._inflight.clear()\n            for pending in self._device_actions.values():\n                if not pending.future.done():\n                    pending.future.set_exception(ClientDisconnected("gateway restarted"))\n            self._device_actions.clear()\n            self._route_outbound.clear()\n''',
)
replace_once(
    "zara/zmq_transport.py",
    '''        for inflight in self._inflight.values():\n            inflight.routes[:] = [\n                candidate for candidate in inflight.routes if candidate.route != route\n            ]\n        return state\n''',
    '''        for inflight in self._inflight.values():\n            inflight.routes[:] = [\n                candidate for candidate in inflight.routes if candidate.route != route\n            ]\n        for action_id, pending in tuple(self._device_actions.items()):\n            if pending.route == route:\n                self._device_actions.pop(action_id, None)\n                if not pending.future.done():\n                    pending.future.set_exception(ClientDisconnected("device route disconnected"))\n        return state\n''',
)
replace_once(
    "zara/zmq_transport.py",
    '''    def _receive(self, socket: zmq.Socket) -> None:\n''',
    r'''    def _route_for_session_locked(self, principal_id: str, session_id: str) -> tuple[bytes, _RouteState] | None:
        matches = [
            (route, state)
            for route, state in self._routes.items()
            if state.ready
            and state.principal_id == principal_id
            and state.session_id == session_id
        ]
        return matches[0] if len(matches) == 1 else None

    def capabilities_for(self, principal_id: str, session_id: str) -> frozenset[str]:
        with self._lock:
            match = self._route_for_session_locked(principal_id, session_id)
            return frozenset() if match is None else match[1].capabilities

    def request_device_action(
        self,
        *,
        principal_id: str,
        session_id: str,
        capability: str,
        args: dict[str, object],
        deadline_ns: int,
        idempotency: str = "at_most_once",
        trace_id: Optional[str] = None,
    ) -> concurrent.futures.Future:
        action_id = _message_id()
        message = ProtocolMessage(
            type="device.action.request",
            id=_message_id(),
            session_id=session_id,
            trace_id=trace_id,
            timestamp_ns=_now_ns(),
            payload_count=0,
            body={
                "action_id": action_id,
                "capability": capability,
                "args": dict(args),
                "deadline_ns": deadline_ns,
                "idempotency": idempotency,
            },
        )
        encode_message(message, limits=self._limits)
        future: concurrent.futures.Future = concurrent.futures.Future()
        with self._lock:
            match = self._route_for_session_locked(principal_id, session_id)
            if match is None:
                raise DeviceCapabilityUnavailable("device session is unavailable")
            route, state = match
            if capability not in state.capabilities:
                raise DeviceCapabilityUnavailable(f"device capability is unavailable: {capability}")
            if len(self._device_actions) >= self._config.pending_request_limit:
                raise ClientBackpressureError("too many device actions are pending")
            self._device_actions[action_id] = _DeviceActionPending(
                route=route,
                principal_id=principal_id,
                session_id=session_id,
                capability=capability,
                future=future,
            )
        if not self._enqueue_outbound(route, message):
            with self._lock:
                self._device_actions.pop(action_id, None)
            future.set_exception(ClientDisconnected("device route disconnected"))
        return future

    def cancel_device_action(self, action_id: str, *, reason: str = "cancelled") -> bool:
        with self._lock:
            pending = self._device_actions.pop(action_id, None)
        if pending is None:
            return False
        message = ProtocolMessage(
            type="device.action.cancel",
            id=_message_id(),
            session_id=pending.session_id,
            timestamp_ns=_now_ns(),
            payload_count=0,
            body={"action_id": action_id, "reason": reason},
        )
        encode_message(message, limits=self._limits)
        self._enqueue_outbound(pending.route, message)
        if not pending.future.done():
            pending.future.set_exception(DeviceActionCancelled(reason))
        return True

    def _device_action_pending(
        self,
        route: bytes,
        state: _RouteState,
        action_id: str,
    ) -> _DeviceActionPending | None:
        pending = self._device_actions.get(action_id)
        if pending is None:
            return None
        if (
            pending.route != route
            or pending.principal_id != state.principal_id
            or pending.session_id != state.session_id
        ):
            return None
        return pending

    def _handle_device_message(
        self,
        socket: zmq.Socket,
        route: bytes,
        state: _RouteState,
        message: ProtocolMessage,
    ) -> None:
        if message.session_id != state.session_id:
            self._send(
                socket,
                route,
                _protocol_error(
                    reply_to=message.id,
                    code="stale_session",
                    message="message session is stale",
                    retryable=False,
                ),
            )
            return
        body = dict(message.body or {})
        if message.type == "capability.snapshot":
            capabilities = frozenset(entry["id"] for entry in body["capabilities"])
            with self._lock:
                current = self._routes.get(route)
                if current is None or current.session_id != state.session_id:
                    return
                current.capabilities = capabilities
            self._send(
                socket,
                route,
                ProtocolMessage(
                    type="capability.snapshot.ok",
                    id=_message_id(),
                    reply_to=message.id,
                    session_id=state.session_id,
                    timestamp_ns=_now_ns(),
                    payload_count=0,
                    body={
                        "capabilities": [
                            {"id": capability, "version": 1}
                            for capability in sorted(capabilities)
                        ]
                    },
                ),
            )
            return

        action_id = body["action_id"]
        with self._lock:
            pending = self._device_action_pending(route, state, action_id)
            if pending is None:
                self._send(
                    socket,
                    route,
                    _protocol_error(
                        reply_to=message.id,
                        code="unknown_action",
                        message="device action is unknown or stale",
                        retryable=False,
                    ),
                )
                return
            if message.type == "device.action.accepted":
                pending.accepted = True
                return
            self._device_actions.pop(action_id, None)

        if message.type == "device.action.result":
            if not pending.future.done():
                pending.future.set_result(
                    DeviceActionResult(
                        action_id=action_id,
                        capability=pending.capability,
                        outcome=body["outcome"],
                    )
                )
            return
        if not pending.future.done():
            pending.future.set_exception(
                DeviceActionRemoteError(body["code"], body.get("message", "device action failed"))
            )

    def _receive(self, socket: zmq.Socket) -> None:
''',
)
replace_once(
    "zara/zmq_transport.py",
    '''        if message.type == "ping":\n''',
    '''        if message.type in {\n            "capability.snapshot",\n            "device.action.accepted",\n            "device.action.result",\n            "device.action.error",\n        }:\n            self._handle_device_message(socket, route, state, message)\n            return\n        if message.type == "ping":\n''',
)
replace_once(
    "zara/zmq_transport.py",
    '''    def close(self, timeout: Optional[float] = None) -> None:\n        self._stop.set()\n''',
    '''    def close(self, timeout: Optional[float] = None) -> None:\n        with self._lock:\n            pending_actions = tuple(self._device_actions.values())\n            self._device_actions.clear()\n        for pending in pending_actions:\n            if not pending.future.done():\n                pending.future.set_exception(ClientDisconnected("gateway closed"))\n        self._stop.set()\n''',
)

replace_once(
    "zara/security_gateway.py",
    '''        if message_type in {"hello", "ping", "conversation.open"}:\n            return Capability.SESSION_BASIC\n''',
    '''        if message_type in {\n            "hello",\n            "ping",\n            "conversation.open",\n            "capability.snapshot",\n            "device.action.accepted",\n            "device.action.result",\n            "device.action.error",\n        }:\n            return Capability.SESSION_BASIC\n''',
)

print("RAGE #159 source patch applied")
