from __future__ import annotations

import concurrent.futures
import queue
from collections import deque
from types import SimpleNamespace

import pytest

from zara.client import ZaraClientState
from zara.protocol import AUDIO_OUTPUT_CODEC, ProtocolMessage, ProtocolValidationError
from zara.runtime import events
from zara.runtime.commands import SubmitTurn
from zara.server import PrincipalContext
from zara import zmq_transport as transport


def message(message_type: str, **kwargs) -> ProtocolMessage:
    return ProtocolMessage(
        type=message_type,
        id=kwargs.pop("id", "m1"),
        timestamp_ns=kwargs.pop("timestamp_ns", 1),
        payload_count=kwargs.pop("payload_count", 0),
        **kwargs,
    )


@pytest.mark.parametrize(
    ("value", "match"),
    [
        (None, "requires codec"),
        ({}, "requires codec"),
        ({"codec": "bad", "sample_rate": 24000, "channels": 1}, "codec"),
        ({"codec": AUDIO_OUTPUT_CODEC, "sample_rate": True, "channels": 1}, "sample_rate"),
        ({"codec": AUDIO_OUTPUT_CODEC, "sample_rate": 24000, "channels": 0}, "channels"),
    ],
)
def test_audio_output_format_validation_fails_closed(value, match):
    with pytest.raises(ValueError, match=match):
        transport._normalize_audio_output_format(value)


def test_audio_output_format_sequence_rejects_empty_duplicates_and_normalizes():
    spec = {"codec": AUDIO_OUTPUT_CODEC, "sample_rate": 24000, "channels": 1}
    with pytest.raises(ValueError, match="non-empty"):
        transport._normalize_audio_output_formats([])
    with pytest.raises(ValueError, match="duplicates"):
        transport._normalize_audio_output_formats([spec, dict(spec)])
    assert transport._normalize_audio_output_formats((spec,)) == [spec]


@pytest.mark.parametrize(
    ("field", "value", "match"),
    [
        ("sndhwm", 0, "positive integer"),
        ("rcvhwm", True, "positive integer"),
        ("max_message_bytes", -1, "positive integer"),
        ("heartbeat_interval_ms", 0, "positive integer"),
        ("heartbeat_timeout_ms", 0, "positive integer"),
        ("poll_interval_ms", 0, "positive integer"),
        ("event_queue_size", 0, "positive integer"),
        ("pending_request_limit", 0, "positive integer"),
        ("idempotency_cache_size", 0, "positive integer"),
        ("linger_ms", -1, "non-negative"),
        ("request_timeout", 0, "positive"),
    ],
)
def test_transport_config_rejects_unbounded_or_invalid_limits(field, value, match):
    with pytest.raises(ValueError, match=match):
        transport.TransportConfig(**{field: value})


class VoiceIngress:
    def __init__(self) -> None:
        self.cancelled: list[dict] = []
        self.fail = False

    def cancel(self, **kwargs) -> None:
        self.cancelled.append(kwargs)
        if self.fail:
            raise RuntimeError("cancel failed")


def gateway(*, voice_ingress=None, config=None) -> transport.ZaraZmqGateway:
    return transport.ZaraZmqGateway(
        "inproc://w07-unit",
        supervisor=SimpleNamespace(),
        principal=PrincipalContext("w07-owner"),
        context=SimpleNamespace(),
        config=config or transport.TransportConfig(event_queue_size=2, pending_request_limit=2),
        voice_ingress=voice_ingress,
    )


def route_state(*, capabilities=frozenset()) -> transport._RouteState:
    return transport._RouteState(
        session_id="session-1",
        principal_id="w07-owner",
        ready=True,
        capabilities=capabilities,
    )


def test_gateway_constructor_and_audio_cancel_validation():
    with pytest.raises(ValueError, match="endpoint"):
        transport.ZaraZmqGateway(
            " ",
            supervisor=SimpleNamespace(),
            principal=PrincipalContext("owner"),
        )
    with pytest.raises(TypeError, match="PrincipalContext"):
        transport.ZaraZmqGateway(
            "inproc://x",
            supervisor=SimpleNamespace(),
            principal=object(),
        )

    ingress = VoiceIngress()
    gw = gateway(voice_ingress=ingress)
    state = route_state()
    state.audio_inputs["stream"] = transport._AudioInputState(
        principal=PrincipalContext("w07-owner"),
        conversation_id="c1",
        trace_id="trace-1",
    )
    gw._cancel_audio_inputs(None)
    gw._cancel_audio_inputs(state)
    assert state.audio_inputs == {}
    assert ingress.cancelled[0]["stream_id"] == "stream"

    state.audio_inputs["stream-2"] = transport._AudioInputState(
        principal=PrincipalContext("w07-owner"),
        conversation_id=None,
        trace_id=None,
    )
    ingress.fail = True
    gw._cancel_audio_inputs(state)
    assert state.audio_inputs == {}


def test_gateway_route_drop_cleans_every_route_owned_resource():
    gw = gateway()
    route = b"route"
    other = b"other"
    state = route_state(capabilities=frozenset({"open_uri"}))
    gw._routes[route] = state
    gw._route_outbound[route] = deque()
    gw._turn_routes[("w07-owner", "turn-1")] = route
    gw._turn_routes[("w07-owner", "turn-2")] = other
    gw._approval_owners[("w07-owner", "tool-1")] = transport._ApprovalOwner(
        route=route, session_id=state.session_id
    )
    gw._approval_owners[("w07-owner", "tool-2")] = transport._ApprovalOwner(
        route=other, session_id="other-session"
    )
    gw._inflight[("w07-owner", "req")] = transport._InflightEntry(
        command=SubmitTurn(text="hello"),
        routes=[
            transport._RequestRoute(route, "w07-owner", state.session_id),
            transport._RequestRoute(other, "w07-owner", "other-session"),
        ],
    )
    pending = transport.DeviceActionHandle("action-1")
    gw._device_actions["action-1"] = transport._DeviceActionPending(
        route=route,
        principal_id="w07-owner",
        session_id=state.session_id,
        capability="open_uri",
        future=pending,
    )

    removed = gw._drop_route_locked(route)

    assert removed is state
    assert route not in gw._routes
    assert route not in gw._route_outbound
    assert ("w07-owner", "turn-1") not in gw._turn_routes
    assert ("w07-owner", "turn-2") in gw._turn_routes
    assert ("w07-owner", "tool-1") not in gw._approval_owners
    assert ("w07-owner", "tool-2") in gw._approval_owners
    assert gw._inflight[("w07-owner", "req")].routes == [
        transport._RequestRoute(other, "w07-owner", "other-session")
    ]
    with pytest.raises(transport.ClientDisconnected):
        pending.result()


def test_gateway_outbound_queue_is_bounded_and_lossy_oldest():
    gw = gateway()
    route = b"route"
    gw._routes[route] = route_state()
    assert gw._enqueue_outbound(b"missing", message("pong")) is False

    assert gw._enqueue_outbound(route, message("pong", id="one")) is True
    assert gw._enqueue_outbound(route, message("pong", id="two")) is True
    assert gw._enqueue_outbound(route, message("pong", id="three")) is True

    queued = list(gw._route_outbound[route])
    assert [item.message.id for item in queued] == ["two", "three"]


def test_gateway_device_action_deadline_capability_backpressure_and_cancel(monkeypatch):
    gw = gateway()
    route = b"device"
    state = route_state(capabilities=frozenset({"open_uri"}))
    gw._routes[route] = state

    with pytest.raises(TypeError, match="integer"):
        gw.request_device_action(
            principal_id="w07-owner",
            session_id=state.session_id,
            capability="open_uri",
            args={},
            deadline_ns=True,
        )
    with pytest.raises(ValueError, match="expired"):
        gw.request_device_action(
            principal_id="w07-owner",
            session_id=state.session_id,
            capability="open_uri",
            args={},
            deadline_ns=0,
        )
    with pytest.raises(transport.DeviceCapabilityUnavailable, match="session"):
        gw.request_device_action(
            principal_id="missing",
            session_id="missing",
            capability="open_uri",
            args={},
            deadline_ns=transport._now_ns() + 1_000_000_000,
        )
    with pytest.raises(transport.DeviceCapabilityUnavailable, match="capability"):
        gw.request_device_action(
            principal_id="w07-owner",
            session_id=state.session_id,
            capability="open_app",
            args={},
            deadline_ns=transport._now_ns() + 1_000_000_000,
        )

    first = gw.request_device_action(
        principal_id="w07-owner",
        session_id=state.session_id,
        capability="open_uri",
        args={"uri": "https://example.invalid"},
        deadline_ns=transport._now_ns() + 1_000_000_000,
    )
    second = gw.request_device_action(
        principal_id="w07-owner",
        session_id=state.session_id,
        capability="open_uri",
        args={"uri": "https://example.invalid/second"},
        deadline_ns=transport._now_ns() + 1_000_000_000,
    )
    with pytest.raises(transport.ClientBackpressureError):
        gw.request_device_action(
            principal_id="w07-owner",
            session_id=state.session_id,
            capability="open_uri",
            args={"uri": "https://example.invalid/overflow"},
            deadline_ns=transport._now_ns() + 1_000_000_000,
        )

    assert gw.cancel_device_action("missing") is False
    assert gw.cancel_device_action(first.action_id, reason="operator cancelled") is True
    with pytest.raises(transport.DeviceActionCancelled, match="operator cancelled"):
        first.result()
    assert gw.cancel_device_action(second.action_id) is True

    disconnected = gateway()
    disconnected._routes[route] = route_state(capabilities=frozenset({"open_uri"}))
    monkeypatch.setattr(disconnected, "_enqueue_outbound", lambda *_args, **_kwargs: False)
    result = disconnected.request_device_action(
        principal_id="w07-owner",
        session_id="session-1",
        capability="open_uri",
        args={},
        deadline_ns=transport._now_ns() + 1_000_000_000,
    )
    with pytest.raises(transport.ClientDisconnected):
        result.result()


def test_gateway_early_turn_buffer_is_bounded_and_consumed():
    gw = gateway()
    held = (message("turn.started", turn_id="turn"), ())
    for index in range(70):
        gw._buffer_early_turn_event("owner", f"turn-{index}", held)
    assert len(gw._early_turn_events) == 64

    gw._buffer_early_turn_event("owner", "active", held)
    gw._buffer_early_turn_event("owner", "active", held)
    assert gw._take_early_turn_events("owner", "active") == [held, held]
    assert gw._take_early_turn_events("owner", "active") == []


class ClientVoice:
    def __init__(self) -> None:
        self.calls: list[tuple] = []

    def start(self, **kwargs) -> None:
        self.calls.append(("start", kwargs))

    def chunk(self, payload, **kwargs) -> None:
        self.calls.append(("chunk", payload, kwargs))

    def finish(self, **kwargs) -> None:
        self.calls.append(("finish", kwargs))

    def cancel(self, **kwargs) -> None:
        self.calls.append(("cancel", kwargs))


def client(*, voice_output=None, config=None) -> transport.ZmqZaraClient:
    result = transport.ZmqZaraClient(
        "inproc://w07-client",
        context=SimpleNamespace(),
        config=config or transport.TransportConfig(sndhwm=2, pending_request_limit=2),
        voice_output=voice_output,
    )
    result._state = ZaraClientState.READY
    result._session_id = "session-1"
    result._audio_output_format = {
        "codec": AUDIO_OUTPUT_CODEC,
        "sample_rate": 24000,
        "channels": 1,
    }
    return result


def test_zmq_client_constructor_and_start_state_validation():
    with pytest.raises(ValueError, match="endpoint"):
        transport.ZmqZaraClient(" ")
    with pytest.raises(TypeError, match="curve_client"):
        transport.ZmqZaraClient("inproc://x", curve_client=object())
    with pytest.raises(ValueError, match="non-empty"):
        transport.ZmqZaraClient("inproc://x", audio_output_formats="bad")

    ready = client()
    completed = ready.start()
    assert completed.result() is True

    starting = client()
    starting._state = ZaraClientState.STARTING
    assert starting.start() is starting._started

    failed = client()
    failed._state = ZaraClientState.FAILED
    with pytest.raises(transport.ClientNotReady, match="failed"):
        failed.start()


def test_zmq_client_start_expiry_outbound_and_request_backpressure():
    c = client()
    c._state = ZaraClientState.STARTING
    c._started = concurrent.futures.Future()
    c._expire_start(0.0)
    with pytest.raises(transport.ClientDisconnected, match="handshake timed out"):
        c._started.result()
    assert c.state is ZaraClientState.FAILED
    assert c._stop.is_set()

    c = client(config=transport.TransportConfig(sndhwm=1, pending_request_limit=1))
    outbound_socket = SimpleNamespace(sent=[])
    outbound_socket.send_multipart = lambda frames: outbound_socket.sent.append(frames)
    c._outbound.put_nowait(transport._ClientOutbound(message("ping")))
    c._drain_client_outbound(outbound_socket)
    assert len(outbound_socket.sent) == 1
    c._drain_client_outbound(outbound_socket)

    pending = concurrent.futures.Future()
    c._pending["same"] = transport._Pending(transport._PendingKind.PING, pending)
    with pytest.raises(transport.ClientBackpressureError, match="already pending"):
        c._request(message("ping", id="same"), transport._PendingKind.PING)
    with pytest.raises(transport.ClientBackpressureError, match="pending request limit"):
        c._request(message("ping", id="other"), transport._PendingKind.PING)

    c._pending.clear()
    c._outbound.put_nowait(transport._ClientOutbound(message("ping", id="occupied")))
    with pytest.raises(transport.ClientBackpressureError, match="outbound queue"):
        c._request(message("ping", id="queued"), transport._PendingKind.PING)
    assert "queued" not in c._pending


def test_zmq_client_voice_output_cancel_mismatch_chunk_and_done():
    voice = ClientVoice()
    c = client(voice_output=voice)
    assert c._handle_voice_output(message("pong"), ()) is False
    no_voice = client()
    assert no_voice._handle_voice_output(message("audio.output.start", turn_id="t0"), ()) is True

    c._remember_cancelled_voice_turn("cancelled")
    assert c._handle_voice_output(
        message("audio.output.chunk", turn_id="cancelled", seq=0), (b"x",)
    ) is True
    assert voice.calls == []

    assert c._handle_voice_output(
        message(
            "audio.output.start",
            turn_id="mismatch",
            stream_id="s",
            body={"codec": AUDIO_OUTPUT_CODEC, "sample_rate": 22050, "channels": 1},
        ),
        (),
    ) is True
    assert voice.calls == []

    start = message(
        "audio.output.start",
        turn_id="t1",
        conversation_id="c1",
        stream_id="s1",
        trace_id="trace",
        body={"codec": AUDIO_OUTPUT_CODEC, "sample_rate": 24000, "channels": 1},
    )
    assert c._handle_voice_output(start, ()) is True
    assert voice.calls[-1][0] == "start"
    assert "t1" in c._active_voice_outputs

    chunk = message(
        "audio.output.chunk",
        turn_id="t1",
        conversation_id="c1",
        stream_id="s1",
        trace_id="trace",
        seq=0,
    )
    c._handle_voice_output(chunk, (b"pcm",))
    assert voice.calls[-1][0] == "chunk"

    c._handle_voice_output(
        message(
            "audio.output.done",
            turn_id="t1",
            conversation_id="c1",
            stream_id="s1",
            trace_id="trace",
        ),
        (),
    )
    assert voice.calls[-1][0] == "finish"
    assert "t1" not in c._active_voice_outputs

    c._active_voice_outputs["t2"] = {
        "conversation_id": "c1",
        "turn_id": "t2",
        "stream_id": "s2",
        "trace_id": None,
    }
    c._stop_voice_output_turn("t2")
    assert voice.calls[-1][0] == "cancel"
    assert "t2" in c._cancelled_voice_turns


def pending(kind: transport._PendingKind):
    return transport._Pending(kind, concurrent.futures.Future())


def test_zmq_client_pending_resolution_accepts_and_rejects_closed_contract():
    c = client()

    p = pending(transport._PendingKind.PING)
    c._resolve_pending(
        p,
        message(
            "protocol.error",
            reply_to="r",
            body={"code": "busy", "message": "later", "retryable": True},
        ),
    )
    with pytest.raises(transport.ProtocolRemoteError) as error:
        p.future.result()
    assert error.value.code == "busy"
    assert error.value.retryable is True

    p = pending(transport._PendingKind.HELLO)
    c._state = ZaraClientState.STARTING
    c._resolve_pending(p, message("pong"))
    with pytest.raises(ProtocolValidationError, match="hello"):
        p.future.result()
    assert c._stop.is_set()

    c._stop.clear()
    p = pending(transport._PendingKind.HELLO)
    c._resolve_pending(
        p,
        message(
            "hello.ok",
            session_id="session-2",
            body={"audio_output_format": {"codec": "bad", "sample_rate": 1, "channels": 1}},
        ),
    )
    with pytest.raises(ProtocolValidationError, match="negotiated"):
        p.future.result()

    c._stop.clear()
    c._audio_output_formats = [
        {"codec": AUDIO_OUTPUT_CODEC, "sample_rate": 24000, "channels": 1}
    ]
    p = pending(transport._PendingKind.HELLO)
    c._resolve_pending(
        p,
        message(
            "hello.ok",
            session_id="session-3",
            body={
                "audio_output_format": {
                    "codec": AUDIO_OUTPUT_CODEC,
                    "sample_rate": 24000,
                    "channels": 1,
                }
            },
        ),
    )
    assert p.future.result() is True
    assert c.state is ZaraClientState.READY
    assert c.session_id == "session-3"

    cases = [
        (transport._PendingKind.PING, message("turn.accepted"), "ping"),
        (
            transport._PendingKind.STATUS,
            message("runtime.status.ok", body={"state": ""}),
            "runtime status",
        ),
        (
            transport._PendingKind.CONVERSATION,
            message("conversation.opened"),
            "conversation",
        ),
        (transport._PendingKind.COMMAND, message("pong"), "command"),
        (transport._PendingKind.AUDIO, message("pong"), "audio input"),
    ]
    for kind, response, match in cases:
        p = pending(kind)
        c._resolve_pending(p, response)
        with pytest.raises(ProtocolValidationError, match=match):
            p.future.result()

    p = pending(transport._PendingKind.PING)
    pong = message("pong")
    c._resolve_pending(p, pong)
    assert p.future.result() is pong

    p = pending(transport._PendingKind.STATUS)
    c._resolve_pending(p, message("runtime.status.ok", body={"state": "ready"}))
    assert p.future.result() == "ready"

    p = pending(transport._PendingKind.CONVERSATION)
    c._resolve_pending(p, message("conversation.opened", conversation_id="c-open"))
    assert p.future.result() == "c-open"

    p = pending(transport._PendingKind.COMMAND)
    c._resolve_pending(
        p, message("turn.accepted", reply_to="request-1", turn_id="turn-1")
    )
    assert p.future.result().request_id == "request-1"

    p = pending(transport._PendingKind.AUDIO)
    accepted = message("audio.input.accepted", stream_id="s", seq=0)
    c._resolve_pending(p, accepted)
    assert p.future.result() is accepted


def test_zmq_client_open_conversation_submit_shutdown_and_fail_pending(monkeypatch):
    c = client()
    requested: list[tuple[ProtocolMessage, transport._PendingKind]] = []

    def request(msg, kind, *, payloads=()):
        requested.append((msg, kind))
        future = concurrent.futures.Future()
        future.set_result("opened-c" if kind is transport._PendingKind.CONVERSATION else msg)
        return future

    monkeypatch.setattr(c, "_request", request)
    assert c.open_conversation("wanted").result() == "opened-c"
    assert c._conversation_id == "opened-c"

    with pytest.raises(TypeError, match="supports turn"):
        c.submit(object())

    stopped = c.shutdown("bye")
    assert stopped.result() == "bye"
    assert c.state is ZaraClientState.STOPPING

    c._state = ZaraClientState.NEW
    c.shutdown()
    assert c.state is ZaraClientState.STOPPED

    first = concurrent.futures.Future()
    second = concurrent.futures.Future()
    second.set_result(True)
    c._pending = {
        "one": transport._Pending(transport._PendingKind.PING, first),
        "two": transport._Pending(transport._PendingKind.PING, second),
    }
    c._fail_pending(transport.ClientDisconnected("gone"))
    with pytest.raises(transport.ClientDisconnected):
        first.result()
    assert second.result() is True
    assert c._pending == {}


def test_zmq_client_runtime_event_projection_publishes_known_and_ignores_unknown():
    c = client()
    sub = c.subscribe(maxsize=32)
    try:
        messages = [
            message("turn.started", turn_id="t", conversation_id="c"),
            message(
                "turn.cancelled",
                turn_id="t",
                conversation_id="c",
                body={"reason": "stop"},
            ),
            message(
                "turn.completed",
                turn_id="t",
                conversation_id="c",
                body={"success": True},
            ),
            message("assistant.started", turn_id="t", conversation_id="c"),
            message(
                "assistant.delta",
                turn_id="t",
                conversation_id="c",
                body={"text": "part"},
            ),
            message(
                "assistant.completed",
                turn_id="t",
                conversation_id="c",
                body={"text": "done", "success": True},
            ),
            message(
                "assistant.response",
                turn_id="t",
                conversation_id="c",
                body={"text": "buffered", "truncated": False},
            ),
            message(
                "runtime.error",
                turn_id="t",
                conversation_id="c",
                body={"reason": "boom", "fatal": False},
            ),
            message(
                "runtime.stopped",
                turn_id="t",
                conversation_id="c",
                body={"reason": "bye"},
            ),
            message(
                "tool.queued",
                turn_id="t",
                conversation_id="c",
                body={"tool_run_id": "tool", "tool_name": "demo"},
            ),
            message(
                "tool.waiting",
                turn_id="t",
                conversation_id="c",
                body={
                    "tool_run_id": "tool",
                    "tool_name": "demo",
                    "kind": "approval",
                    "prompt": "approve?",
                },
            ),
            message(
                "tool.started",
                turn_id="t",
                conversation_id="c",
                body={"tool_run_id": "tool", "tool_name": "demo"},
            ),
            message(
                "tool.completed",
                turn_id="t",
                conversation_id="c",
                body={"tool_run_id": "tool", "tool_name": "demo", "success": True},
            ),
            message(
                "tool.failed",
                turn_id="t",
                conversation_id="c",
                body={"tool_run_id": "tool", "tool_name": "demo", "reason": "bad"},
            ),
            message(
                "tool.cancelled",
                turn_id="t",
                conversation_id="c",
                body={"tool_run_id": "tool", "tool_name": "demo", "reason": "stop"},
            ),
        ]
        for item in messages:
            c._publish_runtime_event(item)
        projected = [sub.get(timeout=0.1).event for _ in messages]
        assert isinstance(projected[0], events.TurnStarted)
        assert isinstance(projected[-1], events.ToolCancelled)

        c._publish_runtime_event(message("pong"))
        with pytest.raises(queue.Empty):
            sub.get(timeout=0.01)
    finally:
        sub.close()
