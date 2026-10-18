from __future__ import annotations

import io
import sys
from dataclasses import replace

import pytest

from zara.context import ContextSensitivity
from zara.protocol import ProtocolMessage, ProtocolValidationError, decode_message, encode_message
from zara.protocol_runtime import command_from_message
from zara.runtime.commands import AcquireTextContext, RevokeTextContext
from zara.security import Capability
from zara.security_gateway import SecureZaraZmqGateway


def acquire_message(**kwargs):
    values = dict(type="context.text.acquire", id="req", session_id="session",
                  conversation_id="chat", timestamp_ns=1, payload_count=0,
                  body={"text": "private snapshot", "display_name": "Context", "sensitivity": "private"})
    values.update(kwargs)
    return ProtocolMessage(**values)


def test_context_wire_acquire_and_revoke_closed_roundtrip():
    message = decode_message(encode_message(acquire_message())).message
    command = command_from_message(message)
    assert isinstance(command, AcquireTextContext)
    assert command.text == "private snapshot" and command.conversation_id == "chat"
    assert command.sensitivity is ContextSensitivity.PRIVATE
    revoke = replace(message, type="context.text.revoke", body={"context_id": "ctx-ref"})
    assert command_from_message(decode_message(encode_message(revoke)).message) == RevokeTextContext(
        request_id="req", context_id="ctx-ref", conversation_id="chat")


@pytest.mark.parametrize("changes", [
    {"conversation_id": None}, {"session_id": None}, {"reply_to": "reply"},
    {"turn_id": "turn"}, {"stream_id": "stream"}, {"seq": 0},
    {"trace_id": "trace"}, {"content_type": "text/plain"}, {"flags": {"idempotent": True}},
    {"body": {"text": "x", "display_name": "Context", "sensitivity": "private", "path": "/private"}},
    {"body": {"text": "x" * 8193, "display_name": "Context", "sensitivity": "private"}},
    {"body": {"text": " ", "display_name": "Context", "sensitivity": "private"}},
    {"body": {"text": "x", "display_name": "Context", "sensitivity": "unknown"}},
    {"body": {"text": "x", "display_name": "bad\nname", "sensitivity": "private"}},
])
def test_context_wire_rejects_invalid_envelopes(changes):
    with pytest.raises(ProtocolValidationError):
        encode_message(acquire_message(**changes))


def test_context_wire_limit_counts_utf8_and_preserves_json_escape_budget():
    for text in ("é" * 4096, "\x01" * 8192):
        command = command_from_message(decode_message(encode_message(acquire_message(
            body={"text": text, "display_name": "Context", "sensitivity": "private"}))).message)
        assert command.text == text
    with pytest.raises(ProtocolValidationError):
        encode_message(acquire_message(body={"text": "é" * 4097, "display_name": "Context", "sensitivity": "private"}))


@pytest.mark.parametrize("opcode", ["context.text.acquire", "context.text.revoke"])
def test_context_mutations_require_context_write(opcode):
    assert SecureZaraZmqGateway._capability_for(opcode) is Capability.CONTEXT_WRITE


def test_cli_accepts_explicit_bounded_stdin_context(monkeypatch):
    import zara.__main__ as cli
    from test_cli_context_ids_red import _install_config
    _install_config(monkeypatch)
    captured = {}
    monkeypatch.setattr(cli, "_run_connected_text", lambda *args, **kwargs: captured.update(kwargs) or 0)
    monkeypatch.setattr(sys, "argv", ["zara", "--connect", "ipc:///tmp/test", "--conversation-id", "chat", "--context-stdin", "inspect"])
    monkeypatch.setattr(sys, "stdin", io.TextIOWrapper(io.BytesIO(b"private stdin"), encoding="utf-8"))
    with pytest.raises(SystemExit) as result:
        cli.main()
    assert result.value.code == 0
    assert captured["context_text"] == "private stdin"


@pytest.fixture
def live_context_runtime(monkeypatch):
    from types import SimpleNamespace
    import tempfile
    import zmq
    from zara.runtime.bridge import RuntimeEventBus
    from zara.runtime.host import RuntimeHost
    from zara.server import PrincipalContext, ServerState
    from zara.zmq_transport import ZaraZmqGateway, TransportConfig
    from test_context_acquisition import build_backend
    from test_runtime_host import stop_host

    backend, manager, memory, llm = build_backend(monkeypatch)
    bus = RuntimeEventBus()
    host = RuntimeHost(lambda: backend, publisher=bus.publish)
    commands = []

    def submit(principal, command):
        assert principal == manager.principal
        commands.append(command)
        return host.submit(command)

    supervisor = SimpleNamespace(state=ServerState.READY, submit=submit,
        subscribe=lambda principal, **kwargs: bus.subscribe(**kwargs))
    context = zmq.Context()
    directory = tempfile.TemporaryDirectory(prefix="zctx-", dir="/tmp")
    endpoint = f"ipc://{directory.name}/context.sock"
    config = TransportConfig(request_timeout=3.0)
    gateway = ZaraZmqGateway(endpoint, supervisor=supervisor, principal=PrincipalContext("owner"), context=context, config=config)
    try:
        host.start().result(timeout=5)
        gateway.start().result(timeout=5)
        yield SimpleNamespace(endpoint=endpoint, context=context, config=config,
            host=host, manager=manager, memory=memory, llm=llm, commands=commands, backend=backend, gateway=gateway)
    finally:
        gateway.close()
        stop_host(host)
        context.term()
        directory.cleanup()


def test_real_client_acquire_replay_submit_revoke_and_restart(live_context_runtime):
    from zara.runtime.commands import SubmitTurn, ContextAcquiredReceipt, ContextRevokedReceipt, RestartRuntime
    from zara.zmq_transport import ZmqZaraClient, ProtocolRemoteError
    run = live_context_runtime
    client = ZmqZaraClient(run.endpoint, context=run.context, config=run.config)
    try:
        client.start().result(timeout=5)
        acquire = AcquireTextContext(request_id="acquire-once", text="private span [00:01]", conversation_id="chat")
        receipt = client.submit(acquire).result(timeout=5)
        assert isinstance(receipt, ContextAcquiredReceipt)
        assert client.submit(acquire).result(timeout=5) == receipt
        assert len(run.commands) == 1 and run.llm.calls == []
        assert not isinstance(run.gateway._replay[("owner", "acquire-once")].command, AcquireTextContext)
        with pytest.raises(ProtocolRemoteError, match="different command"):
            client.submit(replace(acquire, text="different")).result(timeout=5)
        subscription = client.subscribe()
        turn = client.submit(SubmitTurn(text="inspect", conversation_id="chat", context_ids=(receipt.context_id,))).result(timeout=5)
        import zara.__main__ as cli
        assert cli._wait_for_daemon_turn(subscription, turn.turn_id) == "answer"
        from zara.runtime.events import AgentCompleted
        import time
        deadline = time.monotonic() + 5
        while True:
            event = subscription.get(timeout=max(0.01, deadline - time.monotonic())).event
            if isinstance(event, AgentCompleted) and event.turn_id == turn.turn_id:
                assert event.success
                break
            assert time.monotonic() < deadline
        subscription.close()
        assert any("private span [00:01]" in str(message.content) for message in run.llm.calls[0])
        assert [str(message.content) for message in run.manager.context_manager.history] == ["inspect", "answer"]
        assert len(run.memory.messages) == 2
        wrong = client.submit(RevokeTextContext(context_id=receipt.context_id, conversation_id="other")).result(timeout=5)
        assert isinstance(wrong, ContextRevokedReceipt) and not wrong.revoked
        assert client.submit(RevokeTextContext(context_id=receipt.context_id, conversation_id="chat")).result(timeout=5).revoked
        assert run.backend._delegate._attachment_store.diagnostics().item_count == 0
        newer = client.submit(replace(acquire, request_id="fresh")).result(timeout=5)
        run.host.submit(RestartRuntime()).result(timeout=5)
        assert not client.submit(RevokeTextContext(context_id=newer.context_id, conversation_id="chat")).result(timeout=5).revoked
    finally:
        client.close()


@pytest.mark.parametrize("failed_turn", [False, True])
def test_real_cli_paste_reaches_model_then_cleans_up(monkeypatch, live_context_runtime, capsys, failed_turn):
    import zara.__main__ as cli
    from zara.zmq_transport import ZmqZaraClient
    import zara.daemon_client as daemon_client
    run = live_context_runtime
    monkeypatch.setattr(daemon_client, "create_daemon_client", lambda endpoint: ZmqZaraClient(endpoint, context=run.context, config=run.config))
    if failed_turn:
        async def fail(_messages):
            raise RuntimeError("synthetic model unavailable")
        run.llm.ainvoke = fail
        monkeypatch.setattr(cli, "CLI_TURN_TIMEOUT_SECONDS", 1.0)
    result = cli._run_connected_text(run.endpoint, "inspect", conversation_id="chat", context_text="private stdin")
    assert result == (2 if failed_turn else 0)
    assert [type(command).__name__ for command in run.commands] == ["AcquireTextContext", "SubmitTurn", "RevokeTextContext"]
    assert run.backend._delegate._attachment_store.diagnostics().item_count == 0
    if not failed_turn:
        assert any("private stdin" in str(message.content) for message in run.llm.calls[0])
    output = capsys.readouterr()
    assert "private stdin" not in output.out + output.err


@pytest.mark.parametrize("raw", [b"", b" ", b"\xff", b"x" * 8193], ids=["empty", "blank", "invalid-utf8", "oversized"])
def test_stdin_context_rejects_empty_invalid_or_oversized_without_echo(monkeypatch, raw):
    import zara.__main__ as cli
    monkeypatch.setattr(sys, "stdin", io.TextIOWrapper(io.BytesIO(raw), encoding="utf-8"))
    with pytest.raises(ValueError, match="1..8192") as error:
        cli._read_stdin_context()
    assert "xxx" not in str(error.value)


def test_stdin_reads_only_bounded_bytes(monkeypatch):
    import zara.__main__ as cli
    class Reader:
        def read(self, limit):
            assert limit == 8193
            return b"x" * limit
    from types import SimpleNamespace
    monkeypatch.setattr(sys, "stdin", SimpleNamespace(buffer=Reader()))
    with pytest.raises(ValueError):
        cli._read_stdin_context()


@pytest.mark.parametrize("revoke, response_type, conversation, reference, session", [
    (False, "turn.accepted", "chat", "ctx-ref", "session"),
    (False, "context.text.acquired", "other", "ctx-ref", "session"),
    (True, "context.text.acquired", "chat", "ctx-ref", "session"),
    (True, "context.text.revoked", "chat", "ctx-other", "session"),
    (False, "context.text.acquired", "chat", "ctx-ref", "stale-session"),
    (True, "context.text.revoked", "chat", "ctx-ref", "stale-session"),
])
def test_client_context_receipts_must_match_operation_conversation_and_reference(revoke, response_type, conversation, reference, session):
    from zara.client import ZaraClientState
    from zara.zmq_transport import ZmqZaraClient
    client = ZmqZaraClient("inproc://context-receipt-unit")
    try:
        client._state = ZaraClientState.READY
        client._session_id = "session"
        command = (RevokeTextContext(request_id="req", context_id="ctx-ref", conversation_id="chat") if revoke
                   else AcquireTextContext(request_id="req", text="private", conversation_id="chat"))
        future = client.submit(command)
        body = {"context_id": reference}
        if response_type == "context.text.revoked":
            body["revoked"] = True
        client._resolve_pending(client._pending.pop("req"), ProtocolMessage(
            type=response_type, id="response", reply_to="req", session_id=session, conversation_id=conversation,
            timestamp_ns=1, payload_count=0, body=body))
        assert isinstance(future.exception(timeout=1), ProtocolValidationError)
    finally:
        client.close()


@pytest.mark.parametrize("failure", ["acquire-timeout", "revoke-timeout", "wrong-revoke", "invalid-reference"])
def test_cli_bounds_context_waits_and_validates_receipts(monkeypatch, capsys, failure):
    import concurrent.futures
    import zara.__main__ as cli
    from zara.runtime.commands import ContextAcquiredReceipt, ContextRevokedReceipt, CommandReceipt
    from test_cli_context_ids_red import _Client, _install_client

    class Client(_Client):
        closed = False
        def submit(self, command):
            self.commands.append(command)
            if isinstance(command, AcquireTextContext):
                if failure == "acquire-timeout":
                    return concurrent.futures.Future()
                return self._done(ContextAcquiredReceipt(request_id=command.request_id,
                    conversation_id="chat", context_id="" if failure == "invalid-reference" else "ctx-ref"))
            if isinstance(command, RevokeTextContext):
                if failure == "revoke-timeout":
                    return concurrent.futures.Future()
                return self._done(CommandReceipt(request_id=command.request_id) if failure == "wrong-revoke" else
                    ContextRevokedReceipt(request_id=command.request_id, context_id="ctx-ref", revoked=True))
            return self._done(CommandReceipt(request_id=command.request_id, turn_id="turn"))
        def close(self):
            self.closed = True

    client = Client()
    _install_client(monkeypatch, client)
    monkeypatch.setattr(cli, "CLI_CONTEXT_TIMEOUT_SECONDS", 0.01, raising=False)
    monkeypatch.setattr(cli, "_wait_for_daemon_turn", lambda *_args: "answer")
    assert cli._run_connected_text("ipc:///tmp/test", "inspect", conversation_id="chat", context_text="private payload") == 2
    assert client.closed
    stderr = capsys.readouterr().err
    assert "private payload" not in stderr
    if failure == "acquire-timeout":
        assert "timed out" in stderr and "retention" in stderr
    if failure in {"acquire-timeout", "invalid-reference"}:
        assert len(client.commands) == 1


@pytest.mark.parametrize("granted", [False, True])
@pytest.mark.parametrize("revoke", [False, True])
def test_secure_context_write_denied_or_bound_to_authenticated_principal(granted, revoke):
    import concurrent.futures
    import zmq
    from zara.runtime.commands import ContextAcquiredReceipt, ContextRevokedReceipt
    from zara.security import SecurityRegistry, SecurityLimits
    from zara.security_transport import CurveClientConfig, CurveServerConfig
    from zara.server import PrincipalContext
    from zara.zmq_transport import ZmqZaraClient, ProtocolRemoteError, TransportConfig
    from test_daemon_security_abuse import FakeSupervisor, keypair, tcp_endpoint

    class Supervisor(FakeSupervisor):
        def submit(self, principal, command):
            self.commands.append((principal, command))
            future = concurrent.futures.Future()
            future.set_result(ContextRevokedReceipt(request_id=command.request_id, context_id=command.context_id, revoked=False)
                if isinstance(command, RevokeTextContext) else ContextAcquiredReceipt(request_id=command.request_id,
                    context_id="ctx-auth", conversation_id=command.conversation_id))
            return future

    context = zmq.Context()
    server_public, server_secret = keypair()
    public, secret = keypair()
    registry = SecurityRegistry()
    principal = PrincipalContext("alice", kind="authenticated")
    registry.enroll(public, principal=principal, device_id="test-device", capabilities={
        Capability.SESSION_BASIC, Capability.TURN_SUBMIT,
        *({Capability.CONTEXT_WRITE} if granted else set()),
    })
    endpoint = tcp_endpoint()
    supervisor = Supervisor()
    config = TransportConfig(request_timeout=2.0)
    gateway = SecureZaraZmqGateway(endpoint, supervisor=supervisor, context=context, config=config,
        security_registry=registry, curve_server=CurveServerConfig(public_key=server_public, secret_key=server_secret, zap_domain="zara"),
        security_limits=SecurityLimits())
    client = ZmqZaraClient(endpoint, context=context, config=config,
        curve_client=CurveClientConfig(public_key=public, secret_key=secret, server_public_key=server_public))
    try:
        gateway.start().result(timeout=5)
        client.start().result(timeout=5)
        future = client.submit(RevokeTextContext(context_id="ctx-auth", conversation_id="chat") if revoke
            else AcquireTextContext(text="private authenticated input", conversation_id="chat"))
        if granted:
            assert future.result(timeout=5).context_id == "ctx-auth"
            assert supervisor.commands[0][0] == principal
            assert len(supervisor.commands) == 1
        else:
            with pytest.raises(ProtocolRemoteError):
                future.result(timeout=5)
            assert supervisor.commands == []
    finally:
        client.close()
        gateway.close()
        context.term()


@pytest.mark.parametrize("message_type, body", [
    ("context.text.acquire", {"text": "x", "display_name": "\ud800", "sensitivity": "private"}),
    ("context.text.revoke", {"context_id": "\ud800"}),
    ("context.text.acquired", {"context_id": "\ud800"}),
    ("context.text.revoked", {"context_id": "ctx-ref", "revoked": 1}),
    ("context.text.revoked", {"context_id": "ctx-ref", "revoked": False, "text": "private"}),
    ("context.text.revoke", {"context_id": ""}),
    ("context.text.acquired", {"context_id": "bad\nref"}),
])
def test_context_malformed_metadata_fails_closed_without_raw_unicode_errors(message_type, body):
    response = message_type in {"context.text.acquired", "context.text.revoked"}
    with pytest.raises(ProtocolValidationError):
        encode_message(acquire_message(type=message_type, body=body, reply_to="req" if response else None))


@pytest.mark.parametrize("arguments", [
    ["inspect"], ["--conversation-id", "chat"],
    ["--conversation-id", "chat", "--standalone", "inspect"],
    ["--conversation-id", "chat", "--wake", "inspect"],
    ["--conversation-id", "chat", "--console", "inspect"],
    ["--conversation-id", "chat", "--cancel-turn", "turn", "inspect"],
])
def test_cli_context_stdin_rejects_unsupported_modes_before_reading(monkeypatch, arguments):
    import zara.__main__ as cli
    from test_cli_context_ids_red import _install_config
    _install_config(monkeypatch)
    monkeypatch.setattr(sys, "argv", ["zara", "--context-stdin", *arguments])
    monkeypatch.setattr(cli, "_read_stdin_context", lambda: pytest.fail("must not read stdin"))
    with pytest.raises(SystemExit) as result:
        cli.main()
    assert result.value.code == 2


@pytest.mark.parametrize("revoke", [False, True])
def test_gateway_rejects_invalid_context_backend_receipt_without_payload(monkeypatch, live_context_runtime, revoke):
    import concurrent.futures
    from zara.runtime.commands import CommandReceipt
    from zara.zmq_transport import ZmqZaraClient, ProtocolRemoteError
    run = live_context_runtime
    def invalid(*_args):
        future = concurrent.futures.Future()
        future.set_result(CommandReceipt(request_id="req", detail="private backend payload"))
        return future
    monkeypatch.setattr(run.gateway._supervisor, "submit", invalid)
    client = ZmqZaraClient(run.endpoint, context=run.context, config=run.config)
    try:
        client.start().result(timeout=5)
        command = (RevokeTextContext(context_id="ctx-ref", conversation_id="chat") if revoke
                   else AcquireTextContext(text="private snapshot", conversation_id="chat"))
        with pytest.raises(ProtocolRemoteError, match="runtime command failed") as error:
            client.submit(command).result(timeout=5)
        assert "private" not in str(error.value)
    finally:
        client.close()


def test_direct_codec_invalid_context_metadata_is_generic():
    from zara.protocol_runtime import RuntimeCodecError
    with pytest.raises(RuntimeCodecError, match="invalid context command"):
        command_from_message(acquire_message(body={"text": "private", "display_name": "Context", "sensitivity": "invalid"}))


def test_context_text_unpaired_unicode_is_protocol_error():
    with pytest.raises(ProtocolValidationError):
        encode_message(acquire_message(body={"text": "\ud800", "display_name": "Context", "sensitivity": "private"}))


@pytest.mark.parametrize("missing_conversation", [False, True])
def test_cli_rejects_wrong_acquisition_receipt_or_missing_chat(monkeypatch, missing_conversation):
    import zara.__main__ as cli
    from zara.runtime.commands import ContextAcquiredReceipt
    from test_cli_context_ids_red import _Client, _install_client
    class Client(_Client):
        def submit(self, command):
            self.commands.append(command)
            return self._done(ContextAcquiredReceipt(request_id=command.request_id, context_id="ctx-ref", conversation_id="other"))
    client = Client()
    _install_client(monkeypatch, client)
    assert cli._run_connected_text("ipc:///tmp/test", "inspect", conversation_id=None if missing_conversation else "chat", context_text="private") == 2
    assert len(client.commands) == (0 if missing_conversation else 1)


def test_cli_rejects_bad_stdin_before_creating_client(monkeypatch):
    import zara.__main__ as cli
    from test_cli_context_ids_red import _install_config
    _install_config(monkeypatch)
    monkeypatch.setattr(sys, "argv", ["zara", "--connect", "ipc:///tmp/test", "--conversation-id", "chat", "--context-stdin", "inspect"])
    monkeypatch.setattr(sys, "stdin", io.TextIOWrapper(io.BytesIO(b"\xff"), encoding="utf-8"))
    monkeypatch.setattr(cli, "_run_connected_text", lambda *_args, **_kwargs: pytest.fail("must not connect"))
    with pytest.raises(SystemExit) as result:
        cli.main()
    assert result.value.code == 2
