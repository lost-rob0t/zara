from __future__ import annotations

import concurrent.futures
import time

import pytest
import zmq

from zara.runtime import bridge
from zara.runtime.commands import CommandReceipt, SubmitTurn
from zara.server import PrincipalContext, ServerState
from zara.zmq_transport import TransportConfig, ZaraZmqGateway, ZmqZaraClient


class CountingSupervisor:
    def __init__(self) -> None:
        self.state = ServerState.READY
        self.commands = []
        self.bus = bridge.RuntimeEventBus()

    def submit(self, principal, command):
        assert isinstance(principal, PrincipalContext)
        self.commands.append((principal, command))
        future = concurrent.futures.Future()
        future.set_result(
            CommandReceipt(request_id=command.request_id, turn_id="turn-stable")
        )
        return future

    def subscribe(self, principal, *, maxsize=0):
        assert isinstance(principal, PrincipalContext)
        return self.bus.subscribe(maxsize=maxsize)


@pytest.fixture
def zmq_context():
    context = zmq.Context()
    try:
        yield context
    finally:
        context.term()


def test_duplicate_submit_id_survives_client_reconnect_without_second_side_effect(zmq_context):
    config = TransportConfig(
        sndhwm=8,
        rcvhwm=8,
        max_message_bytes=1024 * 1024,
        heartbeat_interval_ms=100,
        heartbeat_timeout_ms=500,
        linger_ms=0,
        request_timeout=1.0,
        pending_request_limit=8,
        idempotency_cache_size=8,
    )
    endpoint = f"inproc://reconnect-idempotency-{time.time_ns()}"
    principal = PrincipalContext("local-owner")
    supervisor = CountingSupervisor()
    gateway = ZaraZmqGateway(
        endpoint,
        supervisor=supervisor,
        principal=principal,
        context=zmq_context,
        config=config,
    )
    client = ZmqZaraClient(endpoint, context=zmq_context, config=config)

    try:
        gateway.start().result(timeout=1.0)
        client.start().result(timeout=1.0)

        command = SubmitTurn(request_id="stable-across-reconnect", text="do this once")
        first = client.submit(command).result(timeout=1.0)

        client.reconnect().result(timeout=1.0)
        second = client.submit(command).result(timeout=1.0)

        assert second == first
        submitted = [item for _, item in supervisor.commands if isinstance(item, SubmitTurn)]
        assert submitted == [command]
    finally:
        client.close(timeout=1.0)
        gateway.close(timeout=1.0)
