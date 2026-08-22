from __future__ import annotations

import queue

import pytest
import zmq

from zara.protocol import ProtocolMessage
from zara.runtime import events
from zara.zmq_transport import TransportConfig, ZmqZaraClient


def test_client_publishes_turn_completed_as_agent_completed_event():
    context = zmq.Context()
    client = ZmqZaraClient(
        "inproc://turn-completed-client-test",
        context=context,
        config=TransportConfig(linger_ms=0, request_timeout=0.1),
    )
    subscription = client.subscribe(maxsize=1)

    try:
        client._publish_runtime_event(
            ProtocolMessage(
                type="turn.completed",
                id="event-1",
                timestamp_ns=1,
                payload_count=0,
                turn_id="turn-1",
                conversation_id="conversation-1",
                seq=9,
                body={"success": True},
            )
        )

        envelope = subscription.get(timeout=0.05)
        assert envelope.event == events.AgentCompleted(
            turn_id="turn-1",
            conversation_id="conversation-1",
            success=True,
        )
    except queue.Empty:
        pytest.fail("ZmqZaraClient dropped turn.completed instead of publishing AgentCompleted")
    finally:
        client.close(timeout=0.1)
        context.term()
