from __future__ import annotations

import threading

from zara.runtime import events
from zara.runtime.backend import RuntimeBackend, RuntimeTurnResult
from zara.runtime.commands import SubmitTurn
from zara.runtime.host import RuntimeHost, RuntimeHostState


class PublisherAwareBackend(RuntimeBackend):
    def __init__(self) -> None:
        self.publisher = None
        self.started_after_bind = False

    def bind_event_publisher(self, publisher) -> None:
        self.publisher = publisher

    async def start(self) -> None:
        self.started_after_bind = self.publisher is not None

    async def submit_turn(
        self,
        text: str,
        *,
        turn_id: str,
        conversation_id=None,
        context_ids=(),
    latency_trace=None,
    ) -> RuntimeTurnResult:
        self.publisher(
            events.ToolStarted(
                turn_id=turn_id,
                conversation_id=conversation_id,
                label="test-backend",
                tool_run_id="tool-1",
                tool_name="calculator",
            )
        )
        return RuntimeTurnResult(response="done")


def test_runtime_host_binds_custom_publisher_into_backend():
    backend = PublisherAwareBackend()
    published = []
    output_ready = threading.Event()

    def publisher(event):
        published.append(event)
        if isinstance(event, events.OutputReady):
            output_ready.set()
        return None

    host = RuntimeHost(lambda: backend, publisher=publisher)
    try:
        host.start().result(timeout=5)
        receipt = host.submit(
            SubmitTurn(text="exercise publisher", conversation_id="conversation-1")
        ).result(timeout=5)
        assert receipt.turn_id is not None
        assert output_ready.wait(timeout=5)
        assert backend.publisher is publisher
        assert backend.started_after_bind is True
        assert any(
            isinstance(event, events.ToolStarted)
            and event.turn_id == receipt.turn_id
            and event.tool_name == "calculator"
            for event in published
        )
    finally:
        if host.state not in {
            RuntimeHostState.NEW,
            RuntimeHostState.STOPPED,
            RuntimeHostState.FAILED,
        }:
            host.shutdown("test cleanup").result(timeout=5)
        host.join(timeout=5)
