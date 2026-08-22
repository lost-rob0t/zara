import time

from zara.client import InProcessZaraClient, ZaraClientState
from zara.runtime import events
from zara.runtime.backend import RuntimeBackend, RuntimeTurnResult
from zara.runtime.commands import SubmitTurn


class EchoBackend(RuntimeBackend):
    def __init__(self):
        self.started = False
        self.stopped = False

    async def start(self):
        self.started = True

    async def submit_turn(self, text, *, turn_id, conversation_id=None, context_ids=()):
        return RuntimeTurnResult(response=f"echo:{text}")

    async def stop(self):
        self.stopped = True


def test_in_process_client_hides_runtime_host_and_delivers_events():
    backend = EchoBackend()
    client = InProcessZaraClient(backend_factory=lambda: backend, shutdown_timeout=1.0)
    subscription = client.subscribe()

    client.start().result(timeout=1.0)
    assert client.state is ZaraClientState.READY

    receipt = client.submit(SubmitTurn(text="hello")).result(timeout=1.0)
    assert receipt.turn_id

    response = None
    deadline = time.monotonic() + 1.0
    while time.monotonic() < deadline:
        envelope = subscription.get(timeout=max(0.01, deadline - time.monotonic()))
        if isinstance(envelope.event, events.ResponseText):
            response = envelope.event
            break

    assert response is not None
    assert response.text == "echo:hello"

    client.close(timeout=1.0)
    assert backend.stopped
    assert not client.is_alive
    assert client.state is ZaraClientState.STOPPED
