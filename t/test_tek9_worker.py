from __future__ import annotations

import json
from collections import deque

import pykka
import pytest

from zara.music.tek9_worker import (
    Tek9GenerationConflict,
    Tek9WorkerActor,
    Tek9WorkerClient,
    Tek9WorkerUnavailable,
)


class FakeStdin:
    def __init__(self, process):
        self.process = process
        self.closed = False

    def write(self, value):
        if self.closed:
            raise BrokenPipeError
        self.process.requests.append(json.loads(value))
        return len(value)

    def flush(self):
        if self.closed:
            raise BrokenPipeError

    def close(self):
        self.closed = True


class FakeStdout:
    def __init__(self, process):
        self.process = process
        self.closed = False

    def readline(self):
        if self.closed:
            return ""
        if self.process.responses:
            return self.process.responses.popleft() + "\n"
        return ""

    def close(self):
        self.closed = True


class FakeProcess:
    def __init__(self, responses):
        self.responses = deque(responses)
        self.requests = []
        self.stdin = FakeStdin(self)
        self.stdout = FakeStdout(self)
        self.returncode = None
        self.terminated = False

    def poll(self):
        return self.returncode

    def terminate(self):
        self.terminated = True
        self.returncode = 0

    def wait(self, timeout=None):
        return self.returncode

    def kill(self):
        self.returncode = -9


class Factory:
    def __init__(self, *processes):
        self.processes = deque(processes)
        self.calls = []

    def __call__(self, command, **kwargs):
        self.calls.append((list(command), kwargs))
        return self.processes.popleft()


@pytest.fixture(autouse=True)
def cleanup_actors():
    yield
    pykka.ActorRegistry.stop_all()


def test_client_status_uses_fixed_status_operation():
    process = FakeProcess(['{"ok":true,"generation":7,"watermark":"b-track"}'])
    client = Tek9WorkerClient.start(
        ["tek9-ingest"],
        popen_factory=Factory(process),
    )
    try:
        status = client.status("navidrome")
    finally:
        client.stop()

    assert status == {"ok": True, "generation": 7, "watermark": "b-track"}
    assert process.requests == [{"op": "status", "source_id": "navidrome"}]


def test_apply_batch_is_serialized_as_bounded_fixed_command():
    process = FakeProcess(['{"ok":true,"generation":8,"watermark":"c-track"}'])
    client = Tek9WorkerClient.start(
        ["tek9-ingest"],
        popen_factory=Factory(process),
    )
    try:
        result = client.apply_batch(
            source_id="navidrome",
            generation=8,
            expected_generation=7,
            documents=[{"id": "track:1", "value": {"dtype": "track"}}],
            nodes=[{"id": "track:1", "props": {"dtype": "track"}}],
            edges=[],
            graph_name="music",
            watermark="c-track",
        )
    finally:
        client.stop()

    assert result["generation"] == 8
    assert process.requests == [
        {
            "op": "apply_batch",
            "source_id": "navidrome",
            "generation": 8,
            "expected_generation": 7,
            "documents": [{"id": "track:1", "value": {"dtype": "track"}}],
            "nodes": [{"id": "track:1", "props": {"dtype": "track"}}],
            "edges": [],
            "graph_name": "music",
            "watermark": "c-track",
        }
    ]


def test_generation_conflict_is_typed_and_exposes_server_fence():
    process = FakeProcess(
        [
            '{"ok":false,"code":"generation_conflict",'
            '"expected_generation":1,"actual_generation":2,"requested_generation":3}'
        ]
    )
    client = Tek9WorkerClient.start(
        ["tek9-ingest"],
        popen_factory=Factory(process),
    )
    try:
        with pytest.raises(Tek9GenerationConflict) as raised:
            client.apply_batch(
                source_id="navidrome",
                generation=3,
                expected_generation=1,
            )
    finally:
        client.stop()

    assert raised.value.actual_generation == 2


def test_dead_worker_is_not_blindly_replayed_for_mutating_batch():
    dead = FakeProcess([])
    dead.returncode = 1
    replacement = FakeProcess(['{"ok":true,"generation":2,"watermark":null}'])
    factory = Factory(dead, replacement)
    client = Tek9WorkerClient.start(
        ["tek9-ingest"],
        popen_factory=factory,
    )
    try:
        with pytest.raises(Tek9WorkerUnavailable):
            client.apply_batch(
                source_id="navidrome",
                generation=2,
                expected_generation=1,
            )
        status = client.status("navidrome")
    finally:
        client.stop()

    assert status["generation"] == 2
    assert len(factory.calls) == 2
    assert replacement.requests == [{"op": "status", "source_id": "navidrome"}]


def test_actor_mailbox_blocks_instead_of_dropping_ingest_writes():
    assert Tek9WorkerActor.mailbox_overflow == "block"
    assert Tek9WorkerActor.mailbox_size > 0


def test_client_rejects_oversized_batch_before_worker():
    process = FakeProcess([])
    client = Tek9WorkerClient.start(
        ["tek9-ingest"],
        popen_factory=Factory(process),
        max_records=2,
    )
    try:
        with pytest.raises(ValueError, match="documents"):
            client.apply_batch(
                source_id="navidrome",
                generation=1,
                expected_generation=0,
                documents=[
                    {"id": "1", "value": {}},
                    {"id": "2", "value": {}},
                    {"id": "3", "value": {}},
                ],
            )
    finally:
        client.stop()

    assert process.requests == []
