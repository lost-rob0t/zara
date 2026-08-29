from __future__ import annotations

import concurrent.futures
import threading

import pytest
import zmq

from zara.zmq_transport import ZmqZaraClient


def _future(*, result=None, error: BaseException | None = None):
    future = concurrent.futures.Future()
    if error is not None:
        future.set_exception(error)
    else:
        future.set_result(result)
    return future


def test_reconnect_with_backoff_retries_with_capped_exponential_delay(monkeypatch):
    context = zmq.Context()
    client = ZmqZaraClient("inproc://reconnect-backoff", context=context)
    attempts = iter(
        [
            _future(error=ConnectionError("first")),
            _future(error=ConnectionError("second")),
            _future(error=ConnectionError("third")),
            _future(result=True),
        ]
    )
    delays = []
    calls = []

    def reconnect():
        calls.append(True)
        return next(attempts)

    monkeypatch.setattr(client, "reconnect", reconnect)

    try:
        future = client.reconnect_with_backoff(
            max_attempts=4,
            initial_delay=0.1,
            max_delay=0.25,
            sleeper=delays.append,
        )

        assert future.result(timeout=1.0) is True
        assert len(calls) == 4
        assert delays == [0.1, 0.2, 0.25]
    finally:
        context.term()


def test_reconnect_with_backoff_is_bounded_and_surfaces_last_failure(monkeypatch):
    context = zmq.Context()
    client = ZmqZaraClient("inproc://reconnect-backoff-exhausted", context=context)
    delays = []
    calls = []

    def reconnect():
        calls.append(True)
        return _future(error=ConnectionError(f"attempt-{len(calls)}"))

    monkeypatch.setattr(client, "reconnect", reconnect)

    try:
        future = client.reconnect_with_backoff(
            max_attempts=3,
            initial_delay=0.05,
            max_delay=0.1,
            sleeper=delays.append,
        )

        with pytest.raises(ConnectionError, match="attempt-3"):
            future.result(timeout=1.0)
        assert len(calls) == 3
        assert delays == [0.05, 0.1]
    finally:
        context.term()


def test_reconnect_with_backoff_coalesces_concurrent_callers(monkeypatch):
    context = zmq.Context()
    client = ZmqZaraClient("inproc://reconnect-backoff-single-generation", context=context)
    reconnect_started = threading.Event()
    pending = concurrent.futures.Future()
    calls = []

    def reconnect():
        calls.append(True)
        reconnect_started.set()
        return pending

    monkeypatch.setattr(client, "reconnect", reconnect)

    try:
        first = client.reconnect_with_backoff(
            max_attempts=2,
            initial_delay=0.01,
            max_delay=0.02,
            sleeper=lambda _delay: None,
        )
        assert reconnect_started.wait(timeout=1.0)

        second = client.reconnect_with_backoff(
            max_attempts=2,
            initial_delay=0.01,
            max_delay=0.02,
            sleeper=lambda _delay: None,
        )
        coalesced = second is first
        pending.set_result(True)

        assert first.result(timeout=1.0) is True
        assert second.result(timeout=1.0) is True
        assert coalesced is True
        assert len(calls) == 1
    finally:
        if not pending.done():
            pending.set_result(True)
        context.term()
