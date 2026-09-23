from __future__ import annotations

import asyncio
import threading
import time
from pathlib import Path

import pytest

from zara.tts.qwen import Qwen3TTSClient


@pytest.mark.asyncio
@pytest.mark.parametrize("operation", ["register", "delete"])
async def test_cancelled_voice_mutation_waiter_releases_late_acquisition(
    operation: str,
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
):
    """A cancelled async waiter must not orphan a lock acquired by its worker."""
    client = Qwen3TTSClient("http://localhost:7860", total_timeout=2.0)
    audio = tmp_path / "authorized.wav"
    audio.write_bytes(b"RIFFxxxxWAVEfmt data")

    started = threading.Event()
    allow_acquire = threading.Event()
    acquired = threading.Event()
    released = threading.Event()
    handle = object()

    def late_acquire(name: str, *args, **kwargs):
        assert name == "same_voice"
        started.set()
        assert allow_acquire.wait(timeout=2.0)
        acquired.set()
        return handle

    def observe_release(acquired_handle) -> None:
        assert acquired_handle is handle
        released.set()

    monkeypatch.setattr(client, "_acquire_voice_mutation_lock", late_acquire)
    monkeypatch.setattr(client, "_release_voice_mutation_lock", observe_release)

    if operation == "register":
        mutation = client.register_voice("same_voice", str(audio), "fixture")
    else:
        mutation = client.delete_voice("same_voice")

    task = asyncio.create_task(mutation)
    assert await asyncio.to_thread(started.wait, 1.0)

    task.cancel()
    allow_acquire.set()
    with pytest.raises(asyncio.CancelledError):
        await asyncio.wait_for(task, timeout=1.0)

    assert await asyncio.to_thread(acquired.wait, 1.0)
    await asyncio.sleep(0)
    assert released.is_set(), "late-acquired voice mutation lock was orphaned"


@pytest.mark.asyncio
@pytest.mark.parametrize("operation", ["register", "delete"])
async def test_cancelled_voice_mutation_waiter_does_not_retain_real_flock(
    operation: str,
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
):
    """Cancel a real contended flock waiter and prove the next mutation can lock."""
    client = Qwen3TTSClient("http://localhost:7860", total_timeout=2.0)
    client.voice_mutation_timeout = 0.5
    audio = tmp_path / "authorized.wav"
    audio.write_bytes(b"RIFFxxxxWAVEfmt data")
    name = f"same_voice_{operation}_{time.time_ns()}"

    original_acquire = client._acquire_voice_mutation_lock
    started = threading.Event()

    def observed_acquire(
        lock_name: str,
        cancelled: threading.Event | None = None,
    ):
        started.set()
        return original_acquire(lock_name, cancelled)

    holder = original_acquire(name)
    monkeypatch.setattr(client, "_acquire_voice_mutation_lock", observed_acquire)
    try:
        if operation == "register":
            mutation = client.register_voice(name, str(audio), "fixture")
        else:
            mutation = client.delete_voice(name)

        task = asyncio.create_task(mutation)
        assert await asyncio.to_thread(started.wait, 1.0)
        task.cancel()
        with pytest.raises(asyncio.CancelledError):
            await asyncio.wait_for(task, timeout=1.0)
    finally:
        client._release_voice_mutation_lock(holder)

    acquired_at = time.monotonic()
    next_handle = await asyncio.wait_for(
        asyncio.to_thread(original_acquire, name),
        timeout=0.25,
    )
    elapsed = time.monotonic() - acquired_at
    try:
        assert elapsed < 0.25
    finally:
        client._release_voice_mutation_lock(next_handle)
