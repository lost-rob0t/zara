from __future__ import annotations

import asyncio
import threading
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
