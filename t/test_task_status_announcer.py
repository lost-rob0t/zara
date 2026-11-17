"""Terse spoken summaries for long-horizon task lifecycle events."""

import asyncio
from types import SimpleNamespace
from unittest.mock import AsyncMock

import pytest

from zara.runtime import events
from zara.runtime.task_announcer import TaskStatusAnnouncer


def wav_bytes():
    import io
    import wave

    output = io.BytesIO()
    with wave.open(output, "wb") as audio:
        audio.setnchannels(1)
        audio.setsampwidth(2)
        audio.setframerate(16000)
        audio.writeframes(b"\x00\x00" * 16)
    return output.getvalue()


class FakeEngine:
    def __init__(self):
        self.spoken = []

    async def synthesize_async(self, text):
        self.spoken.append(text)
        return SimpleNamespace(
            success=True,
            cancelled=False,
            error=None,
            audio=wav_bytes(),
            audio_format="wav",
        )

    async def close(self):
        pass


class Recorder:
    def __init__(self):
        self.events = []

    def __call__(self, event):
        self.events.append(event)


def make_announcer(engine=None, **kwargs):
    engine = engine if engine is not None else FakeEngine()
    recorder = Recorder()
    announcer = TaskStatusAnnouncer(
        engine_factory=lambda: engine,
        publish=recorder,
        sample_rate=24000,
        **kwargs,
    )
    return announcer, engine, recorder


@pytest.mark.asyncio
async def test_completed_event_speaks_terse_summary():
    announcer, engine, recorder = make_announcer()

    await announcer.handle_event(
        events.TaskCompleted(task_id="t1", label="Plants research")
    )

    assert engine.spoken == ["Plants research done."]
    assert [type(event) for event in recorder.events] == [
        events.AudioOutputStarted,
        events.AudioOutputChunk,
        events.AudioOutputFinished,
    ]


@pytest.mark.asyncio
async def test_started_and_step_events_are_silent():
    announcer, engine, recorder = make_announcer()

    await announcer.handle_event(
        events.TaskStarted(task_id="t1", label="Plants research")
    )
    await announcer.handle_event(
        events.TaskStepCompleted(task_id="t1", turn_id="turn-1", label="Plants", step_index=1)
    )

    assert engine.spoken == []
    assert recorder.events == []


@pytest.mark.asyncio
async def test_failed_event_speaks_failed_line():
    announcer, engine, _ = make_announcer()

    await announcer.handle_event(
        events.TaskFailed(task_id="t1", label="Backup sweep", reason="disk full")
    )

    assert engine.spoken == ["Backup sweep failed."]


@pytest.mark.asyncio
async def test_approval_and_cancel_events_speak():
    announcer, engine, _ = make_announcer()

    await announcer.handle_event(
        events.TaskWaitingApproval(task_id="t1", turn_id="turn-1", label="Deploy")
    )
    await announcer.handle_event(
        events.TaskCancelled(task_id="t2", label="Deploy", reason="user")
    )

    assert engine.spoken == ["Deploy needs approval.", "Deploy cancelled."]


@pytest.mark.asyncio
async def test_disabled_announcer_is_silent():
    announcer, engine, recorder = make_announcer(enabled=False)

    await announcer.handle_event(
        events.TaskCompleted(task_id="t1", label="Plants research")
    )

    assert engine.spoken == []
    assert recorder.events == []


@pytest.mark.asyncio
async def test_engine_failure_keeps_announcer_alive():
    engine = FakeEngine()

    async def flaky(text):
        if not engine.spoken:
            engine.spoken.append(text)
            raise RuntimeError("boom")
        engine.spoken.append(text)
        return SimpleNamespace(
            success=True, cancelled=False, error=None,
            audio=wav_bytes(), audio_format="wav",
        )

    engine.synthesize_async = flaky
    announcer, _, recorder = make_announcer(engine=engine)

    await announcer.handle_event(
        events.TaskCompleted(task_id="t1", label="First task")
    )
    await announcer.handle_event(
        events.TaskCompleted(task_id="t2", label="Second task")
    )

    assert engine.spoken == ["First task done.", "Second task done."]
    assert len(recorder.events) == 3


@pytest.mark.asyncio
async def test_missing_label_falls_back_to_task():
    announcer, engine, _ = make_announcer()

    await announcer.handle_event(events.TaskCompleted(task_id="t1", label=""))

    assert engine.spoken == ["Task done."]


class FakeSubscription:
    def __init__(self, items):
        self._items = list(items)

    def get_nowait(self):
        if not self._items:
            raise RuntimeError("empty")
        return SimpleNamespace(event=self._items.pop(0))


@pytest.mark.asyncio
async def test_start_drains_subscription_and_speaks():
    engine = FakeEngine()
    recorder = Recorder()
    subscription = FakeSubscription(
        [events.TaskCompleted(task_id="t1", label="Plants research")]
    )
    announcer = TaskStatusAnnouncer(
        engine_factory=lambda: engine,
        publish=recorder,
        sample_rate=24000,
        subscription=subscription,
        poll_interval=0.01,
    )

    announcer.start()
    for _ in range(100):
        if recorder.events:
            break
        await asyncio.sleep(0.01)
    announcer.stop()

    assert engine.spoken == ["Plants research done."]
    assert len(recorder.events) == 3


@pytest.mark.asyncio
async def test_start_without_subscription_is_inert():
    announcer, engine, recorder = make_announcer()

    announcer.start()
    await asyncio.sleep(0.02)
    announcer.stop()

    assert engine.spoken == []
    assert recorder.events == []


@pytest.mark.asyncio
async def test_close_shuts_engine_down():
    engine = FakeEngine()
    engine.close = AsyncMock()
    announcer, _, _ = make_announcer(engine=engine)

    await announcer.handle_event(
        events.TaskCompleted(task_id="t1", label="X")
    )
    await announcer.close()

    engine.close.assert_awaited_once()


@pytest.mark.asyncio
async def test_reset_engine_drops_cached_engine_so_provider_switch_applies_live():
    first_engine = FakeEngine()
    second_engine = FakeEngine()
    engines = [first_engine, second_engine]
    factory_calls = 0

    def factory():
        nonlocal factory_calls
        factory_calls += 1
        return engines[factory_calls - 1]

    announcer = TaskStatusAnnouncer(
        engine_factory=factory,
        publish=Recorder(),
        sample_rate=24000,
    )

    await announcer.handle_event(
        events.TaskCompleted(task_id="t1", label="First")
    )
    assert factory_calls == 1
    assert first_engine.spoken == ["First done."]

    announcer.reset_engine()

    await announcer.handle_event(
        events.TaskCompleted(task_id="t2", label="Second")
    )
    assert factory_calls == 2
    assert second_engine.spoken == ["Second done."]
    assert first_engine.spoken == ["First done."]


def test_reset_engine_without_cached_engine_does_not_build_one():
    def factory():
        raise AssertionError("reset_engine must not build an engine")

    announcer = TaskStatusAnnouncer(
        engine_factory=factory,
        publish=Recorder(),
        sample_rate=24000,
    )

    announcer.reset_engine()
