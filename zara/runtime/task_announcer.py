"""Terse spoken summaries for long-horizon task lifecycle events."""

import asyncio
import logging
import threading
import uuid

from zara.runtime import events
from zara.runtime.tts_output import _decode_wav

logger = logging.getLogger(__name__)

_SPEAK_TEMPLATE = {
    events.TaskCompleted: "{label} done.",
    events.TaskFailed: "{label} failed.",
    events.TaskWaitingApproval: "{label} needs approval.",
    events.TaskCancelled: "{label} cancelled.",
}


class TaskStatusAnnouncer:
    """Speak terse task status lines through the shared TTS pipeline.

    Consumes the same runtime event bus as ``TtsOutputBridge`` but reacts
    to task lifecycle events instead of assistant output. Announcements are
    serialized in one worker thread so overlapping completions speak in
    order, and every announcement is bounded to one line.
    """

    def __init__(
        self,
        *,
        engine_factory,
        publish,
        sample_rate: int = 24000,
        enabled: bool = True,
        subscription=None,
        poll_interval: float = 0.05,
    ) -> None:
        self._engine_factory = engine_factory
        self._publish = publish
        self._sample_rate = int(sample_rate)
        self._enabled = bool(enabled)
        self._subscription = subscription
        self._poll_interval = poll_interval
        self._engine = None
        self._stop_event = threading.Event()
        self._thread = None

    def start(self) -> None:
        if self._thread is not None and self._thread.is_alive():
            return
        if self._subscription is None:
            return
        self._stop_event.clear()
        self._thread = threading.Thread(
            target=self._run_thread, name="zara-task-announcer", daemon=True
        )
        self._thread.start()

    def stop(self, timeout: float = 5.0) -> None:
        self._stop_event.set()
        thread = self._thread
        if thread is not None:
            thread.join(timeout)
            self._thread = None

    async def handle_event(self, event) -> None:
        template = _SPEAK_TEMPLATE.get(type(event))
        if template is None or not self._enabled:
            return
        label = (getattr(event, "label", "") or "").strip() or "Task"
        text = template.format(label=label)
        engine = self._get_engine()
        if engine is None:
            return
        try:
            result = await engine.synthesize_async(text)
        except Exception as error:
            logger.warning("Task status synthesis failed: %s", error)
            return
        if not getattr(result, "success", False) or not getattr(result, "audio", b""):
            return
        pcm = _decode_wav(result.audio, self._sample_rate)
        if not pcm:
            return
        stream_id = uuid.uuid4().hex
        turn_id = f"task-status-{stream_id[:12]}"
        self._publish(
            events.AudioOutputStarted(
                turn_id=turn_id,
                conversation_id="",
                stream_id=stream_id,
                sample_rate=self._sample_rate,
                channels=1,
            )
        )
        self._publish(
            events.AudioOutputChunk(
                turn_id=turn_id, conversation_id="", stream_id=stream_id, pcm=pcm
            )
        )
        self._publish(
            events.AudioOutputFinished(
                turn_id=turn_id, conversation_id="", stream_id=stream_id
            )
        )

    def _get_engine(self):
        if self._engine is None:
            try:
                self._engine = self._engine_factory()
            except Exception as error:
                logger.warning(
                    "TTS engine unavailable for task status: %s", error
                )
                return None
        return self._engine

    def reset_engine(self) -> None:
        """Drop the cached engine so the next announcement rebuilds it."""
        self._engine = None

    def _run_thread(self) -> None:
        asyncio.run(self._main())

    async def _main(self) -> None:
        while not self._stop_event.is_set():
            drained = False
            for _ in range(32):
                try:
                    envelope = self._subscription.get_nowait()
                except Exception:
                    break
                drained = True
                event = getattr(envelope, "event", envelope)
                try:
                    await self.handle_event(event)
                except Exception:
                    logger.exception("Task announcer event handling failed")
            if not drained:
                await asyncio.sleep(self._poll_interval)

    async def close(self) -> None:
        engine = self._engine
        self._engine = None
        if engine is not None and hasattr(engine, "close"):
            await engine.close()
