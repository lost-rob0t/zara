"""Immediate pre-generated acknowledgement playback (ZARA-025).

After wake/turn acceptance, plays a short acknowledgement such as
"Okay" while speech capture and routing continue.

Production flow:
1. On startup, check ``$XDG_CACHE_HOME/zarathushtra/acknowledgement.wav``
   for a previously cached clip.
2. If not cached, generate via the configured TTS provider (edge-tts,
   elevenlabs, qwen3) and write to the XDG cache directory so subsequent
   restarts are instant.
3. If TTS generation fails, fall back to the bundled fixture at
   ``assets/sounds/acknowledgement.wav``.

The acknowledgement is:
- **Immediate**: starts before STT, Prolog, or LLM work.
- **Non-blocking**: plays in a background thread; the caller never waits.
- **Barge-in safe**: ``stop()`` halts playback when the user speaks.
- **Failure-safe**: if playback fails, the error is logged and the turn
  proceeds normally.
- **Exactly-once per turn**: duplicate wake events do not replay the
  acknowledgement for the same turn.
- **No success wording**: the acknowledgement is a neutral filler, never
  a confirmation of command execution.
"""

from __future__ import annotations

import asyncio
import hashlib
import logging
import os
import threading
import wave
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Optional

logger = logging.getLogger(__name__)

DEFAULT_ACK_PHRASE = "Okay"
DEFAULT_FIXTURE_PATH = Path(__file__).resolve().parent.parent / "assets" / "sounds" / "acknowledgement.wav"


def _cache_dir() -> Path:
    """Return the XDG cache directory for Zarathushtra."""
    xdg_cache = os.getenv("XDG_CACHE_HOME")
    if xdg_cache:
        return Path(xdg_cache) / "zarathushtra"
    return Path.home() / ".cache" / "zarathushtra"


def _cache_path(provider: str, voice: str, phrase: str) -> Path:
    """Return a cache path keyed by provider+voice+phrase."""
    digest = hashlib.sha256(
        f"{provider}:{voice}:{phrase}".encode()
    ).hexdigest()[:16]
    return _cache_dir() / f"acknowledgement-{digest}.wav"


@dataclass(frozen=True)
class AcknowledgementConfig:
    """Configuration for immediate acknowledgement playback."""
    enabled: bool = True
    phrase: str = DEFAULT_ACK_PHRASE
    fixture_path: str = ""
    volume: float = 1.0
    provider: str = "edge"
    voice: str = "en-US-AriaNeural"


@dataclass
class AcknowledgementResult:
    """Result of an acknowledgement playback attempt."""
    turn_id: str
    played: bool = False
    suppressed: bool = False
    error: Optional[str] = None
    source: str = ""


class AcknowledgementPlayer:
    """Plays a pre-generated acknowledgement audio clip without blocking.

    On ``initialize()``, the player:

    1. Checks the XDG cache for a previously generated clip matching the
       configured provider+voice+phrase.
    2. If no cache hit, generates via the real TTS provider and writes the
       result to the cache so restarts are instant.
    3. If TTS generation fails, loads the bundled fixture at
       ``assets/sounds/acknowledgement.wav``.

    Playback runs in a background daemon thread so the caller is never
    blocked. ``stop()`` halts playback for barge-in. Each turn gets
    exactly one acknowledgement.
    """

    def __init__(
        self,
        config: Optional[AcknowledgementConfig] = None,
        tts_engine: Optional[Any] = None,
    ):
        self.config = config or AcknowledgementConfig()
        self._tts_engine = tts_engine
        self._audio_bytes: Optional[bytes] = None
        self._audio_sample_rate: int = 16000
        self._played_turns: set[str] = set()
        self._playback_thread: Optional[threading.Thread] = None
        self._stop_event = threading.Event()
        self._lock = threading.Lock()
        self._is_playing = False
        self._source: str = ""

    def initialize(self) -> None:
        """Load or generate the acknowledgement audio at startup.

        Tries the XDG cache first, then the real TTS provider, then the
        bundled fixture as a last resort. Failure is logged but never
        raised — the turn proceeds without acknowledgement if the audio
        cannot be loaded.
        """
        if not self.config.enabled:
            logger.info("[Ack] disabled by config")
            return

        cache = _cache_path(
            self.config.provider, self.config.voice, self.config.phrase
        )
        if cache.exists():
            if self._load_wav(cache, source="cache"):
                return

        if self._generate_via_tts(cache):
            return

        fixture = (
            Path(self.config.fixture_path)
            if self.config.fixture_path
            else DEFAULT_FIXTURE_PATH
        )
        self._load_wav(fixture, source="fixture")

    def _load_wav(self, path: Path, source: str) -> bool:
        try:
            with wave.open(str(path), "rb") as wf:
                self._audio_bytes = wf.readframes(wf.getnframes())
                self._audio_sample_rate = wf.getframerate()
            self._source = source
            logger.info(
                "[Ack] loaded from %s: %s (%d bytes, %d Hz)",
                source, path, len(self._audio_bytes or b""),
                self._audio_sample_rate,
            )
            return True
        except Exception as error:
            logger.warning("[Ack] failed to load %s %s: %s", source, path, error)
            return False

    def _generate_via_tts(self, cache_path: Path) -> bool:
        """Generate acknowledgement audio via the configured TTS provider.

        Writes the result to ``cache_path`` so subsequent restarts load
        from the cache without invoking the provider again.
        """
        if self._tts_engine is None:
            return self._generate_edge_tts(cache_path)

        try:
            loop = asyncio.new_event_loop()
            synthesis = loop.run_until_complete(
                self._tts_engine.synthesize_async(self.config.phrase)
            )
            loop.close()
            if synthesis.success and synthesis.audio:
                self._write_wav(cache_path, synthesis.audio)
                self._audio_bytes = synthesis.audio
                self._audio_sample_rate = 16000
                self._source = f"tts:{synthesis.provider}"
                logger.info(
                    "[Ack] generated via %s and cached at %s",
                    synthesis.provider, cache_path,
                )
                return True
            logger.warning("[Ack] TTS synthesis failed: %s", synthesis.error)
        except Exception as error:
            logger.warning("[Ack] TTS generation error: %s", error)
        return False

    def _generate_edge_tts(self, cache_path: Path) -> bool:
        """Generate acknowledgement via edge-tts as a lightweight default."""
        try:
            import edge_tts
            import tempfile
            import subprocess

            cache_path.parent.mkdir(parents=True, exist_ok=True)
            tmp_mp3 = cache_path.with_suffix(".tmp.mp3")

            loop = asyncio.new_event_loop()
            comm = edge_tts.Communicate(
                self.config.phrase, voice=self.config.voice
            )
            loop.run_until_complete(comm.save(str(tmp_mp3)))
            loop.close()

            if not tmp_mp3.exists() or tmp_mp3.stat().st_size == 0:
                logger.warning("[Ack] edge-tts produced no audio")
                return False

            proc = subprocess.run(
                [
                    "mpv", "--no-video", "--no-terminal", "--really-quiet",
                    "--ao=pcm", "--ao-pcm-waveheader=no",
                    "--ao-pcm-file=/dev/stdout",
                    "--audio-channels=mono",
                    "--audio-format=s16",
                    "--audio-samplerate=16000",
                    str(tmp_mp3),
                ],
                capture_output=True,
                timeout=10,
            )
            tmp_mp3.unlink(missing_ok=True)

            if proc.returncode != 0 or not proc.stdout:
                logger.warning("[Ack] mpv conversion failed")
                return False

            self._write_wav(cache_path, proc.stdout)
            self._audio_bytes = proc.stdout
            self._audio_sample_rate = 16000
            self._source = "edge-tts"
            logger.info("[Ack] generated via edge-tts and cached at %s", cache_path)
            return True
        except Exception as error:
            logger.warning("[Ack] edge-tts generation error: %s", error)
            return False

    @staticmethod
    def _write_wav(path: Path, pcm_bytes: bytes) -> None:
        path.parent.mkdir(parents=True, exist_ok=True)
        with wave.open(str(path), "wb") as wf:
            wf.setnchannels(1)
            wf.setsampwidth(2)
            wf.setframerate(16000)
            wf.writeframes(pcm_bytes)

    @property
    def is_playing(self) -> bool:
        return self._is_playing

    @property
    def has_audio(self) -> bool:
        return self._audio_bytes is not None

    @property
    def source(self) -> str:
        return self._source

    def play(self, turn_id: str) -> AcknowledgementResult:
        """Start non-blocking acknowledgement playback for a turn.

        Returns immediately. If the acknowledgement is disabled, has no
        audio, or was already played for this turn, returns a result
        with ``played=False``.
        """
        if not self.config.enabled:
            return AcknowledgementResult(
                turn_id=turn_id, played=False, suppressed=True
            )

        if self._audio_bytes is None:
            return AcknowledgementResult(
                turn_id=turn_id, played=False, error="no audio loaded"
            )

        with self._lock:
            if turn_id in self._played_turns:
                return AcknowledgementResult(
                    turn_id=turn_id, played=False, suppressed=True
                )
            self._played_turns.add(turn_id)

        self._stop_event.clear()
        self._playback_thread = threading.Thread(
            target=self._play_thread,
            name=f"ack-{turn_id}",
            daemon=True,
        )
        self._playback_thread.start()
        return AcknowledgementResult(
            turn_id=turn_id, played=True, source=self._source
        )

    def _play_thread(self) -> None:
        try:
            self._is_playing = True
            import sounddevice as sd
            import numpy as np

            samples = np.frombuffer(self._audio_bytes, dtype=np.int16)
            if self.config.volume != 1.0:
                samples = (
                    samples.astype(np.float32) * self.config.volume
                ).clip(-32768, 32767).astype(np.int16)
            sd.play(samples, self._audio_sample_rate)
            while sd.get_stream().active:
                if self._stop_event.is_set():
                    sd.stop()
                    break
                import time
                time.sleep(0.02)
        except Exception as error:
            logger.warning("[Ack] playback error: %s", error)
        finally:
            self._is_playing = False

    def stop(self) -> None:
        """Stop the current acknowledgement playback (for barge-in)."""
        self._stop_event.set()
        if self._playback_thread and self._playback_thread.is_alive():
            self._playback_thread.join(timeout=0.5)

    def reset(self) -> None:
        """Clear the played-turns set (e.g., between sessions)."""
        with self._lock:
            self._played_turns.clear()
        self.stop()
