"""Immediate pre-generated acknowledgement playback (ZARA-025).

After wake/turn acceptance, plays a short acknowledgement such as
"Okay" or "Let me think about that" while speech capture and routing
continue.

Production flow:
1. On startup, check ``$XDG_CACHE_HOME/zarathushtra/`` for previously
   cached clips keyed by ``provider-voice-phrase``.
2. If not cached, generate via the configured TTS provider (edge-tts,
   elevenlabs, qwen3) and write to the XDG cache directory so subsequent
   restarts are instant.
3. If TTS generation fails, fall back to the bundled fixture at
   ``assets/sounds/acknowledgement.wav``.

Multiple phrase variants are generated at startup and rotated
round-robin across turns so the acknowledgement does not feel repetitive.

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
import re
import subprocess
import threading
import wave
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Optional

logger = logging.getLogger(__name__)

DEFAULT_ACK_PHRASE = "Okay"
DEFAULT_ACK_PHRASES = (
    "Okay",
    "Let me think about that",
    "One sec",
    "Got it",
    "Sure",
    "Hmm, let me see",
)
DEFAULT_FIXTURE_PATH = Path(__file__).resolve().parent.parent / "assets" / "sounds" / "acknowledgement.wav"

# Fallback sample rate if device detection fails. 44100 Hz is universally
# supported by virtually all audio output devices, unlike 16000 Hz which
# fails on many ALSA configurations.
ACK_FALLBACK_SAMPLE_RATE = 44100


def _detect_output_sample_rate() -> int:
    """Query the default output device for its native sample rate.

    Falls back to ``ACK_FALLBACK_SAMPLE_RATE`` if sounddevice is unavailable
    or the default device's rate cannot be determined.
    """
    try:
        import sounddevice as sd

        default_output = sd.default.device[1]
        if default_output is None:
            return ACK_FALLBACK_SAMPLE_RATE
        info = sd.query_devices(default_output, "output")
        rate = info.get("default_samplerate") if isinstance(info, dict) else None
        if rate and int(rate) > 0:
            return int(rate)
    except Exception as error:
        logger.debug("[Ack] device sample-rate detection failed: %s", error)
    return ACK_FALLBACK_SAMPLE_RATE


def _cache_dir() -> Path:
    """Return the XDG cache directory for Zarathushtra."""
    xdg_cache = os.getenv("XDG_CACHE_HOME")
    if xdg_cache:
        return Path(xdg_cache) / "zarathushtra"
    return Path.home() / ".cache" / "zarathushtra"


def _slugify(text: str) -> str:
    """Return a filesystem-safe slug for a provider/voice/phrase label."""
    slug = re.sub(r"[^a-z0-9]+", "-", str(text).lower()).strip("-")
    return slug or "ack"


def _cache_path(provider: str, voice: str, phrase: str) -> Path:
    """Return a cache path keyed by provider+voice+phrase.

    The filename is human-readable (``acknowledgement-<provider>-<voice>-<phrase>-<hash>.wav``)
    so assets generated at first startup can be identified by their
    ``tts_provider``/``voice_id`` at a glance.
    """
    provider_slug = _slugify(provider)
    voice_slug = _slugify(voice)
    phrase_slug = _slugify(phrase)
    digest = hashlib.sha256(
        f"{provider}:{voice}:{phrase}".encode()
    ).hexdigest()[:8]
    return _cache_dir() / f"acknowledgement-{provider_slug}-{voice_slug}-{phrase_slug}-{digest}.wav"


@dataclass(frozen=True)
class AcknowledgementConfig:
    """Configuration for immediate acknowledgement playback."""
    enabled: bool = True
    phrase: str = DEFAULT_ACK_PHRASE
    phrases: tuple[str, ...] = ()
    fixture_path: str = ""
    volume: float = 1.0
    provider: str = "edge"
    voice: str = "en-US-AriaNeural"

    def effective_phrases(self) -> tuple[str, ...]:
        """Return the phrase variants to generate, in stable order."""
        if self.phrases:
            return tuple(dict.fromkeys(self.phrases))
        return (self.phrase,)


@dataclass
class AcknowledgementResult:
    """Result of an acknowledgement playback attempt."""
    turn_id: str
    played: bool = False
    suppressed: bool = False
    error: Optional[str] = None
    source: str = ""
    phrase: str = ""


@dataclass
class _VariantClip:
    phrase: str
    audio_bytes: bytes
    sample_rate: int
    source: str


class AcknowledgementPlayer:
    """Plays a pre-generated acknowledgement audio clip without blocking.

    On ``initialize()``, the player generates (or loads from cache) one
    clip per configured phrase variant. Variants are rotated
    round-robin across turns.

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
        self._clips: list[_VariantClip] = []
        self._played_turns: set[str] = set()
        self._playback_thread: Optional[threading.Thread] = None
        self._stop_event = threading.Event()
        self._lock = threading.Lock()
        self._variant_cursor = 0
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

        phrases = self.config.effective_phrases()
        for phrase in phrases:
            clip = self._load_or_generate(phrase)
            if clip is not None:
                self._clips.append(clip)

        if not self._clips:
            fixture = (
                Path(self.config.fixture_path)
                if self.config.fixture_path
                else DEFAULT_FIXTURE_PATH
            )
            clip = self._load_fixture(fixture)
            if clip is not None:
                self._clips.append(clip)

        if self._clips:
            self._source = self._clips[0].source
            logger.info(
                "[Ack] initialized %d variant(s): %s",
                len(self._clips),
                [clip.phrase for clip in self._clips],
            )
        else:
            logger.warning("[Ack] no clips loaded; acknowledgement will be silent")

    def _load_or_generate(self, phrase: str) -> Optional[_VariantClip]:
        cache = _cache_path(
            self.config.provider, self.config.voice, phrase
        )
        if cache.exists():
            clip = self._load_wav(cache, phrase, source="cache")
            if clip is not None:
                return clip

        clip = self._generate_via_tts(phrase, cache)
        if clip is not None:
            return clip

        fixture = (
            Path(self.config.fixture_path)
            if self.config.fixture_path
            else DEFAULT_FIXTURE_PATH
        )
        return self._load_wav(fixture, phrase, source="fixture")

    def _load_wav(
        self, path: Path, phrase: str, source: str
    ) -> Optional[_VariantClip]:
        try:
            with wave.open(str(path), "rb") as wf:
                audio_bytes = wf.readframes(wf.getnframes())
                sample_rate = wf.getframerate()
            logger.info(
                "[Ack] loaded %r from %s: %s (%d bytes, %d Hz)",
                phrase, source, path, len(audio_bytes), sample_rate,
            )
            return _VariantClip(
                phrase=phrase,
                audio_bytes=audio_bytes,
                sample_rate=sample_rate,
                source=source,
            )
        except Exception as error:
            logger.warning("[Ack] failed to load %s %s: %s", source, path, error)
            return None

    def _load_fixture(self, path: Path) -> Optional[_VariantClip]:
        return self._load_wav(path, self.config.phrase, source="fixture")

    def _generate_via_tts(self, phrase: str, cache_path: Path) -> Optional[_VariantClip]:
        """Generate acknowledgement audio via the configured TTS provider.

        Writes the result to ``cache_path`` so subsequent restarts load
        from the cache without invoking the provider again.
        """
        sample_rate = _detect_output_sample_rate()
        if self._tts_engine is None:
            return self._generate_edge_tts(phrase, cache_path, sample_rate)

        try:
            loop = asyncio.new_event_loop()
            synthesis = loop.run_until_complete(
                self._tts_engine.synthesize_async(phrase)
            )
            loop.close()
            if not synthesis.success or not synthesis.audio:
                logger.warning("[Ack] TTS synthesis failed for %r: %s", phrase, synthesis.error)
                return None

            pcm_bytes, decoded_rate = self._normalise_to_pcm(
                synthesis.audio, synthesis.audio_format, sample_rate
            )
            if not pcm_bytes:
                logger.warning(
                    "[Ack] could not decode %s audio for %r",
                    synthesis.audio_format, phrase,
                )
                return None

            self._write_wav(cache_path, pcm_bytes, decoded_rate)
            logger.info(
                "[Ack] generated %r via %s and cached at %s",
                phrase, synthesis.provider, cache_path,
            )
            return _VariantClip(
                phrase=phrase,
                audio_bytes=pcm_bytes,
                sample_rate=decoded_rate,
                source=f"tts:{synthesis.provider}",
            )
        except Exception as error:
            logger.warning("[Ack] TTS generation error for %r: %s", phrase, error)
            return None

    def _generate_edge_tts(
        self, phrase: str, cache_path: Path, sample_rate: Optional[int] = None
    ) -> Optional[_VariantClip]:
        """Generate acknowledgement via edge-tts as a lightweight default."""
        if sample_rate is None:
            sample_rate = _detect_output_sample_rate()
        try:
            import edge_tts

            cache_path.parent.mkdir(parents=True, exist_ok=True)
            tmp_mp3 = cache_path.with_suffix(".tmp.mp3")

            loop = asyncio.new_event_loop()
            comm = edge_tts.Communicate(
                phrase, voice=self.config.voice
            )
            loop.run_until_complete(comm.save(str(tmp_mp3)))
            loop.close()

            if not tmp_mp3.exists() or tmp_mp3.stat().st_size == 0:
                logger.warning("[Ack] edge-tts produced no audio for %r", phrase)
                return None

            pcm_bytes, decoded_rate = self._decode_mp3_to_pcm(
                tmp_mp3.read_bytes(), sample_rate
            )
            tmp_mp3.unlink(missing_ok=True)

            if not pcm_bytes:
                logger.warning("[Ack] mpv conversion failed for %r", phrase)
                return None

            self._write_wav(cache_path, pcm_bytes, decoded_rate)
            logger.info("[Ack] generated %r via edge-tts and cached at %s", phrase, cache_path)
            return _VariantClip(
                phrase=phrase,
                audio_bytes=pcm_bytes,
                sample_rate=decoded_rate,
                source="edge-tts",
            )
        except Exception as error:
            logger.warning("[Ack] edge-tts generation error for %r: %s", phrase, error)
            return None

    @staticmethod
    def _normalise_to_pcm(
        audio_bytes: bytes,
        audio_format: Optional[str],
        target_sample_rate: Optional[int] = None,
    ) -> tuple[bytes, int]:
        """Return 16-bit mono PCM bytes and sample rate for playback."""
        if audio_format == "wav":
            try:
                import io
                buf = io.BytesIO(audio_bytes)
                with wave.open(buf, "rb") as wf:
                    wav_rate = wf.getframerate()
                    pcm = wf.readframes(wf.getnframes())
                    if target_sample_rate and wav_rate != target_sample_rate:
                        return AcknowledgementPlayer._resample_pcm(
                            pcm, wav_rate, target_sample_rate
                        )
                    return pcm, wav_rate
            except Exception:
                pass
        if audio_format == "mp3":
            return AcknowledgementPlayer._decode_mp3_to_pcm(
                audio_bytes, target_sample_rate
            )
        if audio_format is None:
            logger.warning("[Ack] unknown audio format; treating bytes as raw PCM")
            return audio_bytes, target_sample_rate or 16000
        return b"", target_sample_rate or 16000

    @staticmethod
    def _decode_mp3_to_pcm(
        mp3_bytes: bytes, target_sample_rate: Optional[int] = None
    ) -> tuple[bytes, int]:
        """Decode mp3 bytes to 16-bit mono PCM.

        Uses ``soundfile`` (libsndfile ≥1.1) first — pure Python, no
        subprocess. Falls back to legacy ``mpv`` subprocess if
        ``soundfile`` cannot decode the input.
        """
        rate = target_sample_rate or 44100
        pcm, decoded_rate = AcknowledgementPlayer._decode_mp3_soundfile(
            mp3_bytes, target_sample_rate
        )
        if pcm:
            return pcm, decoded_rate
        return AcknowledgementPlayer._decode_mp3_mpv(mp3_bytes, rate)

    @staticmethod
    def _decode_mp3_soundfile(
        mp3_bytes: bytes, target_sample_rate: Optional[int] = None
    ) -> tuple[bytes, int]:
        """Decode mp3 via soundfile (libsndfile). Returns (pcm, rate)."""
        try:
            import io
            import numpy as np
            import soundfile as sf

            data, src_rate = sf.read(io.BytesIO(mp3_bytes), dtype="int16")
            if data.ndim > 1:
                data = data[:, 0]
            if target_sample_rate and src_rate != target_sample_rate:
                data, src_rate = AcknowledgementPlayer._resample_array(
                    data, src_rate, target_sample_rate
                )
            return data.tobytes(), src_rate
        except Exception as error:
            logger.debug("[Ack] soundfile mp3 decode failed: %s", error)
            return b"", 0

    @staticmethod
    def _resample_pcm(
        pcm_bytes: bytes, src_rate: int, dst_rate: int
    ) -> tuple[bytes, int]:
        """Resample 16-bit mono PCM via soundfile, mpv legacy fallback."""
        pcm, rate = AcknowledgementPlayer._resample_soundfile(
            pcm_bytes, src_rate, dst_rate
        )
        if pcm:
            return pcm, rate
        return AcknowledgementPlayer._resample_mpv(pcm_bytes, src_rate, dst_rate)

    @staticmethod
    def _resample_soundfile(
        pcm_bytes: bytes, src_rate: int, dst_rate: int
    ) -> tuple[bytes, int]:
        """Resample 16-bit mono PCM via soundfile."""
        try:
            import numpy as np
            import soundfile as sf
            import io

            data = np.frombuffer(pcm_bytes, dtype=np.int16).astype(np.float32)
            data, dst = AcknowledgementPlayer._resample_array(data, src_rate, dst_rate)
            return data.astype(np.int16).tobytes(), dst
        except Exception as error:
            logger.debug("[Ack] soundfile resample failed: %s", error)
            return b"", 0

    @staticmethod
    def _resample_array(
        data: "np.ndarray", src_rate: int, dst_rate: int
    ) -> "tuple[np.ndarray, int]":
        """Resample a 1-D float32 numpy array via simple linear interpolation."""
        import numpy as np

        if src_rate == dst_rate or len(data) == 0:
            return data, dst_rate
        ratio = dst_rate / src_rate
        n_out = max(1, int(len(data) * ratio))
        indices = np.linspace(0, len(data) - 1, n_out)
        resampled = np.interp(indices, np.arange(len(data)), data).astype(np.float32)
        return resampled, dst_rate

    @staticmethod
    def _decode_mp3_mpv(
        mp3_bytes: bytes, target_sample_rate: int
    ) -> tuple[bytes, int]:
        """Legacy mp3 decode via mpv subprocess (fallback only)."""
        try:
            proc = subprocess.run(
                [
                    "mpv", "--no-video", "--no-terminal", "--really-quiet",
                    "--ao=pcm", "--ao-pcm-waveheader=no",
                    "--ao-pcm-file=/dev/stdout",
                    "--audio-channels=mono",
                    "--audio-format=s16",
                    f"--audio-samplerate={target_sample_rate}",
                    "-",
                ],
                input=mp3_bytes,
                capture_output=True,
                timeout=10,
            )
            if proc.returncode == 0 and proc.stdout:
                return proc.stdout, target_sample_rate
        except Exception as error:
            logger.warning("[Ack] mpv mp3 decode failed: %s", error)
        return b"", target_sample_rate

    @staticmethod
    def _resample_mpv(
        pcm_bytes: bytes, src_rate: int, dst_rate: int
    ) -> tuple[bytes, int]:
        """Legacy resample via mpv subprocess (fallback only)."""
        try:
            proc = subprocess.run(
                [
                    "mpv", "--no-video", "--no-terminal", "--really-quiet",
                    "--ao=pcm", "--ao-pcm-waveheader=no",
                    "--ao-pcm-file=/dev/stdout",
                    "--audio-channels=mono",
                    "--audio-format=s16",
                    f"--audio-samplerate={dst_rate}",
                    "--demuxer=rawaudio",
                    f"--demuxer-rawaudio-format=s16le",
                    f"--demuxer-rawaudio-rate={src_rate}",
                    "-",
                ],
                input=pcm_bytes,
                capture_output=True,
                timeout=10,
            )
            if proc.returncode == 0 and proc.stdout:
                return proc.stdout, dst_rate
        except Exception as error:
            logger.debug("[Ack] mpv resample failed: %s", error)
        return pcm_bytes, src_rate

    @staticmethod
    def _write_wav(path: Path, pcm_bytes: bytes, sample_rate: int = 16000) -> None:
        path.parent.mkdir(parents=True, exist_ok=True)
        with wave.open(str(path), "wb") as wf:
            wf.setnchannels(1)
            wf.setsampwidth(2)
            wf.setframerate(sample_rate)
            wf.writeframes(pcm_bytes)

    @property
    def is_playing(self) -> bool:
        return self._is_playing

    @property
    def has_audio(self) -> bool:
        return bool(self._clips)

    @property
    def source(self) -> str:
        return self._source

    def _next_clip(self) -> Optional[_VariantClip]:
        if not self._clips:
            return None
        with self._lock:
            clip = self._clips[self._variant_cursor % len(self._clips)]
            self._variant_cursor += 1
        return clip

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

        clip = self._next_clip()
        if clip is None:
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
            args=(clip,),
            name=f"ack-{turn_id}",
            daemon=True,
        )
        self._playback_thread.start()
        return AcknowledgementResult(
            turn_id=turn_id,
            played=True,
            source=clip.source,
            phrase=clip.phrase,
        )

    def _play_thread(self, clip: _VariantClip) -> None:
        try:
            self._is_playing = True
            import sounddevice as sd
            import numpy as np

            samples = np.frombuffer(clip.audio_bytes, dtype=np.int16)
            if self.config.volume != 1.0:
                samples = (
                    samples.astype(np.float32) * self.config.volume
                ).clip(-32768, 32767).astype(np.int16)
            sd.play(samples, clip.sample_rate)
            while sd.get_stream().active:
                if self._stop_event.is_set():
                    sd.stop()
                    break
                import time
                time.sleep(0.01)
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
            self._variant_cursor = 0
        self.stop()
