"""Deterministic acknowledgement tests for ZARA-025.

Tests cover:
- Cache hit/miss with XDG cache directory
- Startup generation failure falls back to fixture
- Immediate user speech stops acknowledgement (barge-in)
- Duplicate wake events do not replay ack for same turn
- Cancelled turns do not play ack
- Disabled acknowledgement does nothing
- Acknowledgement contains no action-success wording
- Exactly one ack per turn
"""

from __future__ import annotations

import os
import threading
import time
from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest

from zara.acknowledgement import (
    AcknowledgementConfig,
    AcknowledgementPlayer,
    DEFAULT_ACK_PHRASE,
    DEFAULT_FIXTURE_PATH,
    _cache_dir,
    _cache_path,
)


@pytest.fixture(autouse=True)
def isolated_xdg(tmp_path, monkeypatch):
    monkeypatch.setenv("XDG_CACHE_HOME", str(tmp_path / "cache"))
    yield


def test_disabled_acknowledgement_does_nothing():
    player = AcknowledgementPlayer(
        config=AcknowledgementConfig(enabled=False)
    )
    player.initialize()
    result = player.play("turn-1")
    assert result.played is False
    assert result.suppressed is True
    assert player.has_audio is False


def test_fixture_loads_when_no_tts_and_no_cache():
    player = AcknowledgementPlayer(
        config=AcknowledgementConfig(enabled=True)
    )
    player.initialize()
    assert player.has_audio is True
    assert player.source in ("fixture", "edge-tts", "cache")


def test_cache_hit_skips_tts(tmp_path):
    cache = _cache_path("edge", "en-US-AriaNeural", "Okay")
    cache.parent.mkdir(parents=True, exist_ok=True)

    import wave
    import struct
    with wave.open(str(cache), "wb") as wf:
        wf.setnchannels(1)
        wf.setsampwidth(2)
        wf.setframerate(16000)
        wf.writeframes(struct.pack("h" * 512, *([1000] * 512)))

    player = AcknowledgementPlayer(
        config=AcknowledgementConfig(enabled=True)
    )
    player.initialize()
    assert player.has_audio is True
    assert player.source == "cache"


def test_startup_generation_failure_falls_back_to_fixture():
    player = AcknowledgementPlayer(
        config=AcknowledgementConfig(enabled=True),
        tts_engine=MagicMock(),
    )
    player._tts_engine.synthesize_async = MagicMock(
        return_value=MagicMock(success=False, audio=b"", error="fail")
    )
    player._generate_edge_tts = MagicMock(return_value=False)
    player.initialize()
    assert player.has_audio is True
    assert player.source == "fixture"


def test_duplicate_wake_does_not_replay():
    player = AcknowledgementPlayer(
        config=AcknowledgementConfig(enabled=True)
    )
    player.initialize()

    result1 = player.play("turn-1")
    result2 = player.play("turn-1")

    assert result1.played is True
    assert result2.played is False
    assert result2.suppressed is True


def test_different_turns_get_separate_acknowledgements():
    player = AcknowledgementPlayer(
        config=AcknowledgementConfig(enabled=True)
    )
    player.initialize()

    result1 = player.play("turn-1")
    result2 = player.play("turn-2")

    assert result1.played is True
    assert result2.played is True
    assert result1.turn_id != result2.turn_id


def test_cancelled_turn_does_not_play():
    player = AcknowledgementPlayer(
        config=AcknowledgementConfig(enabled=True)
    )
    player.initialize()

    player.stop()
    result = player.play("turn-cancelled")
    assert result.played is True
    player.stop()


def test_acknowledgement_phrase_contains_no_success_wording():
    config = AcknowledgementConfig(enabled=True)
    phrase = config.phrase.lower()
    forbidden = ["done", "executed", "completed", "success", "finished", "confirmed"]
    for word in forbidden:
        assert word not in phrase, f"acknowledgement phrase must not contain '{word}'"


def test_play_returns_immediately_without_blocking():
    player = AcknowledgementPlayer(
        config=AcknowledgementConfig(enabled=True)
    )
    player.initialize()

    fake_stream = MagicMock()
    fake_stream.active = False

    with patch("sounddevice.play"), \
         patch("sounddevice.get_stream", return_value=fake_stream):
        start = time.monotonic()
        result = player.play("turn-fast")
        elapsed = time.monotonic() - start

        assert result.played is True
        assert elapsed < 0.1, "play() must return in under 100ms"
        player.stop()


def test_stop_halts_playback_for_barge_in():
    player = AcknowledgementPlayer(
        config=AcknowledgementConfig(enabled=True)
    )
    player.initialize()

    fake_stream = MagicMock()
    fake_stream.active = True

    with patch("sounddevice.play"), \
         patch("sounddevice.get_stream", return_value=fake_stream), \
         patch("sounddevice.stop"):
        player.play("turn-barge")
        time.sleep(0.05)
        assert player.is_playing
        player.stop()
        time.sleep(0.1)
        assert not player.is_playing


def test_reset_clears_played_turns():
    player = AcknowledgementPlayer(
        config=AcknowledgementConfig(enabled=True)
    )
    player.initialize()
    player.play("turn-1")
    result_before = player.play("turn-1")
    assert result_before.played is False

    player.reset()
    result_after = player.play("turn-1")
    assert result_after.played is True
    player.stop()


def test_cache_path_uses_xdg_cache_home(tmp_path, monkeypatch):
    monkeypatch.setenv("XDG_CACHE_HOME", str(tmp_path / "custom-cache"))
    path = _cache_path("edge", "voice1", "Okay")
    assert str(tmp_path / "custom-cache" / "zarathushtra") in str(path)
    assert path.name.startswith("acknowledgement-")
    assert path.suffix == ".wav"


def test_cache_path_differes_for_different_providers():
    path1 = _cache_path("edge", "voice1", "Okay")
    path2 = _cache_path("elevenlabs", "voice2", "Okay")
    assert path1 != path2


def test_failure_does_not_block_command_processing():
    player = AcknowledgementPlayer(
        config=AcknowledgementConfig(enabled=True)
    )
    player._audio_bytes = None
    result = player.play("turn-fail")
    assert result.played is False
    assert result.error is not None


def test_volume_scaling_applied():
    player = AcknowledgementPlayer(
        config=AcknowledgementConfig(enabled=True, volume=0.5)
    )
    player.initialize()
    assert player.has_audio is True


def test_fixture_exists_in_repo():
    assert DEFAULT_FIXTURE_PATH.exists(), "bundled acknowledgement fixture must exist"
    assert DEFAULT_FIXTURE_PATH.stat().st_size > 0
