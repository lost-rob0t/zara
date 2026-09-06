import json
import re
import wave
from pathlib import Path

import pytest


FIXTURE_ROOT = Path(__file__).parent / "fixtures" / "voice"
CASES_DIR = FIXTURE_ROOT / "cases"
RECORDINGS_DIR = FIXTURE_ROOT / "recordings"
SCHEMA = "zara.voice-fixture.v1"
ID_RE = re.compile(r"^[a-z0-9][a-z0-9._-]*$")


def _case_paths():
    return sorted(CASES_DIR.glob("*.json"))


def _load_case(path: Path) -> dict:
    with path.open(encoding="utf-8") as handle:
        value = json.load(handle)
    assert isinstance(value, dict), f"{path}: top-level JSON must be an object"
    return value


def _cases():
    return [(path, _load_case(path)) for path in _case_paths()]


def test_voice_fixture_cases_are_well_formed_and_unique():
    seen_ids = set()
    seen_audio = set()

    for path, case in _cases():
        assert case.get("schema") == SCHEMA, f"{path}: unsupported schema"

        fixture_id = case.get("id")
        assert isinstance(fixture_id, str) and ID_RE.fullmatch(fixture_id), (
            f"{path}: id must be lowercase and filesystem-safe"
        )
        assert fixture_id not in seen_ids, f"duplicate fixture id: {fixture_id}"
        seen_ids.add(fixture_id)

        phrase = case.get("phrase")
        assert isinstance(phrase, str) and phrase.strip(), f"{path}: phrase is required"

        expected = case.get("expected_transcript")
        assert isinstance(expected, str) and expected.strip(), (
            f"{path}: expected_transcript is required"
        )

        audio = case.get("audio")
        assert isinstance(audio, str) and Path(audio).name == audio, (
            f"{path}: audio must be a basename, not a path"
        )
        assert audio.endswith(".wav"), f"{path}: audio must use .wav"
        assert audio not in seen_audio, f"duplicate audio filename: {audio}"
        seen_audio.add(audio)

        seconds = case.get("seconds")
        assert isinstance(seconds, int) and 1 <= seconds <= 60, (
            f"{path}: seconds must be an integer from 1 to 60"
        )
        assert case.get("sample_rate_hz") == 16000, (
            f"{path}: human regression fixtures must be 16 kHz"
        )
        assert case.get("channels") == 1, (
            f"{path}: human regression fixtures must be mono"
        )

        tags = case.get("tags")
        assert isinstance(tags, list) and tags, f"{path}: at least one tag is required"
        assert all(isinstance(tag, str) and tag.strip() for tag in tags), (
            f"{path}: tags must be non-empty strings"
        )


def test_every_declared_voice_fixture_has_a_real_recording():
    missing = []
    for path, case in _cases():
        recording = RECORDINGS_DIR / case["audio"]
        if not recording.is_file():
            missing.append(f"{case['id']} -> {recording.relative_to(FIXTURE_ROOT.parent.parent)}")

    assert not missing, (
        "Declared human voice fixtures are missing recordings. Open voice-fixtures.org "
        "in Emacs, enter the fixtures/voice-recordings worktree, and record them:\n  "
        + "\n  ".join(missing)
    )


@pytest.mark.parametrize("path,case", _cases(), ids=lambda value: str(value)[:80])
def test_voice_fixture_recording_format(path, case):
    recording = RECORDINGS_DIR / case["audio"]
    if not recording.is_file():
        pytest.fail(
            f"{path}: missing {recording}; record it through voice-fixtures.org"
        )

    with wave.open(str(recording), "rb") as wav:
        assert wav.getnchannels() == case["channels"]
        assert wav.getframerate() == case["sample_rate_hz"]
        assert wav.getsampwidth() == 2, "fixtures must be signed 16-bit PCM WAV"
        frame_count = wav.getnframes()
        assert frame_count >= case["sample_rate_hz"] // 4, (
            f"{recording}: recording is too short to be a useful human fixture"
        )
        duration = frame_count / wav.getframerate()
        assert duration <= case["seconds"] + 1.0, (
            f"{recording}: duration {duration:.2f}s exceeds declared capture window"
        )
