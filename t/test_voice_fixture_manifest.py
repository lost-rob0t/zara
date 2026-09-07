import json
import math
import re
import struct
import wave
from pathlib import Path

import pytest


FIXTURE_ROOT = Path(__file__).parent / "fixtures" / "voice"
CASES_DIR = FIXTURE_ROOT / "cases"
RECORDINGS_DIR = FIXTURE_ROOT / "recordings"
SCHEMA = "zara.voice-fixture.v1"
ID_RE = re.compile(r"^[a-z0-9][a-z0-9._-]*$")
REQUIRED_KEYS = {
    "schema",
    "id",
    "phrase",
    "audio",
    "seconds",
    "sample_rate_hz",
    "channels",
    "expected_transcript",
    "tags",
}


def _case_paths():
    paths = sorted(CASES_DIR.glob("*.json"))
    assert paths, f"{CASES_DIR}: voice fixture corpus must declare at least one case"
    return paths


def _load_case(path: Path) -> dict:
    with path.open(encoding="utf-8") as handle:
        value = json.load(handle)
    assert isinstance(value, dict), f"{path}: top-level JSON must be an object"
    return value


def _cases():
    return [(path, _load_case(path)) for path in _case_paths()]


VOICE_CASES = _cases()
VOICE_CASE_IDS = [
    case.get("id") if isinstance(case.get("id"), str) else path.stem
    for path, case in VOICE_CASES
]


def test_voice_fixture_cases_are_well_formed_and_unique():
    seen_ids = set()
    seen_audio = set()

    for path, case in VOICE_CASES:
        assert set(case) == REQUIRED_KEYS, (
            f"{path}: keys must exactly match schema {SCHEMA}; "
            f"missing={sorted(REQUIRED_KEYS - set(case))}, "
            f"unknown={sorted(set(case) - REQUIRED_KEYS)}"
        )
        assert case["schema"] == SCHEMA, f"{path}: unsupported schema"

        fixture_id = case["id"]
        assert isinstance(fixture_id, str) and ID_RE.fullmatch(fixture_id), (
            f"{path}: id must be lowercase and filesystem-safe"
        )
        assert path.stem == fixture_id, (
            f"{path}: filename must match fixture id {fixture_id!r}"
        )
        assert fixture_id not in seen_ids, f"duplicate fixture id: {fixture_id}"
        seen_ids.add(fixture_id)

        phrase = case["phrase"]
        assert isinstance(phrase, str) and phrase.strip(), f"{path}: phrase is required"

        expected = case["expected_transcript"]
        assert isinstance(expected, str) and expected.strip(), (
            f"{path}: expected_transcript is required"
        )

        audio = case["audio"]
        assert isinstance(audio, str) and Path(audio).name == audio, (
            f"{path}: audio must be a basename, not a path"
        )
        assert audio == f"{fixture_id}.wav", (
            f"{path}: audio must be the canonical filename {fixture_id}.wav"
        )
        assert audio not in seen_audio, f"duplicate audio filename: {audio}"
        seen_audio.add(audio)

        seconds = case["seconds"]
        assert type(seconds) is int and 1 <= seconds <= 60, (
            f"{path}: seconds must be an integer from 1 to 60"
        )
        assert case["sample_rate_hz"] == 16000, (
            f"{path}: human regression fixtures must be 16 kHz"
        )
        assert case["channels"] == 1, (
            f"{path}: human regression fixtures must be mono"
        )

        tags = case["tags"]
        assert isinstance(tags, list) and tags, f"{path}: at least one tag is required"
        assert all(isinstance(tag, str) and tag.strip() for tag in tags), (
            f"{path}: tags must be non-empty strings"
        )
        assert len(tags) == len(set(tags)), f"{path}: tags must be unique"


def test_voice_recording_directory_has_no_orphans():
    declared = {case["audio"] for _, case in VOICE_CASES}
    if not RECORDINGS_DIR.exists():
        return

    actual = {path.name for path in RECORDINGS_DIR.iterdir() if path.is_file()}
    orphaned = sorted(actual - declared)
    assert not orphaned, (
        f"{RECORDINGS_DIR}: undeclared recording files found: {orphaned}; "
        "declare them in cases/*.json or remove them"
    )


def test_every_declared_voice_fixture_has_a_real_recording():
    missing = []
    for path, case in VOICE_CASES:
        recording = RECORDINGS_DIR / case["audio"]
        if not recording.is_file():
            missing.append(f"{case['id']} -> {recording.relative_to(FIXTURE_ROOT.parent.parent)}")

    assert not missing, (
        "Declared human voice fixtures are missing recordings. Open voice-fixtures.org "
        "in Emacs, enter the fixtures/voice-recordings worktree, and record them:\n  "
        + "\n  ".join(missing)
    )


@pytest.mark.parametrize("path,case", VOICE_CASES, ids=VOICE_CASE_IDS)
def test_voice_fixture_recording_format(path, case):
    recording = RECORDINGS_DIR / case["audio"]
    if not recording.is_file():
        pytest.fail(
            f"{path}: missing {recording}; record it through voice-fixtures.org"
        )

    with wave.open(str(recording), "rb") as wav:
        assert wav.getcomptype() == "NONE", "fixtures must use uncompressed PCM WAV"
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

        frames = wav.readframes(frame_count)
        samples = [sample[0] for sample in struct.iter_unpack("<h", frames)]
        assert samples, f"{recording}: recording contains no PCM samples"
        scale = 32768.0
        peak = max(abs(sample) for sample in samples) / scale
        rms = math.sqrt(
            sum((sample / scale) ** 2 for sample in samples) / len(samples)
        )
        assert peak >= 0.003, (
            f"{recording}: peak {peak:.6f} is effectively silence; record real speech"
        )
        assert rms >= 0.0005, (
            f"{recording}: RMS {rms:.6f} is effectively silence; record real speech"
        )
        clipped = sum(abs(sample) >= 32760 for sample in samples) / len(samples)
        assert clipped < 0.02, (
            f"{recording}: {clipped:.1%} clipped samples; re-record at a lower level"
        )
