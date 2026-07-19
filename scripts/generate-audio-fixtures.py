#!/usr/bin/env python3
"""Generate deterministic audio fixtures for ZARA-021 regression gate.

Each fixture is a small WAV file (16 kHz, 16-bit, mono) suitable for
testing the audio pipeline without a live microphone. The fixtures are
generated from fixed seeds and mathematical functions so they are
byte-identical across runs and platforms.

Run with: python scripts/generate-audio-fixtures.py [output_dir]

Default output: t/fixtures/audio/
"""

from __future__ import annotations

import hashlib
import math
import struct
import sys
import wave
from pathlib import Path

import numpy as np

SAMPLE_RATE = 16000
SAMPLE_WIDTH = 2  # 16-bit
CHANNELS = 1

FIXTURE_SPECS = [
    ("silence.wav", 1.0, "silence"),
    ("wake_word.wav", 0.5, "sine_burst"),
    ("command_speech.wav", 2.0, "sine"),
    ("trailing_silence.wav", 1.0, "silence"),
    ("interruption.wav", 0.3, "sine_burst"),
    ("noise.wav", 1.0, "noise"),
    ("long_speech.wav", 10.0, "sine"),
]


def _to_int16(samples: np.ndarray) -> np.ndarray:
    clipped = np.clip(samples, -1.0, 1.0)
    return (clipped * 32767).astype(np.int16)


def _write_wav(path: Path, samples: np.ndarray) -> None:
    data = _to_int16(samples).tobytes()
    with wave.open(str(path), "wb") as wf:
        wf.setnchannels(CHANNELS)
        wf.setsampwidth(SAMPLE_WIDTH)
        wf.setframerate(SAMPLE_RATE)
        wf.writeframes(data)


def _generate(name: str, duration: float, kind: str) -> np.ndarray:
    n = int(SAMPLE_RATE * duration)
    t = np.arange(n) / SAMPLE_RATE

    if kind == "silence":
        return np.zeros(n, dtype=np.float64)

    if kind == "sine":
        freq = 440.0
        envelope = np.ones(n)
        fade = min(int(SAMPLE_RATE * 0.05), n // 4)
        if fade > 0:
            envelope[:fade] = np.linspace(0, 1, fade)
            envelope[-fade:] = np.linspace(1, 0, fade)
        return 0.3 * envelope * np.sin(2 * math.pi * freq * t)

    if kind == "sine_burst":
        freq = 880.0
        envelope = np.zeros(n)
        onset = int(n * 0.2)
        offset = int(n * 0.8)
        envelope[onset:offset] = 1.0
        fade = min(int(SAMPLE_RATE * 0.01), (offset - onset) // 4)
        if fade > 0:
            envelope[onset:onset + fade] = np.linspace(0, 1, fade)
            envelope[offset - fade:offset] = np.linspace(1, 0, fade)
        return 0.3 * envelope * np.sin(2 * math.pi * freq * t)

    if kind == "noise":
        seed = int.from_bytes(hashlib.sha256(name.encode()).digest()[:4], "little")
        rng = np.random.default_rng(seed=seed)
        return 0.1 * rng.standard_normal(n)

    raise ValueError(f"unknown fixture kind: {kind}")


def generate_all(output_dir: Path) -> list[Path]:
    output_dir.mkdir(parents=True, exist_ok=True)
    paths = []
    for name, duration, kind in FIXTURE_SPECS:
        samples = _generate(name, duration, kind)
        path = output_dir / name
        _write_wav(path, samples)
        paths.append(path)
    return paths


def main() -> int:
    output_dir = Path(sys.argv[1]) if len(sys.argv) > 1 else Path("t/fixtures/audio")
    if not output_dir.is_absolute():
        output_dir = Path.cwd() / output_dir
    paths = generate_all(output_dir)
    for path in paths:
        size = path.stat().st_size
        print(f"  {path.name}: {size} bytes")
    print(f"\nGenerated {len(paths)} fixtures in {output_dir}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
