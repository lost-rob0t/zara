#!/usr/bin/env python3
"""Measured STT comparison for short follow-up utterances (#882).

Synthesizes a small phrase set through the local qwen3-TTS OpenAI-compatible
endpoint, converts it to 16 kHz mono PCM, transcribes it with whisper-cli
(Vulkan build) for each model x beam configuration, and reports per-phrase
WER, aggregate WER, RTF, and the peak amdgpu VRAM observed for the whisper
process via /proc fdinfo.

Usage (inside `nix develop`):
  python scripts/stt-phrase-benchmark.py \
      --models base.en small.en --beams 1 3 \
      --out /var/tmp/zara-stt-benchmark
"""

from __future__ import annotations

import argparse
import json
import re
import subprocess
import sys
import time
import urllib.request
from pathlib import Path

DEFAULT_TTS_URL = "http://localhost:7860/v1/audio/speech"
DEFAULT_VOICE = "zara"

PHRASES = [
    "what did I say",
    "what time is it",
    "tell me a joke",
    "set a timer for ten minutes",
    "what's the weather like tomorrow",
    "remind me to call mom",
    "play some music",
    "never mind",
    "who won the game last night",
    "stop",
]


def synthesize_phrase(text: str, tts_url: str, voice: str, wav_path: Path) -> None:
    payload = json.dumps(
        {
            "model": "qwen3-tts",
            "input": text,
            "voice": voice,
            "response_format": "wav",
        }
    ).encode("utf-8")
    request = urllib.request.Request(
        tts_url,
        data=payload,
        headers={"Content-Type": "application/json"},
        method="POST",
    )
    with urllib.request.urlopen(request, timeout=60) as response:
        audio = response.read()
    if not audio.startswith(b"RIFF"):
        raise RuntimeError(f"TTS returned non-WAV audio for {text!r}")
    wav_path.write_bytes(audio)


def to_16k_mono(source: Path, target: Path, ffmpeg: str) -> None:
    subprocess.run(
        [
            ffmpeg,
            "-y",
            "-v",
            "error",
            "-i",
            str(source),
            "-ar",
            "16000",
            "-ac",
            "1",
            "-c:a",
            "pcm_s16le",
            str(target),
        ],
        check=True,
    )


def wav_duration_seconds(path: Path) -> float:
    probe = subprocess.run(
        ["ffprobe", "-v", "error", "-show_entries", "format=duration",
         "-of", "csv=p=0", str(path)],
        check=True,
        capture_output=True,
        text=True,
    )
    return float(probe.stdout.strip())


def normalize_words(text: str) -> list[str]:
    cleaned = re.sub(r"[^a-z0-9' ]+", " ", text.lower())
    return cleaned.split()


def word_error_rate(reference: str, hypothesis: str) -> float:
    ref = normalize_words(reference)
    hyp = normalize_words(hypothesis)
    if not ref:
        return 0.0 if not hyp else 1.0
    previous = list(range(len(hyp) + 1))
    for i, ref_word in enumerate(ref, start=1):
        current = [i]
        for j, hyp_word in enumerate(hyp, start=1):
            current.append(
                min(
                    previous[j] + 1,
                    current[j - 1] + 1,
                    previous[j - 1] + (ref_word != hyp_word),
                )
            )
        previous = current
    return previous[-1] / len(ref)


def _read_vram_kib(pid: int) -> int:
    total = 0
    fd_dir = Path(f"/proc/{pid}/fdinfo")
    try:
        entries = list(fd_dir.iterdir())
    except OSError:
        return 0
    for entry in entries:
        try:
            content = entry.read_text()
        except OSError:
            continue
        for line in content.splitlines():
            if line.startswith("drm-memory-vram:"):
                for token in line.split(":", 1)[1].split():
                    if token.isdigit():
                        total = max(total, int(token))
    return total


def run_whisper(
    whisper_cli: Path,
    model: Path,
    audio: Path,
    beam: int,
    threads: int,
) -> tuple[str, float, int]:
    """Return (text, wall_seconds, peak_vram_kib)."""
    started = time.monotonic()
    process = subprocess.Popen(
        [
            str(whisper_cli),
            "-m",
            str(model),
            "-f",
            str(audio),
            "-l",
            "en",
            "-bs",
            str(beam),
            "-t",
            str(threads),
            "-np",
            "-nt",
        ],
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        text=True,
    )
    peak = 0
    while process.poll() is None:
        peak = max(peak, _read_vram_kib(process.pid))
        time.sleep(0.2)
    stdout, _ = process.communicate()
    elapsed = time.monotonic() - started
    text = " ".join(line.strip() for line in stdout.splitlines() if line.strip())
    return text, elapsed, peak


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--models", nargs="+", default=["base.en", "small.en"])
    parser.add_argument("--beams", nargs="+", type=int, default=[1, 3])
    parser.add_argument("--threads", type=int, default=4)
    parser.add_argument("--voice", default=DEFAULT_VOICE)
    parser.add_argument("--tts-url", default=DEFAULT_TTS_URL)
    parser.add_argument(
        "--model-dir",
        type=Path,
        default=Path("~/.cache/zara/whisper.cpp").expanduser(),
    )
    parser.add_argument("--whisper-cli", type=Path, default=Path("whisper-cli"))
    parser.add_argument("--ffmpeg", default="ffmpeg")
    parser.add_argument("--out", type=Path, default=Path("/var/tmp/zara-stt-benchmark"))
    parser.add_argument("--skip-tts", action="store_true",
                        help="reuse previously synthesized phrase audio")
    args = parser.parse_args()

    phrase_dir = args.out / "phrases"
    phrase_dir.mkdir(parents=True, exist_ok=True)

    audio_paths: dict[str, Path] = {}
    durations: dict[str, float] = {}
    for index, phrase in enumerate(PHRASES):
        target = phrase_dir / f"phrase-{index:02d}.wav"
        if not args.skip_tts or not target.is_file():
            raw = phrase_dir / f"raw-{index:02d}.wav"
            synthesize_phrase(phrase, args.tts_url, args.voice, raw)
            to_16k_mono(raw, target, args.ffmpeg)
            raw.unlink()
        audio_paths[phrase] = target
        durations[phrase] = wav_duration_seconds(target)
        print(f"phrase[{index}] ready: {phrase!r} ({durations[phrase]:.2f}s)")

    results = []
    for model_name in args.models:
        model_path = args.model_dir / f"ggml-{model_name}.bin"
        if not model_path.is_file():
            print(f"model not found: {model_path}", file=sys.stderr)
            return 1
        for beam in args.beams:
            rows = []
            total_duration = 0.0
            total_decode = 0.0
            peak_vram = 0
            for phrase, audio_path in audio_paths.items():
                text, elapsed, vram = run_whisper(
                    args.whisper_cli, model_path, audio_path, beam, args.threads
                )
                wer = word_error_rate(phrase, text)
                rows.append(
                    {
                        "phrase": phrase,
                        "transcript": text,
                        "wer": round(wer, 4),
                        "decode_seconds": round(elapsed, 3),
                    }
                )
                total_duration += durations[phrase]
                total_decode += elapsed
                peak_vram = max(peak_vram, vram)
                print(
                    f"[{model_name} beam={beam}] {phrase!r} -> {text!r} "
                    f"(wer={wer:.2f}, {elapsed:.2f}s)"
                )
            aggregate_wer = (
                sum(row["wer"] for row in rows) / len(rows) if rows else 1.0
            )
            result = {
                "model": model_name,
                "beam": beam,
                "aggregate_wer": round(aggregate_wer, 4),
                "total_audio_seconds": round(total_duration, 2),
                "total_decode_seconds": round(total_decode, 2),
                "rtf": round(total_decode / total_duration, 3)
                if total_duration
                else None,
                "peak_vram_kib": peak_vram if peak_vram else None,
                "rows": rows,
            }
            results.append(result)
            print(
                f"== {model_name} beam={beam}: WER={aggregate_wer:.3f} "
                f"RTF={result['rtf']} VRAM={result['peak_vram_kib']}"
            )

    output = args.out / "results.json"
    output.write_text(json.dumps(results, indent=2))
    print(f"results written: {output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
