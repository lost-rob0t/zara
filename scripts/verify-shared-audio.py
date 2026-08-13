#!/usr/bin/env python3

import argparse
import math
import os
import shutil
import subprocess
import threading
import time

import numpy as np

from zara import audio


SIGNAL_THRESHOLD = 1e-5


def pactl(*args: str) -> str:
    return subprocess.check_output(["pactl", *args], text=True)


def wait_for_stream(name: str, timeout: float = 5.0) -> bool:
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if name in pactl("list", "source-outputs"):
            return True
        time.sleep(0.1)
    return False


def start_discord_fixture(source: str) -> subprocess.Popen:
    parec = shutil.which("parec")
    assert parec is not None
    env = os.environ.copy()
    env["PULSE_PROP_application.name"] = "DiscordFixture"
    return subprocess.Popen(
        [
            parec,
            "--client-name=DiscordFixture",
            "--stream-name=DiscordFixture microphone",
            "--format=s16le",
            "--rate=48000",
            "--channels=1",
            f"--device={source}",
        ],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.PIPE,
        env=env,
    )


def stop_process(process: subprocess.Popen) -> None:
    if process.poll() is not None:
        return
    process.terminate()
    try:
        process.wait(timeout=1.0)
    except subprocess.TimeoutExpired:
        process.kill()
        process.wait(timeout=1.0)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Verify Zara Pulse shared capture with real PCM measurements."
    )
    parser.add_argument("--seconds", type=float, default=5.0)
    parser.add_argument(
        "--require-signal",
        action="store_true",
        help="fail if captured PCM never rises above the non-silent threshold",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    duration = max(0.25, args.seconds)
    sample_rate, note = audio.resolve_input_sample_rate(16000.0)
    assert sample_rate == 16000.0, sample_rate
    assert note is not None and "Pulse shared capture via parec" in note, note
    assert audio.sd.InputStream is audio.PulseInputStream

    source = pactl("get-default-source").strip()
    discord = start_discord_fixture(source)
    lock = threading.Lock()
    samples_received = 0
    sum_squares = 0.0
    peak = 0.0

    try:
        assert wait_for_stream("DiscordFixture"), (
            "Discord fixture never appeared as a Pulse recording stream"
        )
        assert discord.poll() is None, "Discord fixture exited before Zara started"

        def callback(indata, frames, _time_info, status):
            nonlocal samples_received, sum_squares, peak
            assert status is None
            assert frames > 0
            assert indata.ndim == 2
            assert indata.shape[1] == 1
            mono = indata[:, 0].astype(np.float64, copy=False)
            with lock:
                samples_received += len(mono)
                sum_squares += float(np.dot(mono, mono))
                if len(mono):
                    peak = max(peak, float(np.max(np.abs(mono))))

        zara_visible = False
        discord_visible = False
        started = time.monotonic()
        stream = audio.sd.InputStream(
            samplerate=sample_rate,
            channels=1,
            callback=callback,
        )
        with stream:
            deadline = started + duration
            while time.monotonic() < deadline:
                source_outputs = pactl("list", "source-outputs")
                zara_visible = "Zarathushtra" in source_outputs
                discord_visible = "DiscordFixture" in source_outputs
                time.sleep(0.1)

        elapsed = max(time.monotonic() - started, 1e-9)
        with lock:
            count = samples_received
            measured_peak = peak
            measured_rms = math.sqrt(sum_squares / count) if count else 0.0

        assert zara_visible, "Zarathushtra never appeared as a Pulse recording stream"
        assert discord_visible, "Discord fixture disappeared when Zara opened the source"
        assert count > 0, "shared Pulse capture produced no PCM samples"
        assert stream.last_error is None, stream.last_error
        if args.require_signal:
            assert measured_peak > SIGNAL_THRESHOLD, (
                "Pulse capture received PCM but it was silent: "
                f"peak={measured_peak:.6f}, rms={measured_rms:.6f}"
            )
    finally:
        stop_process(discord)

    print(note)
    print(f"source={source}")
    print(f"sample_rate={int(sample_rate)}")
    print("channels=1")
    print(f"duration={elapsed:.3f}s")
    print(f"samples={count}")
    print(f"bytes={count * 2}")
    print(f"peak={measured_peak:.6f}")
    print(f"rms={measured_rms:.6f}")
    print(f"non_silent={measured_peak > SIGNAL_THRESHOLD}")
    print("PASS: concurrent Pulse capture is alive and PCM reached Zara")


if __name__ == "__main__":
    main()
