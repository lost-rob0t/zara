#!/usr/bin/env python3

import os
import shutil
import subprocess
import threading
import time

from zara import audio


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


def main() -> None:
    sample_rate, note = audio.resolve_input_sample_rate(16000.0)
    assert sample_rate == 16000.0, sample_rate
    assert note is not None and "Pulse shared capture via parec" in note, note
    assert audio.sd.InputStream is audio.PulseInputStream

    source = pactl("get-default-source").strip()
    discord = start_discord_fixture(source)
    try:
        assert wait_for_stream("DiscordFixture"), (
            "Discord fixture never appeared as a Pulse recording stream"
        )
        assert discord.poll() is None, "Discord fixture exited before Zara started"

        received_audio = threading.Event()

        def callback(indata, frames, _time_info, status):
            assert status is None
            assert frames > 0
            assert indata.ndim == 2
            assert indata.shape[1] == 1
            received_audio.set()

        zara_visible = False
        discord_visible = False
        with audio.sd.InputStream(
            samplerate=sample_rate,
            channels=1,
            callback=callback,
        ):
            deadline = time.monotonic() + 5.0
            while time.monotonic() < deadline:
                source_outputs = pactl("list", "source-outputs")
                zara_visible = "Zarathushtra" in source_outputs
                discord_visible = "DiscordFixture" in source_outputs
                if zara_visible and discord_visible and received_audio.is_set():
                    break
                time.sleep(0.1)

        assert zara_visible, "Zarathushtra never appeared as a Pulse recording stream"
        assert discord_visible, "Discord fixture disappeared when Zara opened the source"
        assert received_audio.is_set(), "shared Pulse capture produced no audio frames"
    finally:
        stop_process(discord)

    print(note)
    print("PASS: DiscordFixture + Zarathushtra share one source and Zara receives frames")


if __name__ == "__main__":
    main()
