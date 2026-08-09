#!/usr/bin/env python3

import subprocess
import threading
import time

from zara import audio


def pactl(*args: str) -> str:
    return subprocess.check_output(["pactl", *args], text=True)


def main() -> None:
    sample_rate, note = audio.resolve_input_sample_rate(16000.0)
    assert sample_rate == 16000.0, sample_rate
    assert note is not None and "Pulse shared capture via parec" in note, note
    assert audio.sd.InputStream is audio.PulseInputStream

    received_audio = threading.Event()

    def callback(indata, frames, _time_info, status):
        assert status is None
        assert frames > 0
        assert indata.ndim == 2
        assert indata.shape[1] == 1
        received_audio.set()

    visible = False
    with audio.sd.InputStream(
        samplerate=sample_rate,
        channels=1,
        callback=callback,
    ):
        deadline = time.monotonic() + 5.0
        while time.monotonic() < deadline:
            source_outputs = pactl("list", "source-outputs")
            visible = "Zarathushtra" in source_outputs
            if visible and received_audio.is_set():
                break
            time.sleep(0.1)

    assert visible, "Zarathushtra never appeared as a Pulse recording stream"
    assert received_audio.is_set(), "shared Pulse capture produced no audio frames"
    print(note)
    print("PASS: Zarathushtra is visible as a Pulse recording stream and receives frames")


if __name__ == "__main__":
    main()
