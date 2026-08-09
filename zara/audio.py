"""
Audio I/O - capture and playback
"""

import os
import queue
import shutil
import subprocess
import sys
import threading
import time
from typing import Optional, Tuple

import numpy as np
import sounddevice as sd
from threading import Thread


_PULSE_APPLICATION_NAME = "Zarathushtra"
_PULSE_ICON_NAME = "audio-input-microphone"
_PULSE_MEDIA_ROLE = "phone"
_ORIGINAL_INPUT_STREAM = sd.InputStream
_SHARED_STREAM_INSTALLED = False


def _prefer_shared_input() -> bool:
    value = os.getenv("ZARA_PREFER_SHARED_INPUT", "1").strip().lower()
    return value not in {"0", "false", "no", "off"}


def _run_pactl(*args: str) -> Optional[str]:
    pactl = shutil.which("pactl")
    if pactl is None:
        return None

    try:
        result = subprocess.run(
            [pactl, *args],
            check=False,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            timeout=1.5,
        )
    except (OSError, subprocess.SubprocessError):
        return None

    if result.returncode != 0:
        return None
    return result.stdout.strip()


def _pulse_backend_status() -> Tuple[bool, str]:
    if not sys.platform.startswith("linux"):
        return False, "non-Linux platform"
    if not _prefer_shared_input():
        return False, "disabled by ZARA_PREFER_SHARED_INPUT"
    if shutil.which("parec") is None:
        return False, "parec is not available"
    if shutil.which("pactl") is None:
        return False, "pactl is not available"
    if _run_pactl("info") is None:
        return False, "PulseAudio/PipeWire-Pulse server is not reachable"
    return True, "PulseAudio/PipeWire-Pulse server reachable"


def _pulse_default_source() -> Optional[str]:
    source = os.getenv("ZARA_PULSE_SOURCE", "").strip()
    if source:
        return source

    source = _run_pactl("get-default-source")
    return source or None


class PulseInputStream:
    """sounddevice-compatible input stream backed by PulseAudio's parec."""

    def __init__(
        self,
        samplerate: float,
        channels: int,
        callback,
        blocksize: int = 0,
        **_kwargs,
    ):
        if channels <= 0:
            raise ValueError("channels must be positive")

        self.samplerate = int(round(float(samplerate)))
        self.channels = int(channels)
        self.callback = callback
        self.blocksize = int(blocksize) if blocksize else max(256, self.samplerate // 50)
        self.process: Optional[subprocess.Popen] = None
        self.reader_thread: Optional[threading.Thread] = None
        self.stop_event = threading.Event()

    def _command(self) -> list[str]:
        parec = shutil.which("parec")
        if parec is None:
            raise RuntimeError("parec disappeared after Pulse backend selection")

        command = [
            parec,
            f"--client-name={_PULSE_APPLICATION_NAME}",
            f"--stream-name={_PULSE_APPLICATION_NAME} microphone",
            "--format=s16le",
            f"--rate={self.samplerate}",
            f"--channels={self.channels}",
        ]
        source = _pulse_default_source()
        if source:
            command.append(f"--device={source}")
        return command

    def _environment(self) -> dict[str, str]:
        env = os.environ.copy()
        env["PULSE_PROP_application.name"] = _PULSE_APPLICATION_NAME
        env["PULSE_PROP_application.icon_name"] = _PULSE_ICON_NAME
        env["PULSE_PROP_media.role"] = _PULSE_MEDIA_ROLE
        return env

    def start(self):
        if self.process is not None:
            return self

        available, reason = _pulse_backend_status()
        if not available:
            raise RuntimeError(f"shared Pulse capture unavailable: {reason}")

        self.stop_event.clear()
        self.process = subprocess.Popen(
            self._command(),
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            env=self._environment(),
            bufsize=0,
        )

        try:
            self.process.wait(timeout=0.08)
        except subprocess.TimeoutExpired:
            pass
        else:
            stderr = b""
            if self.process.stderr is not None:
                stderr = self.process.stderr.read()
            detail = stderr.decode("utf-8", errors="replace").strip()
            raise RuntimeError(
                f"parec exited during startup with {self.process.returncode}: "
                f"{detail or 'no error output'}"
            )

        self.reader_thread = threading.Thread(
            target=self._reader_loop,
            name="zara-pulse-capture",
            daemon=True,
        )
        self.reader_thread.start()
        return self

    def _reader_loop(self) -> None:
        process = self.process
        if process is None or process.stdout is None:
            return

        frame_bytes = self.blocksize * self.channels * 2
        while not self.stop_event.is_set():
            try:
                raw = process.stdout.read(frame_bytes)
            except (OSError, ValueError):
                break
            if not raw:
                break

            sample_bytes = len(raw) - (len(raw) % 2)
            if sample_bytes == 0:
                continue

            samples = np.frombuffer(raw[:sample_bytes], dtype="<i2")
            complete = samples.size - (samples.size % self.channels)
            if complete == 0:
                continue

            audio = (
                samples[:complete]
                .reshape(-1, self.channels)
                .astype(np.float32)
                / 32768.0
            )
            if self.callback is not None:
                self.callback(audio, len(audio), None, None)

    def stop(self):
        self.stop_event.set()
        process = self.process
        if process is not None and process.poll() is None:
            process.terminate()
            try:
                process.wait(timeout=1.0)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait(timeout=1.0)

        thread = self.reader_thread
        if thread is not None and thread.is_alive():
            thread.join(timeout=1.0)
        return self

    def close(self):
        self.stop()
        if self.process is not None:
            if self.process.stdout is not None:
                self.process.stdout.close()
            if self.process.stderr is not None:
                self.process.stderr.close()
        self.process = None
        self.reader_thread = None

    def __enter__(self):
        return self.start()

    def __exit__(self, _exc_type, _exc, _tb):
        self.close()
        return False


def _install_shared_input_stream() -> None:
    global _SHARED_STREAM_INSTALLED
    if _SHARED_STREAM_INSTALLED:
        return
    sd.InputStream = PulseInputStream
    _SHARED_STREAM_INSTALLED = True


def _restore_portaudio_input_stream() -> None:
    global _SHARED_STREAM_INSTALLED
    if not _SHARED_STREAM_INSTALLED:
        return
    if sd.InputStream is PulseInputStream:
        sd.InputStream = _ORIGINAL_INPUT_STREAM
    _SHARED_STREAM_INSTALLED = False


def resolve_input_sample_rate(
    target_rate: float,
    channels: int = 1,
    device: Optional[int] = None,
) -> Tuple[float, Optional[str]]:
    if device is None:
        available, reason = _pulse_backend_status()
        if available:
            _install_shared_input_stream()
            source = _pulse_default_source() or "@DEFAULT_SOURCE@"
            return float(target_rate), (
                "Audio input: Pulse shared capture via parec "
                f"(source='{source}', app='{_PULSE_APPLICATION_NAME}')"
            )
        if sys.platform.startswith("linux") and _prefer_shared_input():
            shared_note = f"Audio input: shared Pulse capture unavailable ({reason}); "
        else:
            shared_note = ""
        _restore_portaudio_input_stream()
    else:
        shared_note = ""
        _restore_portaudio_input_stream()

    try:
        sd.check_input_settings(
            device=device,
            samplerate=target_rate,
            channels=channels,
        )
        note = f"{shared_note}using PortAudio" if shared_note else None
        return float(target_rate), note
    except Exception as exc:
        try:
            device_id = device if device is not None else sd.default.device[0]
            info = sd.query_devices(device_id, "input")
            default_rate = float(info["default_samplerate"])
        except Exception as dev_exc:
            return float(target_rate), (
                f"{shared_note}Audio input sample rate check failed; "
                f"using configured {target_rate}Hz. Details: {exc}; "
                f"device lookup error: {dev_exc}"
            )

        try:
            sd.check_input_settings(
                device=device,
                samplerate=default_rate,
                channels=channels,
            )
            return default_rate, (
                f"{shared_note}Audio input sample rate {target_rate}Hz not supported; "
                f"falling back to device default {default_rate}Hz"
            )
        except Exception as fallback_exc:
            return float(target_rate), (
                f"{shared_note}Audio input sample rate check failed; "
                f"using configured {target_rate}Hz. Details: {exc}; "
                f"fallback check error: {fallback_exc}"
            )


def resample_audio(audio: np.ndarray, input_rate: float, target_rate: float) -> np.ndarray:
    if input_rate == target_rate or audio.size == 0:
        return audio

    ratio = target_rate / input_rate
    new_length = int(round(audio.shape[0] * ratio))
    if new_length <= 0:
        return audio[:0]

    x_old = np.arange(audio.shape[0], dtype=np.float32)
    x_new = np.linspace(0, audio.shape[0] - 1, new_length, dtype=np.float32)

    if audio.ndim == 1:
        resampled = np.interp(x_new, x_old, audio).astype(np.float32)
        return resampled

    channels = audio.shape[1]
    resampled_channels = []
    for ch in range(channels):
        resampled_channels.append(np.interp(x_new, x_old, audio[:, ch]))
    return np.stack(resampled_channels, axis=1).astype(np.float32)


class AudioCapture:
    """Capture audio from microphone"""

    def __init__(self, sample_rate=16000, channels=1):
        self.sample_rate = sample_rate
        self.channels = channels
        self.stream = None
        self.running = False

    def _callback(self, indata, frames, time_info, status):
        """Audio callback - puts data in queue"""
        if status:
            print(f"Audio capture status: {status}")

        if hasattr(self, "queue"):
            try:
                self.queue.put_nowait(indata.copy())
            except queue.Full:
                pass

    def start(self, output_queue: queue.Queue):
        """Start capturing audio"""
        self.queue = output_queue
        self.running = True

        self.stream = sd.InputStream(
            samplerate=self.sample_rate,
            channels=self.channels,
            callback=self._callback,
        )
        self.stream.start()
        print(f"🎤 Audio capture started ({self.sample_rate}Hz, {self.channels}ch)")

    def stop(self):
        """Stop capturing"""
        self.running = False
        if self.stream:
            self.stream.stop()
            self.stream.close()
        print("🎤 Audio capture stopped")


class AudioOutput:
    """Play audio through speakers"""

    def __init__(self, sample_rate=22050):
        self.sample_rate = sample_rate

    def play(self, audio_data: bytes):
        """Play audio data (blocking)"""
        if not audio_data:
            return

        try:
            try:
                audio_array = np.frombuffer(audio_data, dtype=np.float32)
                audio_float = audio_array
            except ValueError:
                try:
                    audio_array = np.frombuffer(audio_data, dtype=np.int16)
                    audio_float = audio_array.astype(np.float32) / 32768.0
                except ValueError:
                    audio_float = np.frombuffer(audio_data, dtype=np.uint8)
                    audio_float = audio_float.astype(np.float32) / 255.0

            if (
                len(audio_float) % 2 == 0
                and audio_float.max() <= 1.0
                and audio_float.min() >= -1.0
            ):
                try:
                    audio_float = audio_float.reshape(-1, 2)
                    if audio_float.shape[1] == 2:
                        audio_float = np.mean(audio_float, axis=1)
                except Exception:
                    pass

            if len(audio_float) == 0:
                print("Warning: Empty audio data")
                return

            sd.play(audio_float, samplerate=self.sample_rate)
            sd.wait()

        except Exception as e:
            print(f"Audio playback error: {e}")
            print(f"Audio data length: {len(audio_data)} bytes")
            print(f"Sample rate: {self.sample_rate}")
            if len(audio_data) >= 4:
                print(f"First 4 bytes: {audio_data[:4].hex()}")

    def play_async(self, audio_data: bytes):
        """Play audio in background thread"""
        thread = Thread(target=self.play, args=(audio_data,), daemon=True)
        thread.start()


if __name__ == "__main__":
    print("Testing audio capture for 3 seconds...")

    test_queue = queue.Queue()
    capture = AudioCapture()
    capture.start(test_queue)

    time.sleep(3)

    capture.stop()

    print(f"Captured {test_queue.qsize()} audio chunks")

    print("\nTesting audio playback (440Hz tone)...")

    duration = 1.0
    sample_rate = 22050
    frequency = 440.0

    t = np.linspace(0, duration, int(sample_rate * duration))
    tone = np.sin(2 * np.pi * frequency * t)
    tone_int16 = (tone * 32767).astype(np.int16)

    output = AudioOutput(sample_rate=sample_rate)
    output.play(tone_int16.tobytes())

    print("Audio tests complete")
