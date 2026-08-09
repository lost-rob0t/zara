"""
Audio I/O - capture and playback
"""

import os
import queue
import sys
from typing import Optional, Tuple
import numpy as np
import sounddevice as sd
from threading import Thread


_SHARED_INPUT_NAMES = ("pipewire", "pulse")


def _prefer_shared_input() -> bool:
    value = os.getenv("ZARA_PREFER_SHARED_INPUT", "1").strip().lower()
    return value not in {"0", "false", "no", "off"}


def _shared_input_device(channels: int = 1) -> Optional[Tuple[int, str]]:
    """Return the best PortAudio device backed by PipeWire/PulseAudio."""
    if not sys.platform.startswith("linux") or not _prefer_shared_input():
        return None

    try:
        devices = sd.query_devices()
    except Exception:
        return None

    candidates = []
    for index, info in enumerate(devices):
        try:
            max_inputs = int(info.get("max_input_channels", 0))
        except (TypeError, ValueError, AttributeError):
            continue
        if max_inputs < channels:
            continue

        name = str(info.get("name", "")).strip()
        lowered = name.lower()
        priority = None
        for rank, shared_name in enumerate(_SHARED_INPUT_NAMES):
            if lowered == shared_name:
                priority = rank
                break
            if shared_name in lowered:
                priority = rank + len(_SHARED_INPUT_NAMES)
                break
        if priority is not None:
            candidates.append((priority, index, name))

    if not candidates:
        return None

    _, index, name = min(candidates)
    return index, name


def _set_default_input_device(device_id: int) -> None:
    """Change only PortAudio's default input, preserving the output device."""
    try:
        current = sd.default.device
        output_id = current[1]
    except Exception:
        output_id = None
    sd.default.device = (device_id, output_id)


def resolve_input_sample_rate(
    target_rate: float,
    channels: int = 1,
    device: Optional[int] = None,
) -> Tuple[float, Optional[str]]:
    selected_device = device
    shared_note = None

    if selected_device is None:
        shared = _shared_input_device(channels=channels)
        if shared is not None:
            selected_device, device_name = shared
            _set_default_input_device(selected_device)
            shared_note = f"Audio input: using shared device '{device_name}'"

    try:
        sd.check_input_settings(
            device=selected_device,
            samplerate=target_rate,
            channels=channels,
        )
        return float(target_rate), shared_note
    except Exception as exc:
        try:
            device_id = selected_device if selected_device is not None else sd.default.device[0]
            info = sd.query_devices(device_id, "input")
            default_rate = float(info["default_samplerate"])
        except Exception as dev_exc:
            return float(target_rate), (
                f"Audio input sample rate check failed; using configured {target_rate}Hz. "
                f"Details: {exc}; device lookup error: {dev_exc}"
            )

        try:
            sd.check_input_settings(
                device=selected_device,
                samplerate=default_rate,
                channels=channels,
            )
            prefix = f"{shared_note}; " if shared_note else ""
            return default_rate, (
                f"{prefix}Audio input sample rate {target_rate}Hz not supported; "
                f"falling back to device default {default_rate}Hz"
            )
        except Exception as fallback_exc:
            return float(target_rate), (
                f"Audio input sample rate check failed; using configured {target_rate}Hz. "
                f"Details: {exc}; fallback check error: {fallback_exc}"
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

        if hasattr(self, 'queue'):
            try:
                self.queue.put_nowait(indata.copy())
            except queue.Full:
                # Drop frame if queue is full
                pass

    def start(self, output_queue: queue.Queue):
        """Start capturing audio"""
        self.queue = output_queue
        self.running = True

        self.stream = sd.InputStream(
            samplerate=self.sample_rate,
            channels=self.channels,
            callback=self._callback
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
            # Convert bytes to numpy array
            # Try different formats that TTS might produce
            try:
                # Try 32-bit float first
                audio_array = np.frombuffer(audio_data, dtype=np.float32)
                audio_float = audio_array
            except ValueError:
                try:
                    # Try 16-bit PCM
                    audio_array = np.frombuffer(audio_data, dtype=np.int16)
                    audio_float = audio_array.astype(np.float32) / 32768.0
                except ValueError:
                    # Try 24-bit or other format - interpret as bytes then convert
                    audio_float = np.frombuffer(audio_data, dtype=np.uint8)
                    audio_float = audio_float.astype(np.float32) / 255.0

            # Reshape if stereo
            if len(audio_float) % 2 == 0 and audio_float.max() <= 1.0 and audio_float.min() >= -1.0:
                # Might be stereo interleaved
                try:
                    audio_float = audio_float.reshape(-1, 2)
                    if audio_float.shape[1] == 2:
                        # Convert to mono for playback
                        audio_float = np.mean(audio_float, axis=1)
                except:
                    pass  # Keep as mono if reshaping fails

            # Ensure we have valid audio data
            if len(audio_float) == 0:
                print("Warning: Empty audio data")
                return

            # Play
            sd.play(audio_float, samplerate=self.sample_rate)
            sd.wait()

        except Exception as e:
            print(f"Audio playback error: {e}")
            print(f"Audio data length: {len(audio_data)} bytes")
            print(f"Sample rate: {self.sample_rate}")
            # Try to detect format
            if len(audio_data) >= 4:
                print(f"First 4 bytes: {audio_data[:4].hex()}")

    def play_async(self, audio_data: bytes):
        """Play audio in background thread"""
        thread = Thread(target=self.play, args=(audio_data,), daemon=True)
        thread.start()


if __name__ == "__main__":
    # Test audio capture
    import time

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
