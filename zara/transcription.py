"""
Audio transcription using faster-whisper
"""

import asyncio
import os
from typing import Optional

import numpy as np
from faster_whisper import WhisperModel


_GPU_DEVICE_ALIASES = {"amd", "hip", "rocm"}


def normalize_device(device: str) -> str:
    normalized = str(device).strip().lower()
    if normalized == "cpu":
        return "cpu"
    if normalized == "cuda" or normalized in _GPU_DEVICE_ALIASES:
        return "cuda"
    raise ValueError(f"Unsupported transcription device: {device!r}")


class Transcriber:
    """Async wrapper for faster-whisper"""

    def __init__(self, model: str = "small", device: str = "cpu", threads: Optional[int] = None):
        self.model_name = model
        self.device = normalize_device(device)

        if threads is None:
            threads = os.cpu_count() or 1
        cpu_threads = max(1, int(threads))

        compute_type = "int8" if self.device == "cpu" else "float16"

        print(
            f"Loading Whisper model '{model}' on {self.device} "
            f"({compute_type=}, cpu_threads={cpu_threads})..."
        )
        self.model = WhisperModel(
            model,
            device=self.device,
            compute_type=compute_type,
            cpu_threads=cpu_threads,
            num_workers=1,
        )
        print(f"✓ Whisper loaded ({cpu_threads} CPU threads, 1 model worker)")

    async def transcribe_async(self, audio_data: bytes) -> str:
        """
        Transcribe audio asynchronously
        Returns: transcribed text or empty string
        """
        return await asyncio.get_event_loop().run_in_executor(
            None, self._transcribe_sync, audio_data
        )

    def _transcribe_sync(self, audio_data: bytes) -> str:
        """Synchronous transcription"""
        audio_array = np.frombuffer(audio_data, dtype=np.int16)
        audio_float = audio_array.astype(np.float32) / 32768.0

        if audio_float.ndim > 1:
            audio_float = audio_float[:, 0]

        rms = np.sqrt(np.mean(audio_float ** 2))
        if rms < 0.001:
            return ""

        try:
            segments, info = self.model.transcribe(
                audio_float,
                beam_size=1,
                vad_filter=True,
                language="en",
                condition_on_previous_text=False,
                no_speech_threshold=0.4
            )

            texts = []
            for segment in segments:
                text = segment.text.strip()
                if text:
                    texts.append(text)

            result = " ".join(texts)

            if result.lower().strip() in ["thank you", "thanks"]:
                return ""

            return result

        except Exception as e:
            print(f"Transcription error: {e}")
            return ""

    def transcribe(self, audio_data: bytes) -> str:
        """Synchronous wrapper"""
        return self._transcribe_sync(audio_data)


if __name__ == "__main__":
    import sounddevice as sd

    print("Testing transcription...")
    print("Recording 3 seconds of audio...")

    sample_rate = 16000
    duration = 3

    audio = sd.rec(
        int(duration * sample_rate),
        samplerate=sample_rate,
        channels=1,
        dtype=np.int16
    )
    sd.wait()

    print("Transcribing...")

    transcriber = Transcriber("tiny")
    text = transcriber.transcribe(audio.tobytes())

    print(f"Result: {text}")
