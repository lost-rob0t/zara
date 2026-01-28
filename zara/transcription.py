"""
Audio transcription using faster-whisper
"""

import asyncio
import numpy as np
from faster_whisper import WhisperModel
from typing import Optional


class Transcriber:
    """Async wrapper for faster-whisper"""
    
    def __init__(self, model: str = "small", device: str = "cpu", threads: Optional[int] = None):
        self.model_name = model
        self.device = device
        
        if threads is None:
            import os
            threads = os.cpu_count()
        
        compute_type = "int8" if device == "cpu" else "float16"
        
        print(f"Loading Whisper model '{model}' on {device}...")
        self.model = WhisperModel(
            model,
            device=device,
            compute_type=compute_type,
            num_workers=threads
        )
        print(f"✓ Whisper loaded ({threads} threads)")
    
    async def transcribe_async(self, audio_data: bytes) -> str:
        """
        Transcribe audio asynchronously
        Returns: transcribed text or empty string
        """
        # Run transcription in executor to avoid blocking
        return await asyncio.get_event_loop().run_in_executor(
            None, self._transcribe_sync, audio_data
        )
    
    def _transcribe_sync(self, audio_data: bytes) -> str:
        """Synchronous transcription"""
        # Convert bytes to float32 numpy array
        audio_array = np.frombuffer(audio_data, dtype=np.int16)
        audio_float = audio_array.astype(np.float32) / 32768.0
        
        # Flatten if stereo
        if audio_float.ndim > 1:
            audio_float = audio_float[:, 0]
        
        # Check for silence (avoid hallucinations)
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
            
            # Collect text from segments
            texts = []
            for segment in segments:
                text = segment.text.strip()
                if text:
                    texts.append(text)
            
            result = " ".join(texts)
            
            # Filter known hallucinations
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
    # Test with a simple recording
    import sounddevice as sd
    import time
    
    print("Testing transcription...")
    print("Recording 3 seconds of audio...")
    
    sample_rate = 16000
    duration = 3
    
    # Record
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
