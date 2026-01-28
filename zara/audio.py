"""
Audio I/O - capture and playback
"""

import queue
import numpy as np
import sounddevice as sd
from threading import Thread


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
        # Convert bytes to numpy array
        # Assumes 16-bit PCM
        audio_array = np.frombuffer(audio_data, dtype=np.int16)
        
        # Convert to float for sounddevice
        audio_float = audio_array.astype(np.float32) / 32768.0
        
        # Play
        sd.play(audio_float, samplerate=self.sample_rate)
        sd.wait()
    
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
    
    # Test playback with a simple tone
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
