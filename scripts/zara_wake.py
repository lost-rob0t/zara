#!/usr/bin/env python3
"""
Wake Word Listener for Zarathushtra
"""

import os
import sys
import time
import queue
import subprocess
import numpy as np
import sounddevice as sd
from threading import Event
from pathlib import Path
from faster_whisper import WhisperModel

SAMPLE_RATE = 16000
CHANNELS = 1
PASSIVE_CHUNK_SEC = 3
ACTIVE_CHUNK_SEC = 8
WAKE_WORDS = ["zarathustra", "hey zara", "zara", "sarah"]
STOP_PHRASES = ["end", "end quote", "end zara", "stop conversation"]
TIMEOUT_ACTIVE = 10

PIDFILE = "/tmp/zara_wakeword.pid"
LOGFILE = "/tmp/zara_wakeword.log"

# Point to your main.pl, not a handler
PROLOG_MAIN = Path(__file__).parent / "main.pl"




class Zara:
    def __init__(self, whisper_model="tiny.en",
                 device="cpu",
                 chat_history=None,
                 chat_router=None,
                 settings={},
                 ):
        self.chat_history = chat_history or []
        self.waker = WakeWordListener()
        self.chat_router=None
        self.prolog_client = None
        self.llm = None
        self.tools = {}
        self.listener = WakeWordListener(whisper_model, device)
    def normalize(self, text):
        return text.lower()

    def handle_text(self, text):
        normalized = self.normalize(text)
        self.chat_history.append()




class WakeWordListener:
    def __init__(self, model="tiny.en", device="cpu"):
        self.state = "PASSIVE"
        self.audio_queue = queue.Queue()
        self.stop_event = Event()
        self.last_activity = time.time()

        threads = os.cpu_count()
        self.log(f"Loading Whisper {model} (threads={threads})")

        self.model = WhisperModel(
            model, device=device,
            compute_type="int8", num_workers=threads
        )

    def log(self, msg):
        ts = time.strftime("%H:%M:%S")
        with open(LOGFILE, "a") as f:
            f.write(f"[{ts}] {msg}\n")
        print(f"[{ts}] {msg}", flush=True)

    def audio_callback(self, indata, frames, time_info, status):
        if status:
            self.log(f"Audio: {status}")
        self.audio_queue.put(indata.copy())

    def transcribe(self, audio_data):
        if audio_data.ndim > 1:
            audio_data = audio_data[:, 0]

        segments, _ = self.model.transcribe(
            audio_data.astype(np.float32),
            beam_size=1,
            vad_filter=True,
            language="en",
            no_speech_threshold=0.5
        )

        return " ".join(seg.text.strip() for seg in segments).strip()

    def collect_audio(self, seconds):
        target_frames = int(seconds * SAMPLE_RATE)
        buffer = np.zeros((0, CHANNELS), dtype="float32")

        while buffer.shape[0] < target_frames and not self.stop_event.is_set():
            try:
                data = self.audio_queue.get(timeout=0.5)
                buffer = np.concatenate((buffer, data))
            except queue.Empty:
                continue

        return buffer[:target_frames] if buffer.shape[0] >= target_frames else None

    def clear_queue(self):
        """Drain audio queue to prevent stale data"""
        try:
            while True:
                self.audio_queue.get_nowait()
        except queue.Empty:
            pass

    def check_wake_word(self, text):
        text_lower = text.lower()
        return any(wake in text_lower for wake in WAKE_WORDS)

    def strip_wake_word(self, text):
        """Remove wake word from command"""
        text_lower = text.lower()
        for wake in WAKE_WORDS:
            if wake in text_lower:
                # Remove wake word and any leading/trailing noise
                parts = text_lower.split(wake, 1)
                if len(parts) > 1:
                    return parts[1].strip()
        return text.strip()

    def send_to_prolog(self, command_text):
        """Pass string to Prolog via stdin"""
        try:
            proc = subprocess.Popen(
                ["swipl", "-q", "-s", str(PROLOG_MAIN)],
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True
            )

            stdout, stderr = proc.communicate(input=f"{command_text}\nquit\n", timeout=10)

            if stdout:
                self.log(f"Response:\n{stdout.strip()}")
            if stderr:
                self.log(f"Error: {stderr.strip()}")

        except subprocess.TimeoutExpired:
            proc.kill()
            self.log("Prolog timeout")
        except Exception as e:
            self.log(f"Prolog error: {e}")


    def run(self):
        self.log("🔥 Starting Wake Word Listener")
        self.log(f"Wake words: {', '.join(WAKE_WORDS)}")

        with open(PIDFILE, "w") as f:
            f.write(str(os.getpid()))

        with sd.InputStream(
            samplerate=SAMPLE_RATE,
            channels=CHANNELS,
            callback=self.audio_callback
        ):
            while not self.stop_event.is_set():
                if self.state == "PASSIVE":
                    self.passive_mode()
                elif self.state == "ACTIVE":
                    self.active_mode()

        Path(PIDFILE).unlink(missing_ok=True)
        self.log("Stopped")

    def passive_mode(self):
        """Listen for wake word"""
        chunk = self.collect_audio(PASSIVE_CHUNK_SEC)
        if chunk is None:
            return

        text = self.transcribe(chunk)

        if self.check_wake_word(text):
            self.log(f"🔥 Wake word detected")
            self.clear_queue()  # Critical: prevent old audio
            self.state = "ACTIVE"
            self.last_activity = time.time()



    def active_mode(self):
        """Transcribe and execute command"""
        if time.time() - self.last_activity > TIMEOUT_ACTIVE:
            self.log("⏸️ Timeout - returning to passive")
            self.state = "PASSIVE"
            return

        chunk = self.collect_audio(ACTIVE_CHUNK_SEC)
        if chunk is None:
            return

        text = self.transcribe(chunk)

        if not text or len(text) < 3:
            # Silence or noise, stay active briefly
            return

        self.log(f"📝 Heard: '{text}'")
        self.last_activity = time.time()

        text_lower = text.lower()

        # Stop commands
        if any(stop in text_lower for stop in ['stop', 'sleep', 'goodbye', 'never mind']):
            self.log("Returning to passive mode")
            self.state = "PASSIVE"
            return

        # Strip wake word if user repeated it
        command = self.strip_wake_word(text)

        if command:
            self.send_to_prolog(command)

        # Stay active for follow-up commands
        # Will timeout naturally if user stops talking


def main():
    model = sys.argv[1] if len(sys.argv) > 1 else "tiny.en"
    device = sys.argv[2] if len(sys.argv) > 2 else "cpu"

    listener = WakeWordListener(model, device)

    try:
        listener.run()
    except KeyboardInterrupt:
        listener.stop_event.set()
        listener.log("Interrupted")


if __name__ == "__main__":
    main()
