#!/usr/bin/env python3
"""
Wake Word Listener for Zarathushtra - ASYNC VERSION
Hybrid architecture: Execution commands → Prolog, Conversational queries → LLM
Fully async for maximum responsiveness
"""

import os
import time
import asyncio
import pathlib
import numpy as np
import sounddevice as sd
import faster_whisper
import pyswip
from concurrent.futures import ThreadPoolExecutor

# Import LLM client and ChatHistory
from . import llm
from .tts import TTSEngine

SAMPLE_RATE = 16000
CHANNELS = 1
PASSIVE_CHUNK_SEC = 3
ACTIVE_CHUNK_SEC = 8
WAKE_WORDS = ["zarathustra", "hey zara", "zara", "sarah"]
STOP_PHRASES = ["end", "end quote", "end zara", "stop conversation"]
TIMEOUT_ACTIVE = 10

PIDFILE = "/tmp/zara_wakeword.pid"
LOGFILE = "/tmp/zara_wakeword.log"


def load_llm_config():
    """
    Load LLM configuration from environment variables or Prolog config.
    Falls back to defaults if not found.
    """
    provider = os.getenv("ZARA_LLM_PROVIDER", "ollama")
    model = os.getenv("ZARA_LLM_MODEL")
    endpoint = os.getenv("ZARA_LLM_ENDPOINT")

    return {
        "provider": provider,
        "model": model,
        "endpoint": endpoint
    }


def load_tts_config():
    """Load TTS configuration from environment variables"""
    provider = os.getenv("ZARA_TTS_PROVIDER", "qwen3")
    qwen3_url = os.getenv("QWEN3_TTS_URL", "http://localhost:7860")
    qwen3_voice = os.getenv("QWEN3_VOICE", "demo_speaker0")

    return {
        "provider": provider,
        "qwen3_url": qwen3_url,
        "qwen3_voice": qwen3_voice
    }


class WakeWordListener:
    def __init__(self, model="tiny.en", device="cpu", prolog_main_path=None, enable_tts=True):
        self.state = "PASSIVE"
        self.audio_queue = asyncio.Queue()
        self.stop_event = asyncio.Event()
        self.last_activity = time.time()
        self.enable_tts = enable_tts

        # Thread pool for CPU-bound operations
        self.executor = ThreadPoolExecutor(max_workers=4)

        # Chat history for conversational context
        self.chat_history = llm.ChatHistory(max_length=20)

        # LLM client (lazy init on first conversational query)
        self.llm_client = None
        self.llm_config = load_llm_config()

        # TTS client (lazy init on first use)
        self.tts_client = None
        self.tts_config = load_tts_config()

        # Initialize Prolog
        self.prolog = pyswip.Prolog()
        self.log("Initializing Prolog...")

        # Find main.pl
        if prolog_main_path:
            prolog_path = pathlib.Path(prolog_main_path)
        else:
            # Try multiple locations
            candidates = [
                pathlib.Path.cwd() / "main.pl",
                pathlib.Path(__file__).parent.parent / "main.pl",
                pathlib.Path("/usr/share/zarathushtra/main.pl"),
            ]
            prolog_path = None
            for candidate in candidates:
                if candidate.exists():
                    prolog_path = candidate
                    break

        if not prolog_path or not prolog_path.exists():
            raise FileNotFoundError(f"Could not find main.pl. Tried: {candidates}")

        self.log(f"Consulting Prolog: {prolog_path}")
        self.prolog.consult(str(prolog_path))

        threads = os.cpu_count()
        self.log(f"Loading Whisper {model} (threads={threads})")
        self.log(f"LLM Provider: {self.llm_config['provider']}")
        if self.enable_tts:
            self.log(f"TTS Provider: {self.tts_config['provider']}")

        self.model = faster_whisper.WhisperModel(
            model, device=device,
            compute_type="int8", num_workers=threads
        )

    def log(self, msg):
        ts = time.strftime("%H:%M:%S")
        with open(LOGFILE, "a") as f:
            f.write(f"[{ts}] {msg}\n")
        print(f"[{ts}] {msg}", flush=True)

    def audio_callback(self, indata, frames, time_info, status):
        """Audio callback - runs in separate thread, needs thread-safe queue push"""
        if status:
            self.log(f"Audio: {status}")

        # Put audio in queue in a thread-safe way
        # We'll use call_soon_threadsafe to put it in the async queue
        asyncio.run_coroutine_threadsafe(
            self.audio_queue.put(indata.copy()),
            asyncio.get_event_loop()
        )

    async def transcribe_async(self, audio_data):
        """Run Whisper transcription in thread pool to avoid blocking"""
        loop = asyncio.get_event_loop()

        def _transcribe():
            if audio_data.ndim > 1:
                audio_data_mono = audio_data[:, 0]
            else:
                audio_data_mono = audio_data

            segments, _ = self.model.transcribe(
                audio_data_mono.astype(np.float32),
                beam_size=1,
                vad_filter=True,
                language="en",
                no_speech_threshold=0.5
            )

            return " ".join(seg.text.strip() for seg in segments).strip()

        return await loop.run_in_executor(self.executor, _transcribe)

    async def collect_audio(self, seconds):
        """Collect audio for specified duration"""
        target_frames = int(seconds * SAMPLE_RATE)
        buffer = np.zeros((0, CHANNELS), dtype="float32")

        while buffer.shape[0] < target_frames and not self.stop_event.is_set():
            try:
                data = await asyncio.wait_for(self.audio_queue.get(), timeout=0.5)
                buffer = np.concatenate((buffer, data))
            except asyncio.TimeoutError:
                continue

        return buffer[:target_frames] if buffer.shape[0] >= target_frames else None

    async def clear_queue(self):
        """Drain audio queue to prevent stale data"""
        while not self.audio_queue.empty():
            try:
                self.audio_queue.get_nowait()
            except asyncio.QueueEmpty:
                break

    def check_wake_word(self, text):
        text_lower = text.lower()
        return any(wake in text_lower for wake in WAKE_WORDS)

    def strip_wake_word(self, text):
        """Remove wake word from command"""
        text_lower = text.lower()
        for wake in WAKE_WORDS:
            if wake in text_lower:
                parts = text_lower.split(wake, 1)
                if len(parts) > 1:
                    return parts[1].strip()
        return text.strip()

    async def query_prolog_async(self, command_text):
        """
        Query Prolog intent resolver and executor in thread pool.
        Returns: (needs_llm: bool, result: str)
        """
        loop = asyncio.get_event_loop()

        def _query_prolog():
            try:
                query = f"intent_resolver:resolve(\"{command_text}\", Intent, Args)"
                self.log(f"Prolog query: {query}")

                results = list(self.prolog.query(query))

                if not results:
                    self.log("Prolog resolution failed - routing to LLM")
                    return (True, "Resolution failed")

                result = results[0]
                intent = result["Intent"]
                args = result["Args"]

                self.log(f"Resolved: intent={intent}, args={args}")

                if intent == "ask":
                    self.log("Intent is 'ask' - routing to LLM")
                    return (True, "LLM fallback requested")

                exec_query = f"commands:execute({intent}, {args})"
                self.log(f"Executing: {exec_query}")

                exec_results = list(self.prolog.query(exec_query))

                if exec_results is not None:
                    return (False, f"Executed: {intent} {args}")
                else:
                    return (False, f"Execution failed for {intent}")

            except Exception as e:
                self.log(f"Prolog error: {e}")
                return (True, f"Prolog error: {str(e)}")

        return await loop.run_in_executor(self.executor, _query_prolog)

    async def query_llm_async(self, query_text):
        """Query LLM with chat history for conversational responses"""
        try:
            if self.llm_client is None:
                self.log(f"Initializing LLM client: {self.llm_config['provider']}")
                self.llm_client = llm.LLMClient(
                    provider=self.llm_config['provider'],
                    model=self.llm_config['model'],
                    endpoint=self.llm_config['endpoint']
                )

            # Use async query directly
            response = await self.llm_client.query_async(
                query_text,
                chat_history=self.chat_history.get_messages()
            )

            self.log(f"LLM response: {response}")
            return response

        except Exception as e:
            error_msg = f"LLM error: {str(e)}"
            self.log(error_msg)
            return error_msg

    async def synthesize_and_play_async(self, text):
        """Synthesize text to speech and play it"""
        try:
            if self.tts_client is None:
                self.log(f"Initializing TTS client: {self.tts_config['provider']}")
                self.tts_client = TTSEngine(provider=self.tts_config['provider'])

            self.log("Synthesizing speech...")
            audio_bytes = await self.tts_client.synthesize_async(text)

            if audio_bytes:
                # Save to temp file and play in thread pool
                import tempfile
                import subprocess

                loop = asyncio.get_event_loop()

                def _play_audio():
                    with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as f:
                        temp_file = f.name
                        f.write(audio_bytes)

                    try:
                        subprocess.run(["aplay", temp_file], capture_output=True, check=True)
                    finally:
                        os.unlink(temp_file)

                await loop.run_in_executor(self.executor, _play_audio)
                self.log("Audio playback complete")
            else:
                self.log("Failed to synthesize speech")

        except Exception as e:
            self.log(f"TTS error: {e}")

    async def send_response_async(self, title, message):
        """Send response via notification and optionally TTS"""
        try:
            import subprocess

            loop = asyncio.get_event_loop()

            def _send_notification():
                subprocess.run(
                    ["notify-send", "-u", "normal", title, message],
                    capture_output=True,
                    text=True,
                    timeout=5
                )

            # Send notification in background
            await loop.run_in_executor(self.executor, _send_notification)
            self.log(f"Sent notification: {title}")

            # If TTS is enabled, also speak the response
            if self.enable_tts:
                await self.synthesize_and_play_async(message)

        except Exception as e:
            self.log(f"Failed to send response: {e}")
            self.log(f"Response: {message[:200]}...")

    async def run_async(self):
        """Main async event loop"""
        self.log("🔥 Starting Wake Word Listener (ASYNC)")
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
                    await self.passive_mode_async()
                elif self.state == "ACTIVE":
                    await self.active_mode_async()

        pathlib.Path(PIDFILE).unlink(missing_ok=True)
        self.log("Stopped")

    async def passive_mode_async(self):
        """Listen for wake word"""
        chunk = await self.collect_audio(PASSIVE_CHUNK_SEC)
        if chunk is None:
            return

        text = await self.transcribe_async(chunk)

        if self.check_wake_word(text):
            self.log(f"🔥 Wake word detected")
            await self.clear_queue()
            self.state = "ACTIVE"
            self.last_activity = time.time()

    async def active_mode_async(self):
        """Transcribe and route command (Prolog first, LLM if needed)"""
        if time.time() - self.last_activity > TIMEOUT_ACTIVE:
            self.log("⏸️ Timeout - returning to passive")
            self.state = "PASSIVE"
            return

        chunk = await self.collect_audio(ACTIVE_CHUNK_SEC)
        if chunk is None:
            return

        text = await self.transcribe_async(chunk)

        if not text or len(text) < 3:
            return

        self.log(f"📝 Heard: '{text}'")
        self.last_activity = time.time()

        text_lower = text.lower()

        if any(stop in text_lower for stop in ['stop', 'sleep', 'goodbye', 'never mind']):
            self.log("Returning to passive mode")
            self.state = "PASSIVE"
            return

        command = self.strip_wake_word(text)

        if not command:
            return

        needs_llm, prolog_result = await self.query_prolog_async(command)

        if needs_llm:
            self.log("Routing to LLM for conversational response")
            response = await self.query_llm_async(command)

            self.chat_history.add_user_message(command)
            self.chat_history.add_assistant_message(response)

            await self.send_response_async("Zara", response)

        else:
            self.log(f"Prolog executed: {prolog_result}")

            self.chat_history.add_user_message(command)
            self.chat_history.add_assistant_message(prolog_result)


def main(model="tiny.en", device="cpu", prolog_main_path=None, enable_tts=True):
    """Main entry point for wake word listener"""
    listener = WakeWordListener(
        model=model,
        device=device,
        prolog_main_path=prolog_main_path,
        enable_tts=enable_tts
    )

    async def run_with_cleanup():
        try:
            await listener.run_async()
        except KeyboardInterrupt:
            listener.stop_event.set()
            listener.log("Interrupted")
        finally:
            listener.executor.shutdown(wait=True)
            if listener.tts_client:
                await listener.tts_client.close()

    try:
        asyncio.run(run_with_cleanup())
    except KeyboardInterrupt:
        pass

    return 0
