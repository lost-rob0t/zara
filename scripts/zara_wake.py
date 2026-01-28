#!/usr/bin/env python3
"""
Wake Word Listener for Zarathushtra
Hybrid architecture: Execution commands → Prolog, Conversational queries → LLM
"""

import os
import sys
import time
import queue
import asyncio
import threading
import pathlib
import numpy as np
import sounddevice as sd
import faster_whisper
import pyswip

# Import LLM client and ChatHistory
sys.path.insert(0, str(pathlib.Path(__file__).parent.parent))
import zara.llm

SAMPLE_RATE = 16000
CHANNELS = 1
PASSIVE_CHUNK_SEC = 3
ACTIVE_CHUNK_SEC = 8
WAKE_WORDS = ["zarathustra", "hey zara", "zara", "sarah"]
STOP_PHRASES = ["end", "end quote", "end zara", "stop conversation"]
TIMEOUT_ACTIVE = 10

PIDFILE = "/tmp/zara_wakeword.pid"
LOGFILE = "/tmp/zara_wakeword.log"

# Point to main.pl
PROLOG_MAIN = pathlib.Path(__file__).parent / "main.pl"


def load_llm_config():
    """
    Load LLM configuration from environment variables or Prolog config.
    Falls back to defaults if not found.
    """
    provider = os.getenv("ZARA_LLM_PROVIDER", "ollama")
    model = os.getenv("ZARA_LLM_MODEL")
    endpoint = os.getenv("ZARA_LLM_ENDPOINT")

    # TODO: Could also parse ~/.zarathushtra/config.pl for Prolog config
    # For now, use environment variables as primary method

    return {
        "provider": provider,
        "model": model,  # None uses provider default
        "endpoint": endpoint  # None uses provider default
    }


def classify_intent(text):
    """
    Classify user input as 'execution' or 'conversation'.

    Execution commands: open, search, set, play, start, launch, etc.
    Conversational queries: what, why, how, tell me, who, explain, etc.
    """
    text_lower = text.lower().strip()

    # Conversational query keywords (check first - higher priority)
    conversational_keywords = [
        "what", "why", "how", "when", "where", "who",
        "tell me", "explain", "describe", "list",
        "can you", "could you", "would you",
        "do you", "are you", "is it", "was it",
        "help", "suggest", "recommend"
    ]

    # Execution command keywords (verbs)
    execution_keywords = [
        "open", "close", "launch", "start", "stop", "quit", "kill",
        "search", "find", "look up", "google",
        "set", "create", "make", "add", "remove", "delete",
        "play", "pause", "skip", "next", "previous",
        "volume", "mute", "unmute",
        "timer", "alarm", "reminder",
        "todo", "task", "note",
        "lock", "unlock", "shutdown", "reboot", "restart",
        "show", "display", "navigate"
    ]

    # Check for conversational patterns first (they take priority)
    first_word = text_lower.split()[0] if text_lower.split() else ""
    if first_word in conversational_keywords:
        return "conversation"

    # Check for question words at start
    if any(text_lower.startswith(kw) for kw in conversational_keywords):
        return "conversation"

    # Check for execution keywords as first word (imperative commands)
    if first_word in execution_keywords:
        return "execution"

    # Check for conversational patterns anywhere in text
    if any(keyword in text_lower for keyword in conversational_keywords):
        return "conversation"

    # Check for execution keywords anywhere
    if any(keyword in text_lower for keyword in execution_keywords):
        return "execution"

    # Default to execution (conservative - let Prolog handle it)
    return "execution"




class WakeWordListener:
    def __init__(self, model="tiny.en", device="cpu"):
        self.state = "PASSIVE"
        self.audio_queue = queue.Queue()
        self.stop_event = threading.Event()
        self.last_activity = time.time()

        # Chat history for conversational context
        self.chat_history = zara.llm.ChatHistory(max_length=20)

        # LLM client (lazy init on first conversational query)
        self.llm_client = None
        self.llm_config = load_llm_config()

        # Initialize Prolog
        self.prolog = pyswip.Prolog()
        self.log("Initializing Prolog...")

        # Consult main.pl
        prolog_path = PROLOG_MAIN
        if not prolog_path.exists():
            # Try parent directory (for nix build)
            prolog_path = pathlib.Path(__file__).parent.parent / "main.pl"

        self.log(f"Consulting Prolog: {prolog_path}")
        self.prolog.consult(str(prolog_path))

        threads = os.cpu_count()
        self.log(f"Loading Whisper {model} (threads={threads})")
        self.log(f"LLM Provider: {self.llm_config['provider']}")

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

    def query_prolog(self, command_text):
        """
        Query Prolog intent resolver and executor.
        Returns: (needs_llm: bool, result: str)
        """
        try:
            # Query resolve/3 to get intent and args (module-qualified)
            query = f"intent_resolver:resolve(\"{command_text}\", Intent, Args)"
            self.log(f"Prolog query: {query}")

            results = list(self.prolog.query(query))

            if not results:
                # Resolution failed - route to LLM
                self.log("Prolog resolution failed - routing to LLM")
                return (True, "Resolution failed")

            result = results[0]
            intent = result["Intent"]
            args = result["Args"]

            self.log(f"Resolved: intent={intent}, args={args}")

            # Check if intent is "ask" (LLM fallback in Prolog)
            if intent == "ask":
                self.log("Intent is 'ask' - routing to LLM")
                return (True, "LLM fallback requested")

            # Execute the command via Prolog
            # Args is already a Python list from pyswip
            exec_query = f"commands:execute({intent}, {args})"
            self.log(f"Executing: {exec_query}")

            exec_results = list(self.prolog.query(exec_query))

            if exec_results is not None:
                return (False, f"Executed: {intent} {args}")
            else:
                return (False, f"Execution failed for {intent}")

        except Exception as e:
            self.log(f"Prolog error: {e}")
            # On error, route to LLM as fallback
            return (True, f"Prolog error: {str(e)}")

    def query_llm(self, query_text):
        """Query LLM with chat history for conversational responses"""
        try:
            # Lazy init LLM client on first use
            if self.llm_client is None:
                self.log(f"Initializing LLM client: {self.llm_config['provider']}")
                self.llm_client = zara.llm.LLMClient(
                    provider=self.llm_config['provider'],
                    model=self.llm_config['model'],
                    endpoint=self.llm_config['endpoint']
                )

            # Query with chat history
            response = self.llm_client.query(
                query_text,
                chat_history=self.chat_history.get_messages()
            )

            self.log(f"LLM response: {response}")
            return response

        except Exception as e:
            error_msg = f"LLM error: {str(e)}"
            self.log(error_msg)
            return error_msg

    def send_response_to_prolog(self, title, message):
        """Send response to Prolog for notification via alert module"""
        try:
            # Escape quotes in message
            escaped_msg = message.replace('"', '\\"').replace("'", "\\'")

            # Call alert:alert/3 - alert(Title, Format, Args)
            # Use ~w format for simple string display
            query = f'alert:alert("{title}", "~w", ["{escaped_msg}"])'
            self.log(f"Sending notification via Prolog: {title}")

            list(self.prolog.query(query))

        except Exception as e:
            self.log(f"Failed to send Prolog notification: {e}")
            # Fallback: log the message
            self.log(f"Response: {message[:200]}...")


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

        pathlib.Path(PIDFILE).unlink(missing_ok=True)
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
        """Transcribe and route command (Prolog first, LLM if needed)"""
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

        if not command:
            return

        # Query Prolog first - it will tell us if LLM is needed
        needs_llm, prolog_result = self.query_prolog(command)

        if needs_llm:
            # Prolog couldn't handle it - route to LLM
            self.log("Routing to LLM for conversational response")
            response = self.query_llm(command)

            # Add to chat history
            self.chat_history.add_user_message(command)
            self.chat_history.add_assistant_message(response)

            # Send response back to Prolog for notification
            self.send_response_to_prolog("Zara", response)

        else:
            # Prolog handled it - execution command
            self.log(f"Prolog executed: {prolog_result}")

            # Add to chat history
            self.chat_history.add_user_message(command)
            self.chat_history.add_assistant_message(prolog_result)

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
