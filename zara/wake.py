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
from typing import Any, List, Optional, Tuple
import numpy as np
import sounddevice as sd
import faster_whisper
from concurrent.futures import ThreadPoolExecutor

# Import LLM client and ChatHistory
from . import llm
from .tts import PlaybackResult, TTSEngine
from .agent import AgentManager
from .config import get_config
from .notifications import send_notification_async
from .audio import resolve_input_sample_rate, resample_audio
from .prolog_engine import PrologEngine
from .python_skills import python_skills
from .memory import build_memory_manager

DEFAULT_SAMPLE_RATE = 16000
CHANNELS = 1
PASSIVE_CHUNK_SEC = 3
ACTIVE_CHUNK_SEC = 8
DEFAULT_SILENCE_THRESHOLD = 0.01  # RMS threshold for silence detection
DEFAULT_SILENCE_DURATION = 5.0  # Seconds of silence before considering speech complete
MAX_RECORDING_DURATION = 30.0  # Maximum recording duration to prevent infinite recording
WAKE_WORDS = ["zarathustra", "hey zara", "zara", "sarah"]
TIMEOUT_ACTIVE = 5

PIDFILE = "/tmp/zara_wakeword.pid"
LOGFILE = "/tmp/zara_wakeword.log"




def load_tts_config():
    """Load TTS configuration from config system"""
    config = get_config()
    tts_cfg = config.get_section("tts")

    provider = tts_cfg.get("provider", "qwen3")
    qwen3_url = tts_cfg.get("endpoint", os.getenv("QWEN3_TTS_URL", "http://localhost:7860"))
    qwen3_voice = tts_cfg.get("voice", os.getenv("QWEN3_VOICE", "demo_speaker0"))

    return {
        "provider": provider,
        "qwen3_url": qwen3_url,
        "qwen3_voice": qwen3_voice
    }


class WakeWordListener:
    def __init__(self, model="tiny.en", device="cpu", prolog_main_path=None, enable_tts=True):
        self.state = "PASSIVE"
        self.audio_queue: Optional[asyncio.Queue] = None  # Will be created in async context
        self.stop_event: Optional[asyncio.Event] = None  # Will be created in async context
        self.last_activity = time.time()
        self.enable_tts = enable_tts
        self.loop: Optional[asyncio.AbstractEventLoop] = None  # Event loop reference for audio callback

        # Thread pool for CPU-bound operations
        self.executor = ThreadPoolExecutor(max_workers=4)

        # Chat history for conversational context
        self.chat_history = llm.ChatHistory(max_length=20)

        # LLM client (lazy init on first conversational query)
        self.llm_client = None
        # Get config from config system
        self.config = get_config()
        self.llm_config = self.config.get_llm_config()

        # Agent manager for conversation mode (lazy init)
        self.agent_manager: Optional[AgentManager] = None

        # Memory manager for sessions
        memory_config = self.config.get_section("memory")
        self.memory = build_memory_manager(memory_config)
        self.session_id: Optional[str] = self.memory.start_session()

        # TTS client (lazy init on first use)
        self.tts_client = None
        self.tts_config = load_tts_config()
        self.tts_task: Optional[asyncio.Task] = None
        self.tts_stop_event: Optional[asyncio.Event] = None
        self.tts_player_proc = None
        self.tts_lock = asyncio.Lock()
        self.last_tts_status: Optional[PlaybackResult] = None

        wake_cfg = self.config.get_section("wake")
        self.stop_on_interrupt = bool(wake_cfg.get("stop_tts_on_input", True))
        self.silence_duration = self._parse_float(
            wake_cfg.get("silence_duration", DEFAULT_SILENCE_DURATION),
            DEFAULT_SILENCE_DURATION,
        )
        self.silence_threshold = self._parse_float(
            wake_cfg.get("silence_threshold", DEFAULT_SILENCE_THRESHOLD),
            DEFAULT_SILENCE_THRESHOLD,
        )
        self.silence_log_interval = self._parse_float(
            wake_cfg.get("silence_log_interval", 0.5),
            0.5,
        )

        self.target_sample_rate = self._parse_float(
            wake_cfg.get("sample_rate", DEFAULT_SAMPLE_RATE),
            DEFAULT_SAMPLE_RATE,
        )
        self.input_sample_rate, rate_note = resolve_input_sample_rate(
            self.target_sample_rate,
            channels=CHANNELS,
        )
        if rate_note:
            self.log(rate_note)

        # Initialize Prolog engine
        self.log("Initializing Prolog...")

        candidates = [
            pathlib.Path.cwd() / "main.pl",
            pathlib.Path(__file__).parent.parent / "main.pl",
            pathlib.Path("/usr/share/zarathushtra/main.pl"),
        ]

        if prolog_main_path:
            prolog_path = pathlib.Path(prolog_main_path)
        else:
            prolog_path = None
            for candidate in candidates:
                if candidate.exists():
                    prolog_path = candidate
                    break

        if not prolog_path or not prolog_path.exists():
            raise FileNotFoundError(f"Could not find main.pl. Tried: {candidates}")

        self.log(f"Consulting Prolog: {prolog_path}")
        self.prolog = PrologEngine(prolog_path)
        self.log("Prolog engine ready")

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

    def _parse_float(self, value: Any, default: float) -> float:
        try:
            return float(value)
        except (TypeError, ValueError):
            return default

    def _is_conversation_stop(self, text: str) -> bool:
        state = "conversation" if self.in_conversation_mode() else "passive"
        return self.prolog.is_conversation_stop(text, state=state)

    async def _stop_tts(self):
        if self.tts_stop_event is not None:
            self.tts_stop_event.set()
        if self.tts_player_proc and self.tts_player_proc.returncode is None:
            self.tts_player_proc.kill()
            await self.tts_player_proc.wait()
        if self.tts_task and not self.tts_task.done():
            self.tts_task.cancel()
            try:
                await self.tts_task
            except asyncio.CancelledError:
                pass
        self.tts_task = None
        self.tts_stop_event = None
        self.tts_player_proc = None

    def audio_callback(self, indata, _frames, _time_info, status):
        """Audio callback - runs in separate thread, needs thread-safe queue push"""
        if status:
            self.log(f"Audio: {status}")

        # Put audio in queue in a thread-safe way
        # We need to use the stored loop reference since this runs in a different thread
        if self.loop and self.audio_queue:
            asyncio.run_coroutine_threadsafe(
                self.audio_queue.put(indata.copy()),
                self.loop
            )

    async def transcribe_async(self, audio_data):
        """Run Whisper transcription in thread pool to avoid blocking"""
        loop = asyncio.get_event_loop()

        def _transcribe():
            if audio_data.ndim > 1:
                audio_data_mono = audio_data[:, 0]
            else:
                audio_data_mono = audio_data

            if self.input_sample_rate != self.target_sample_rate:
                audio_data_mono = resample_audio(
                    audio_data_mono,
                    self.input_sample_rate,
                    self.target_sample_rate,
                )

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
        target_frames = int(seconds * self.input_sample_rate)
        buffer = np.zeros((0, CHANNELS), dtype="float32")

        while buffer.shape[0] < target_frames and not self.stop_event.is_set():
            try:
                data = await asyncio.wait_for(self.audio_queue.get(), timeout=0.5)
                buffer = np.concatenate((buffer, data))
            except asyncio.TimeoutError:
                continue

        return buffer[:target_frames] if buffer.shape[0] >= target_frames else None

    async def collect_audio_until_silence(self):
        """Collect audio until silence is detected (for active mode)"""
        buffer = np.zeros((0, CHANNELS), dtype="float32")
        silence_start = None
        total_duration = 0.0
        speech_detected = False
        last_log_time = 0.0

        while not self.stop_event.is_set():
            try:
                data = await asyncio.wait_for(self.audio_queue.get(), timeout=0.5)
                buffer = np.concatenate((buffer, data))

                # Calculate RMS of the current chunk to detect silence
                chunk_mono = data[:, 0] if data.ndim > 1 else data
                rms = np.sqrt(np.mean(chunk_mono.astype(np.float32) ** 2))

                # Update total duration
                chunk_duration = len(data) / self.input_sample_rate
                total_duration += chunk_duration

                # Check if we've exceeded max recording duration
                if total_duration > MAX_RECORDING_DURATION:
                    self.log(f"Max recording duration ({MAX_RECORDING_DURATION}s) reached, transcribing")
                    break

                now = time.time()
                if now - last_log_time >= self.silence_log_interval:
                    if speech_detected and silence_start is not None:
                        elapsed = now - silence_start
                        self.log(
                            f"Silence check: rms={rms:.4f} threshold={self.silence_threshold:.3f} "
                            f"elapsed={elapsed:.2f}s"
                        )
                    else:
                        self.log(
                            f"Silence check: rms={rms:.4f} threshold={self.silence_threshold:.3f}"
                        )
                    last_log_time = now

                # Detect speech vs silence
                if rms > self.silence_threshold:
                    speech_detected = True
                    silence_start = None
                else:
                    if not speech_detected:
                        continue
                    if silence_start is None:
                        silence_start = time.time()
                    else:
                        # Check if we've had enough silence
                        silence_elapsed = time.time() - silence_start
                        if silence_elapsed >= self.silence_duration:
                            self.log(
                                f"Silence detected after {silence_elapsed:.2f}s, transcribing"
                            )
                            break

            except asyncio.TimeoutError:
                if not speech_detected:
                    continue
                # Timeout counts as silence only after speech
                if silence_start is None:
                    silence_start = time.time()
                else:
                    silence_elapsed = time.time() - silence_start
                    if silence_elapsed >= self.silence_duration:
                        self.log(f"Audio timeout after {silence_elapsed:.2f}s silence")
                        break
                continue

        # Return buffer if we have enough audio
        if buffer.shape[0] > self.input_sample_rate * 0.5:  # At least 0.5 seconds
            return buffer
        return None

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

    def in_conversation_mode(self) -> bool:
        """Check if currently in conversation mode"""
        if self.agent_manager is not None:
            return self.agent_manager.conversation_manager.in_conversation
        return False

    def _apply_conversation_grace(self):
        if self.agent_manager is None:
            return
        if not self.in_conversation_mode():
            return
        agent_config = self.config.get_section("agent")
        configured_grace = agent_config.get("post_tts_silence_seconds", 0.0)
        try:
            configured_grace = float(configured_grace)
        except (TypeError, ValueError):
            configured_grace = 0.0
        grace_seconds = max(self.silence_duration, configured_grace)
        self.agent_manager.conversation_manager.update_activity(grace_seconds=grace_seconds)
        self.log(f"Conversation grace window: {grace_seconds:.1f}s")

    def _ensure_agent_manager(self):
        """Lazy initialize agent manager"""
        if self.agent_manager is None:
            self.log("Initializing agent manager for conversation mode")
            config = get_config()
            self.agent_manager = AgentManager(
                config=config,
                prolog_engine=self.prolog,
                memory_manager=self.memory,
            )

    async def speak_async(self, text):
        """Speak text via TTS if enabled"""
        if self.enable_tts:
            await self.synthesize_and_play_async(text)

    async def query_with_fallback_async(self, command_text: str) -> Tuple[bool, str]:
        """
        Try Prolog first, fallback to conversation mode if:
        1. Prolog resolution fails, OR
        2. Already in conversation mode

        Returns: (used_agent_mode, response)
        """
        # Try Prolog first
        self.log("Attempting Prolog resolution before agent fallback")
        loop = asyncio.get_event_loop()
        resolution_state = "conversation" if self.in_conversation_mode() else "passive"

        def _try_prolog():
            try:
                result = self.prolog.resolve_intent(command_text, state=resolution_state)
                self.log(f"Command got: {command_text}")
                if not result:
                    self.log("Prolog resolution failed")
                    return (False, "", True)

                intent = result.name
                args = result.args

                self.log(f"Resolved: intent={intent}, args={args}")

                if intent == "end_conversation":
                    self.log("Stop intent resolved by Prolog")
                    return (False, "", False)

                if intent == "ask":
                    self.log("Intent is 'ask' - needs conversation")
                    return (False, "", True)

                if result.kind == "pending":
                    required = ", ".join(str(slot) for slot in args)
                    return (True, f"Please provide: {required}.", False)

                if result.kind == "python":
                    skill_name = intent
                    if self.session_id is None:
                        self.session_id = self.memory.start_session()
                    skill_args = list(args) if isinstance(args, list) else [args]
                    response = python_skills.execute(skill_name, skill_args)
                    self.memory.add_message(self.session_id, "user", command_text)
                    self.memory.add_message(self.session_id, "assistant", response)
                    return (True, response, False)

                self.log(f"Executing intent={intent}, args={args}")
                if self.prolog.execute_intent(intent, args):
                    response_text = f"Executed: {intent} {args}"
                    if self.session_id is None:
                        self.session_id = self.memory.start_session()
                    self.memory.add_message(self.session_id, "user", command_text)
                    self.memory.add_message(self.session_id, "assistant", response_text)
                    return (True, response_text, False)
                return (False, "", True)

            except Exception as e:
                self.log(f"Prolog error: {e}")
                return (False, "", True)

        prolog_success, prolog_result, needs_agent = await loop.run_in_executor(self.executor, _try_prolog)

        if prolog_success:
            # Prolog handled it successfully
            return (False, prolog_result)

        if not needs_agent:
            return (False, "")

        # Prolog failed or asked for conversation
        self._ensure_agent_manager()
        if self.agent_manager is None:
            return (True, "")

        if not self.in_conversation_mode():
            self.log("Entering conversation mode after Prolog")
            self.agent_manager.conversation_manager.enter_conversation()
            self.agent_manager.conversation_manager.conversation_history.clear()

        self.log("Using agent after Prolog fallback")
        self.log(f"Conversation history has {len(self.agent_manager.conversation_manager.conversation_history)} messages")
        self.log(f"Voice input to LLM (fallback): {command_text!r}")
        self.log(f"Voice input length (fallback): {len(command_text)}")
        result = await self.agent_manager.process_async(command_text)
        response_text = result.get("response", "")
        if self.session_id is None:
            self.session_id = self.memory.start_session()
        self.memory.add_message(self.session_id, "user", command_text)
        if response_text:
            self.memory.add_message(self.session_id, "assistant", response_text)
        return (True, response_text)

    async def query_llm_async(self, query_text):
        """Query LLM with chat history for conversational responses"""
        try:
            if self.llm_client is None:
                self.log(f"Initializing LLM client: {self.llm_config['provider']}")
                self.llm_client = llm.LLMClient(
                    provider=self.llm_config['provider'],
                    model=self.llm_config['model'],
                    endpoint=self.llm_config['endpoint'],
                    api_key=(
                        self.llm_config["anthropic_api_key"]
                        if self.llm_config["provider"] == "anthropic"
                        else self.llm_config["openai_api_key"]
                    ),
                    connect_timeout=self.llm_config["connect_timeout"],
                    read_timeout=self.llm_config["read_timeout"],
                    total_timeout=self.llm_config["total_timeout"],
                    max_retries=self.llm_config["max_retries"],
                    history_limit=self.llm_config["history_limit"],
                )

            result = await self.llm_client.query_async(
                query_text,
                chat_history=self.chat_history.get_messages()
            )
            if not result.success:
                self.log(
                    f"LLM request failed ({result.error_type}): {result.error}"
                )
                if result.cancelled:
                    return ""
                return f"The LLM request failed: {result.error}"

            self.chat_history.add_user_message(query_text)
            self.chat_history.add_assistant_message(result.text)
            self.log(f"LLM response: {result.text}")
            return result.text

        except Exception as e:
            error_msg = f"LLM error: {str(e)}"
            self.log(error_msg)
            return error_msg

    async def synthesize_and_play_async(self, text):
        """Replace any active utterance and start the new one."""
        async with self.tts_lock:
            await self._stop_tts()
            self.tts_stop_event = asyncio.Event()
            self.tts_task = asyncio.create_task(
                self._synthesize_and_play_task(text, self.tts_stop_event)
            )
        self.log("TTS started (non-blocking)")
        return self.tts_task

    async def _synthesize_and_play_task(
        self,
        text,
        stop_event: asyncio.Event,
    ) -> PlaybackResult:
        try:
            if self.tts_client is None:
                self.log(f"Initializing TTS client: {self.tts_config['provider']}")
                config_dict = self.config._config if hasattr(self.config, '_config') else {}
                self.tts_client = TTSEngine(provider=self.tts_config['provider'], config=config_dict)

            if stop_event.is_set():
                status = PlaybackResult(
                    provider=self.tts_config["provider"],
                    success=False,
                    error="Playback cancelled",
                    cancelled=True,
                )
            else:
                self.log("Synthesizing speech...")
                synthesis = await self.tts_client.synthesize_async(text)
                if not synthesis.success:
                    status = PlaybackResult(
                        provider=synthesis.provider,
                        success=False,
                        error=synthesis.error,
                        cancelled=synthesis.cancelled,
                    )
                elif stop_event.is_set():
                    status = PlaybackResult(
                        provider=synthesis.provider,
                        success=False,
                        error="Playback cancelled",
                        cancelled=True,
                    )
                else:
                    self.log("Playing audio...")
                    status = await self._play_audio_task(
                        synthesis.audio,
                        synthesis.audio_format,
                        stop_event,
                    )

        except asyncio.CancelledError:
            status = PlaybackResult(
                provider=self.tts_config["provider"],
                success=False,
                error="Playback cancelled",
                cancelled=True,
            )
        except Exception as e:
            status = PlaybackResult(
                provider=self.tts_config["provider"],
                success=False,
                error=str(e),
            )
        self.last_tts_status = status
        if status.success:
            self.log("Audio playback complete")
        else:
            self.log(f"TTS failed: {status.error}")
        return status

    async def _play_audio_task(
        self,
        audio_bytes: bytes,
        audio_format: Optional[str],
        stop_event: asyncio.Event,
    ) -> PlaybackResult:
        provider = self.tts_config["provider"]
        if not audio_bytes or audio_format not in {"wav", "mp3"}:
            return PlaybackResult(
                provider=provider,
                success=False,
                error="Audio is empty or has an unsupported format",
            )

        try:
            if audio_format == "mp3":
                return await self._play_mp3(audio_bytes, stop_event)

            import soundfile as sf
            import io

            loop = asyncio.get_running_loop()

            def _play():
                audio_io = io.BytesIO(audio_bytes)
                data, samplerate = sf.read(audio_io)
                sd.play(data, samplerate)
                while sd.get_stream().active:
                    if stop_event.is_set():
                        sd.stop()
                        break
                    time.sleep(0.05)

            await loop.run_in_executor(self.executor, _play)
            if stop_event.is_set():
                return PlaybackResult(
                    provider=provider,
                    success=False,
                    error="Playback cancelled",
                    cancelled=True,
                )
            return PlaybackResult(provider=provider, success=True)
        except asyncio.CancelledError:
            return PlaybackResult(
                provider=provider,
                success=False,
                error="Playback cancelled",
                cancelled=True,
            )
        except Exception as e:
            return PlaybackResult(provider=provider, success=False, error=str(e))
        finally:
            self._apply_conversation_grace()

    async def _play_mp3(
        self,
        audio_bytes: bytes,
        stop_event: asyncio.Event,
    ) -> PlaybackResult:
        provider = self.tts_config["provider"]
        process = await asyncio.create_subprocess_exec(
            "mpv",
            "--no-video",
            "--no-terminal",
            "--really-quiet",
            "-",
            stdin=asyncio.subprocess.PIPE,
            stdout=asyncio.subprocess.DEVNULL,
            stderr=asyncio.subprocess.PIPE,
        )
        self.tts_player_proc = process
        playback = asyncio.create_task(process.communicate(audio_bytes))
        stopped = asyncio.create_task(stop_event.wait())
        try:
            done, _ = await asyncio.wait(
                {playback, stopped},
                return_when=asyncio.FIRST_COMPLETED,
            )
            if stopped in done:
                if process.returncode is None:
                    process.kill()
                    await process.wait()
                playback.cancel()
                await asyncio.gather(playback, return_exceptions=True)
                return PlaybackResult(
                    provider=provider,
                    success=False,
                    error="Playback cancelled",
                    cancelled=True,
                )
            _, stderr = await playback
            if process.returncode != 0:
                detail = stderr.decode(errors="replace").strip()
                return PlaybackResult(
                    provider=provider,
                    success=False,
                    error=detail or f"mpv exited with {process.returncode}",
                )
            return PlaybackResult(provider=provider, success=True)
        finally:
            stopped.cancel()
            await asyncio.gather(stopped, return_exceptions=True)
            if process.returncode is None:
                process.kill()
                await process.wait()
            if self.tts_player_proc is process:
                self.tts_player_proc = None

    async def send_response_async(self, title, message):
        """Send response via notification and optionally TTS"""
        try:
            safe_message = message.strip() if message else ""
            if not safe_message:
                safe_message = "No response."

            success = await send_notification_async(title, safe_message, urgency="normal")
            if success:
                self.log(f"Sent notification: {title}")
            else:
                self.log(f"Notification failed for: {title}")
                self.log(f"Notification message: {safe_message[:200]}")

            if self.enable_tts:
                await self.synthesize_and_play_async(safe_message)

        except Exception as e:
            self.log(f"Failed to send response: {e}")
            self.log(f"Response: {message[:200]}...")

    async def run_async(self):
        """Main async event loop"""
        # Store event loop reference for audio callback
        self.loop = asyncio.get_event_loop()

        # Create async primitives in the async context
        self.audio_queue = asyncio.Queue()
        self.stop_event = asyncio.Event()

        self.log("🔥 Starting Wake Word Listener (ASYNC)")
        self.log(f"Wake words: {', '.join(WAKE_WORDS)}")

        with open(PIDFILE, "w") as f:
            f.write(str(os.getpid()))

        with sd.InputStream(
            samplerate=self.input_sample_rate,
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
        if self.prolog.dictation_active():
            self.state = "ACTIVE"
            return

        chunk = await self.collect_audio(PASSIVE_CHUNK_SEC)
        if chunk is None:
            return

        text = await self.transcribe_async(chunk)

        if self.check_wake_word(text):
            await send_notification_async("Zara", "Listening", "normal", 1000)
            self.log(f"🔥 Wake word detected")
            await self.clear_queue()
            self.state = "ACTIVE"
            self.last_activity = time.time()

    async def active_mode_async(self):
        """Transcribe and route command with conversation mode support"""
        # Check conversation timeout
        if self.in_conversation_mode():
            if self.agent_manager is not None and self.agent_manager.should_exit_conversation():
                self.log("Conversation timeout, exiting conversation mode")
                if self.session_id is not None:
                    summary = self.memory.summarise_session(self.session_id, source="wake")
                    if summary:
                        self.log("Conversation summary stored")
                self.session_id = self.memory.start_session()
                self.agent_manager.exit_conversation()
                if self.enable_tts:
                    await self.speak_async("Conversation ended")
                self.state = "PASSIVE"
                return

        # If dictation is active, only watch for stop phrases
        if self.prolog.dictation_active():
            chunk = await self.collect_audio_until_silence()
            if chunk is None:
                return

            text = await self.transcribe_async(chunk)
            if not text or len(text) < 2:
                return

            self.log(f"📝 Dictation heard: '{text}'")
            intent = self.prolog.resolve_intent(text, state="dictation")
            if intent and intent.kind == "prolog" and intent.name == "dictation_stop":
                self.log("Dictation stop detected while active")
                self.prolog.query_once("dictation:stop_dictation")
                self.state = "PASSIVE"
            return

        # Check general activity timeout
        if time.time() - self.last_activity > TIMEOUT_ACTIVE:
            self.log("⏸️ Timeout - returning to passive")
            self.state = "PASSIVE"
            return

        # Use silence detection for more natural recording
        chunk = await self.collect_audio_until_silence()
        if chunk is None:
            return

        text = await self.transcribe_async(chunk)

        if not text or len(text) < 3:
            return

        if self.stop_on_interrupt:
            await self._stop_tts()

        self.log(f"📝 Heard: '{text}'")
        self.last_activity = time.time()

        text_lower = text.lower()

        # Check for stop phrases
        if self._is_conversation_stop(text_lower):
            await self._stop_tts()
            if self.in_conversation_mode():
                self.log("Exiting conversation mode")
                if self.session_id is not None:
                    summary = self.memory.summarise_session(self.session_id, source="wake")
                    if summary:
                        self.log("Conversation summary stored")
                self.session_id = self.memory.start_session()
                if self.agent_manager is not None:
                    self.agent_manager.exit_conversation()
                if self.enable_tts:
                    await self.speak_async("Conversation ended")
                self.state = "PASSIVE"
                return
            self.log("Returning to passive mode")
            self.state = "PASSIVE"
            return

        command = text.strip()

        if not command:
            return

        # Process with Prolog-first fallback
        used_agent, response = await self.query_with_fallback_async(command)

        if not response:
            if used_agent:
                return
            if self.in_conversation_mode():
                self.log("Exiting conversation mode")
                if self.session_id is not None:
                    summary = self.memory.summarise_session(self.session_id, source="wake")
                    if summary:
                        self.log("Conversation summary stored")
                self.session_id = self.memory.start_session()
                if self.agent_manager is not None:
                    self.agent_manager.exit_conversation()
                if self.enable_tts:
                    await self.speak_async("Conversation ended")
            self.state = "PASSIVE"
            return

        await self.send_response_async("Zara", response)

        # Reset activity timeout
        self.last_activity = time.time()


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
            if listener.llm_client:
                await listener.llm_client.close()
            if listener.tts_client:
                await listener.tts_client.close()

    try:
        asyncio.run(run_with_cleanup())
    except KeyboardInterrupt:
        pass

    return 0
