from __future__ import annotations

import asyncio
import json
import os
import queue
import random
import shutil
import subprocess
import tempfile
import threading
import time
import uuid
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Any, Optional

from langchain_core.tools import StructuredTool
from pydantic import BaseModel, Field

from zara.config import get_config
from zara.plugins import PluginMetadata, ServicePlugin
from zara.plugins.builtin.agent_mode_hooks import (
    agent_mode_hooks,
    build_action_context,
)
from zara.runtime import events
from zara.runtime.commands import SubmitTurn
from zara.speech_activity import speech_activity
from zara.tts import TTSEngine


STATE_VERSION = 1
DEFAULT_POLL_SECONDS = 1.0
DEFAULT_QUESTION_MIN_MINUTES = 60.0
DEFAULT_QUESTION_MAX_MINUTES = 240.0
MAX_TASKS = 128
MAX_PROMPT_CHARS = 12000
MAX_SPEAK_CHARS = 4000


@dataclass(frozen=True)
class RecurringTask:
    task_id: str
    name: str
    prompt: str
    interval_seconds: float
    next_run_at: float
    enabled: bool = True
    created_personality_fingerprint: str = ""


class AgentModeStore:
    """Small private JSON state store for recurring tasks and proactive settings."""

    def __init__(self, path: Path, *, questions_enabled: bool = True) -> None:
        self.path = Path(path)
        self._lock = threading.RLock()
        self._questions_enabled_default = bool(questions_enabled)
        self._state = self._load()

    @property
    def questions_enabled(self) -> bool:
        with self._lock:
            return bool(self._state.get("questions_enabled", self._questions_enabled_default))

    def set_questions_enabled(self, enabled: bool) -> None:
        with self._lock:
            self._state["questions_enabled"] = bool(enabled)
            self._save_locked()

    def add_task(
        self,
        *,
        name: str,
        prompt: str,
        interval_seconds: float,
        start_delay_seconds: float = 0.0,
        personality_fingerprint: str = "",
        now: Optional[float] = None,
    ) -> RecurringTask:
        name = " ".join(str(name).split()).strip()
        prompt = str(prompt).strip()
        interval_seconds = float(interval_seconds)
        start_delay_seconds = max(0.0, float(start_delay_seconds))
        personality_fingerprint = str(personality_fingerprint).strip()[:64]
        if not name:
            raise ValueError("task name must not be empty")
        if not prompt:
            raise ValueError("task prompt must not be empty")
        if len(name) > 120:
            raise ValueError("task name must not exceed 120 characters")
        if len(prompt) > MAX_PROMPT_CHARS:
            raise ValueError(f"task prompt must not exceed {MAX_PROMPT_CHARS} characters")
        if interval_seconds < 1.0:
            raise ValueError("task interval must be at least one second")

        current = time.time() if now is None else float(now)
        task = RecurringTask(
            task_id=uuid.uuid4().hex,
            name=name,
            prompt=prompt,
            interval_seconds=interval_seconds,
            next_run_at=current + start_delay_seconds,
            created_personality_fingerprint=personality_fingerprint,
        )
        with self._lock:
            tasks = list(self._state.get("tasks", []))
            if len(tasks) >= MAX_TASKS:
                raise RuntimeError(f"agent mode supports at most {MAX_TASKS} recurring tasks")
            tasks.append(asdict(task))
            self._state["tasks"] = tasks
            self._save_locked()
        return task

    def list_tasks(self) -> list[RecurringTask]:
        with self._lock:
            return [RecurringTask(**dict(item)) for item in self._state.get("tasks", [])]

    def remove_task(self, task_id: str) -> bool:
        task_id = str(task_id).strip()
        if not task_id:
            return False
        with self._lock:
            tasks = list(self._state.get("tasks", []))
            kept = [item for item in tasks if item.get("task_id") != task_id]
            if len(kept) == len(tasks):
                return False
            self._state["tasks"] = kept
            self._save_locked()
            return True

    def claim_due(self, now: Optional[float] = None) -> list[RecurringTask]:
        """Claim due tasks and advance their next-run time before dispatch."""
        current = time.time() if now is None else float(now)
        due: list[RecurringTask] = []
        changed = False
        with self._lock:
            tasks = list(self._state.get("tasks", []))
            for item in tasks:
                task = RecurringTask(**dict(item))
                if not task.enabled or task.next_run_at > current:
                    continue
                due.append(task)
                missed = max(0.0, current - task.next_run_at)
                steps = int(missed // task.interval_seconds) + 1
                item["next_run_at"] = task.next_run_at + steps * task.interval_seconds
                changed = True
            if changed:
                self._state["tasks"] = tasks
                self._save_locked()
        return due

    def _load(self) -> dict[str, Any]:
        if not self.path.exists():
            return {
                "version": STATE_VERSION,
                "questions_enabled": self._questions_enabled_default,
                "tasks": [],
            }
        try:
            raw = json.loads(self.path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError) as error:
            raise RuntimeError(f"cannot read agent mode state: {error}") from error
        if raw.get("version") != STATE_VERSION:
            raise RuntimeError(
                f"unsupported agent mode state version {raw.get('version')!r}; expected {STATE_VERSION}"
            )
        if not isinstance(raw.get("tasks", []), list):
            raise RuntimeError("agent mode tasks state must be a list")
        return raw

    def _save_locked(self) -> None:
        self.path.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
        os.chmod(self.path.parent, 0o700)
        temporary = self.path.with_name(f".{self.path.name}.tmp")
        descriptor = os.open(temporary, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600)
        with os.fdopen(descriptor, "w", encoding="utf-8") as output:
            json.dump(self._state, output, indent=2, sort_keys=True)
            output.write("\n")
            output.flush()
            os.fsync(output.fileno())
        os.chmod(temporary, 0o600)
        os.replace(temporary, self.path)


class ScheduleRecurringTaskArgs(BaseModel):
    name: str = Field(..., description="Short stable name for the recurring task.")
    prompt: str = Field(..., description="Instruction Zara should execute on every run.")
    every_minutes: float = Field(..., ge=1.0 / 60.0, le=525600.0)
    start_delay_minutes: float = Field(0.0, ge=0.0, le=525600.0)


class CancelRecurringTaskArgs(BaseModel):
    task_id: str


class SpeakArgs(BaseModel):
    text: str = Field(..., description="Text Zara should speak aloud through the configured TTS provider.")


class ToggleQuestionsArgs(BaseModel):
    enabled: bool


class AgentModePlugin(ServicePlugin):
    """First-party autonomous mode: recurring work, proactive questions and TTS."""

    enabled_by_default = False
    metadata = PluginMetadata(
        name="agent-mode",
        version="0.2.0",
        api_version="1",
        description="Optional autonomous mode with recurring turns, proactive questions and TTS tools.",
    )

    def __init__(self) -> None:
        self._runtime = None
        self._subscription = None
        self._store: Optional[AgentModeStore] = None
        self._configuration: dict[str, Any] = {}
        self._speech_lock = threading.Lock()
        self._speech_state_lock = threading.RLock()
        self._speech_process: Optional[subprocess.Popen] = None
        self._speech_interrupt_reason = ""
        self._speech_activity_token = ""
        self._next_question_at: Optional[float] = None

    def start(self, runtime) -> None:
        self._runtime = runtime
        self._configuration = dict(runtime.configuration)
        self._store = AgentModeStore(
            self._state_path(self._configuration),
            questions_enabled=bool(self._configuration.get("random_questions", True)),
        )
        self._subscription = runtime.subscribe(maxsize=256)
        runtime.start_worker("scheduler", self._scheduler_loop)
        runtime.start_worker("events", self._event_loop)

    def stop(self) -> None:
        self._interrupt_speech("agent mode stopped")
        if self._subscription is not None:
            self._subscription.close()
            self._subscription = None
        self._runtime = None

    def tools(self):
        def schedule_recurring_task(
            name: str,
            prompt: str,
            every_minutes: float,
            start_delay_minutes: float = 0.0,
        ) -> str:
            context = build_action_context(
                "before_task_create",
                task_name=name,
                prompt=prompt,
            )
            context = agent_mode_hooks.run(context)
            store = self._require_store()
            task = store.add_task(
                name=context.task_name or name,
                prompt=context.prompt,
                interval_seconds=float(every_minutes) * 60.0,
                start_delay_seconds=float(start_delay_minutes) * 60.0,
                personality_fingerprint=context.personality_fingerprint,
            )
            return (
                f"Scheduled recurring task {task.name!r} with id {task.task_id}; "
                f"runs every {every_minutes:g} minute(s); personality context "
                f"{task.created_personality_fingerprint or 'default'} recorded."
            )

        def list_recurring_tasks() -> str:
            tasks = self._require_store().list_tasks()
            if not tasks:
                return "No recurring agent-mode tasks are scheduled."
            now = time.time()
            lines = []
            for task in tasks:
                due_in = max(0.0, task.next_run_at - now)
                lines.append(
                    f"- {task.task_id} | {task.name} | every {task.interval_seconds / 60.0:g}m "
                    f"| next in {due_in / 60.0:.1f}m | personality "
                    f"{task.created_personality_fingerprint or 'default'}"
                )
            return "\n".join(lines)

        def cancel_recurring_task(task_id: str) -> str:
            if self._require_store().remove_task(task_id):
                return f"Cancelled recurring task {task_id}."
            return f"No recurring task found with id {task_id}."

        def speak(text: str) -> str:
            return self._speak_text(text)

        def set_random_questions(enabled: bool) -> str:
            self._require_store().set_questions_enabled(enabled)
            self._next_question_at = None
            return f"Random proactive questions are {'enabled' if enabled else 'disabled'}."

        def agent_mode_status() -> str:
            store = self._require_store()
            tasks = store.list_tasks()
            current = build_action_context("before_proactive_question")
            return (
                "Agent mode is enabled. "
                f"Recurring tasks: {len(tasks)}. "
                f"Random questions: {'on' if store.questions_enabled else 'off'}. "
                f"Personality context: {current.personality_fingerprint}. "
                f"Barge-in: {'on' if self._barge_in_enabled() else 'off'}."
            )

        return (
            StructuredTool.from_function(
                schedule_recurring_task,
                name="schedule_recurring_task",
                description=(
                    "Schedule a persistent task for Zara to execute repeatedly in the background. "
                    "The current personality context is available to lifecycle hooks before storage."
                ),
                args_schema=ScheduleRecurringTaskArgs,
            ),
            StructuredTool.from_function(
                list_recurring_tasks,
                name="list_recurring_tasks",
                description="List persistent recurring tasks owned by agent mode.",
            ),
            StructuredTool.from_function(
                cancel_recurring_task,
                name="cancel_recurring_task",
                description="Cancel one persistent recurring agent-mode task by id.",
                args_schema=CancelRecurringTaskArgs,
            ),
            StructuredTool.from_function(
                speak,
                name="speak",
                description=(
                    "Speak text aloud with Zara's configured TTS provider. Microphone capture remains "
                    "active and confirmed user speech barges in to stop playback."
                ),
                args_schema=SpeakArgs,
            ),
            StructuredTool.from_function(
                set_random_questions,
                name="set_random_questions",
                description="Enable or disable agent mode's random proactive questions.",
                args_schema=ToggleQuestionsArgs,
            ),
            StructuredTool.from_function(
                agent_mode_status,
                name="agent_mode_status",
                description="Show autonomous-mode status, personality context and barge-in state.",
            ),
        )

    def _scheduler_loop(self, stop_event: threading.Event) -> None:
        poll_seconds = self._bounded_float(
            self._configuration.get("poll_seconds", DEFAULT_POLL_SECONDS),
            DEFAULT_POLL_SECONDS,
            minimum=0.1,
            maximum=60.0,
        )
        while not stop_event.is_set():
            now = time.time()
            store = self._require_store()
            for task in store.claim_due(now):
                conversation_id = f"agent-mode:task:{task.task_id}"
                context = build_action_context(
                    "before_task_run",
                    task_name=task.name,
                    task_id=task.task_id,
                    prompt=task.prompt,
                    conversation_id=conversation_id,
                    metadata={
                        "created_personality_fingerprint": task.created_personality_fingerprint,
                    },
                )
                context = agent_mode_hooks.run(context)
                self._dispatch_background_turn(
                    prompt=context.prompt,
                    conversation_id=conversation_id,
                )

            if store.questions_enabled:
                if self._next_question_at is None:
                    self._schedule_next_question(now)
                elif now >= self._next_question_at:
                    self._schedule_next_question(now)
                    conversation_id = f"agent-mode:question:{uuid.uuid4().hex}"
                    context = build_action_context(
                        "before_proactive_question",
                        prompt=self._question_prompt(),
                        conversation_id=conversation_id,
                    )
                    context = agent_mode_hooks.run(context)
                    self._dispatch_background_turn(
                        prompt=context.prompt,
                        conversation_id=conversation_id,
                    )
            else:
                self._next_question_at = None

            stop_event.wait(poll_seconds)

    def _event_loop(self, stop_event: threading.Event) -> None:
        subscription = self._subscription
        if subscription is None:
            return
        while not stop_event.is_set():
            try:
                envelope = subscription.get(timeout=0.25)
            except queue.Empty:
                continue
            except RuntimeError:
                return
            event = envelope.event

            if isinstance(event, events.VoiceSpeechStarted):
                if self._barge_in_enabled():
                    self._interrupt_speech("user speech detected")
                continue

            if not isinstance(event, events.ResponseText):
                continue
            conversation_id = str(event.conversation_id or "")
            if conversation_id.startswith("agent-mode:question:"):
                context = build_action_context(
                    "after_proactive_question",
                    text=event.text,
                    conversation_id=conversation_id,
                )
                context = agent_mode_hooks.run(context)
                if bool(self._configuration.get("speak_questions", True)):
                    self._speak_text(context.text)
            elif conversation_id.startswith("agent-mode:task:"):
                task_id = conversation_id.rsplit(":", 1)[-1]
                context = build_action_context(
                    "after_task_result",
                    task_id=task_id,
                    text=event.text,
                    conversation_id=conversation_id,
                )
                context = agent_mode_hooks.run(context)
                if bool(self._configuration.get("speak_task_results", False)):
                    self._speak_text(context.text)

    def _dispatch_background_turn(self, *, prompt: str, conversation_id: str) -> None:
        runtime = self._runtime
        if runtime is None:
            return
        try:
            future = runtime.dispatch(
                SubmitTurn(text=prompt, conversation_id=conversation_id)
            )
        except Exception:
            return

        def consume_failure(completed) -> None:
            try:
                completed.result()
            except Exception:
                return

        future.add_done_callback(consume_failure)

    def _schedule_next_question(self, now: float) -> None:
        minimum = self._bounded_float(
            self._configuration.get("question_min_minutes", DEFAULT_QUESTION_MIN_MINUTES),
            DEFAULT_QUESTION_MIN_MINUTES,
            minimum=0.1,
            maximum=10080.0,
        )
        maximum = self._bounded_float(
            self._configuration.get("question_max_minutes", DEFAULT_QUESTION_MAX_MINUTES),
            DEFAULT_QUESTION_MAX_MINUTES,
            minimum=minimum,
            maximum=10080.0,
        )
        self._next_question_at = now + random.uniform(minimum, maximum) * 60.0

    def _question_prompt(self) -> str:
        configured = str(self._configuration.get("question_prompt", "")).strip()
        if configured:
            return configured[:MAX_PROMPT_CHARS]
        return (
            "You are running in Zara agent mode. Proactively ask the user exactly one brief, "
            "natural question that fits your current personality and would help you learn a useful "
            "preference, clarify an active project, discover something worth remembering, or help "
            "with their current goals. Use memory only if useful. Do not explain why you are asking. "
            "Return only the question."
        )

    def _speak_text(self, text: str) -> str:
        text = str(text).strip()
        if not text:
            return "Nothing to speak."

        context = build_action_context("before_speak", text=text)
        context = agent_mode_hooks.run(context)
        text = str(context.text).strip()
        if not text:
            return "Speech suppressed by an agent-mode hook."
        if len(text) > MAX_SPEAK_CHARS:
            return f"Refusing to speak more than {MAX_SPEAK_CHARS} characters at once."
        if shutil.which("mpv") is None:
            return "TTS synthesis is available, but mpv is not installed for playback."

        with self._speech_lock:
            try:
                config = get_config()
                tts_config = dict(config.get_section("tts") or {})
                provider = str(tts_config.get("provider", "qwen3"))
                engine = TTSEngine(provider, {"tts": tts_config})
                try:
                    result = asyncio.run(engine.synthesize_async(text))
                finally:
                    asyncio.run(engine.close())
            except Exception as error:
                return self._after_speak(text, f"TTS failed: {error}")

            if not result.success:
                return self._after_speak(
                    text,
                    f"TTS failed: {result.error or 'unknown synthesis error'}",
                )

            suffix = ".mp3" if result.audio_format == "mp3" else ".wav"
            path = None
            activity = None
            process = None
            try:
                with tempfile.NamedTemporaryFile(suffix=suffix, delete=False) as output:
                    output.write(result.audio)
                    path = output.name

                activity = speech_activity.begin(source="agent-mode")
                process = subprocess.Popen(
                    [
                        "mpv",
                        "--no-video",
                        "--audio-display=no",
                        "--really-quiet",
                        "--no-terminal",
                        path,
                    ],
                    stdin=subprocess.DEVNULL,
                    stdout=subprocess.DEVNULL,
                    stderr=subprocess.PIPE,
                    text=True,
                )
                with self._speech_state_lock:
                    self._speech_process = process
                    self._speech_interrupt_reason = ""
                    self._speech_activity_token = activity.token

                try:
                    _, stderr = process.communicate(timeout=120)
                except subprocess.TimeoutExpired:
                    process.terminate()
                    try:
                        _, stderr = process.communicate(timeout=2)
                    except subprocess.TimeoutExpired:
                        process.kill()
                        _, stderr = process.communicate()
                    return self._after_speak(text, "TTS playback timed out.")

                with self._speech_state_lock:
                    interrupted = self._speech_interrupt_reason
                if interrupted:
                    return self._after_speak(text, f"Speech interrupted: {interrupted}.")
                if process.returncode != 0:
                    detail = " ".join((stderr or "").split())[:300]
                    return self._after_speak(
                        text,
                        f"TTS playback failed: {detail or process.returncode}",
                    )
                return self._after_speak(text, "Spoken.")
            except Exception as error:
                return self._after_speak(text, f"TTS playback failed: {error}")
            finally:
                with self._speech_state_lock:
                    if self._speech_process is process:
                        self._speech_process = None
                        self._speech_interrupt_reason = ""
                        self._speech_activity_token = ""
                if activity is not None:
                    speech_activity.end(activity.token)
                if path:
                    try:
                        os.unlink(path)
                    except OSError:
                        pass

    def _after_speak(self, text: str, result: str) -> str:
        context = build_action_context(
            "after_speak",
            text=text,
            metadata={"result": result},
        )
        context = agent_mode_hooks.run(context)
        return str(context.metadata.get("result", result))

    def _interrupt_speech(self, reason: str) -> bool:
        with self._speech_state_lock:
            process = self._speech_process
            if process is None or process.poll() is not None:
                return False
            self._speech_interrupt_reason = str(reason or "barge-in")[:200]
            try:
                process.terminate()
            except OSError:
                return False
            return True

    def _barge_in_enabled(self) -> bool:
        return bool(self._configuration.get("barge_in", True))

    def _require_store(self) -> AgentModeStore:
        if self._store is None:
            raise RuntimeError("agent mode is not running")
        return self._store

    @staticmethod
    def _state_path(configuration: dict[str, Any]) -> Path:
        configured = str(configuration.get("state_path", "")).strip()
        if configured:
            return Path(os.path.expanduser(os.path.expandvars(configured)))
        state_root = os.environ.get("XDG_STATE_HOME", "").strip()
        root = Path(state_root).expanduser() if state_root else Path.home() / ".local" / "state"
        return root / "zarathushtra" / "agent-mode" / "state.json"

    @staticmethod
    def _bounded_float(value, default: float, *, minimum: float, maximum: float) -> float:
        try:
            parsed = float(value)
        except (TypeError, ValueError):
            parsed = float(default)
        return min(max(parsed, minimum), maximum)


def create_plugin():
    return AgentModePlugin()
