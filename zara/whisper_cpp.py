"""Persistent whisper.cpp speech-to-text backend for Zara.

The adapter keeps ``whisper-server`` alive so the model remains resident in
GPU memory between Zara wake/dictation requests. Zara still owns microphone
capture and VAD segmentation; whisper.cpp receives only the final 16 kHz WAV
chunk selected for transcription.
"""

from __future__ import annotations

import atexit
from contextlib import contextmanager
import fcntl
import io
import json
import logging
import os
import shutil
import socket
import subprocess
import tempfile
import threading
import time
from pathlib import Path
from types import SimpleNamespace
from typing import Callable

import httpx
import numpy as np
import soundfile as sf


WHISPER_CPP_MODELS = {
    "tiny",
    "tiny.en",
    "tiny-q5_1",
    "tiny.en-q5_1",
    "tiny-q8_0",
    "base",
    "base.en",
    "base-q5_1",
    "base.en-q5_1",
    "base-q8_0",
    "small",
    "small.en",
    "small-q5_1",
    "small.en-q5_1",
    "small-q8_0",
    "medium",
    "medium.en",
    "medium-q5_0",
    "medium.en-q5_0",
    "medium-q8_0",
    "large-v1",
    "large-v2",
    "large-v2-q5_0",
    "large-v2-q8_0",
    "large-v3",
    "large-v3-q5_0",
    "large-v3-turbo",
    "large-v3-turbo-q5_0",
    "large-v3-turbo-q8_0",
}
MODEL_BASE_URL = "https://huggingface.co/ggerganov/whisper.cpp/resolve/main"
DOWNLOAD_REPORT_BYTES = 64 * 1024 * 1024
logger = logging.getLogger(__name__)
DEVICE_LOSS_MARKERS = ("device lost", "vk_error_device_lost", "gpu recovery failed")


def _worker_state_path() -> Path:
    runtime = os.getenv("XDG_RUNTIME_DIR")
    root = Path(runtime) if runtime else Path.home() / ".local" / "state"
    return root / "zara" / "whisper-worker.json"


def _worker_identity(pid: int) -> str | None:
    try:
        stat = Path(f"/proc/{pid}/stat").read_text()
    except (OSError, ValueError):
        return None
    return stat.rsplit(") ", 1)[1].split()[19]


@contextmanager
def _locked_worker_state():
    path = _worker_state_path()
    path.parent.mkdir(mode=0o700, parents=True, exist_ok=True)
    descriptor = os.open(path, os.O_CREAT | os.O_RDWR, 0o600)
    with os.fdopen(descriptor, "r+") as state_file:
        fcntl.flock(state_file, fcntl.LOCK_EX)
        try:
            contents = state_file.read()
            try:
                state = json.loads(contents) if contents else {}
            except ValueError as error:
                raise RuntimeError("whisper.cpp worker state unreadable") from error
            if not isinstance(state, dict):
                raise RuntimeError("whisper.cpp worker state unreadable")
            original = state.copy()
            yield state
            if state != original:
                state_file.seek(0)
                json.dump(state, state_file)
                state_file.truncate()
                state_file.flush()
                os.fsync(state_file.fileno())
        finally:
            fcntl.flock(state_file, fcntl.LOCK_UN)


def _device_lost(detail: str) -> bool:
    return any(marker in detail.lower() for marker in DEVICE_LOSS_MARKERS)


def _mono_float32(audio) -> np.ndarray:
    value = np.asarray(audio, dtype=np.float32)
    if value.ndim > 1:
        value = value[:, 0]
    value = np.nan_to_num(value, nan=0.0, posinf=1.0, neginf=-1.0)
    return np.clip(value, -1.0, 1.0)


def _wav_bytes(audio, sample_rate: int = 16000) -> bytes:
    buffer = io.BytesIO()
    sf.write(buffer, _mono_float32(audio), sample_rate, format="WAV", subtype="PCM_16")
    return buffer.getvalue()


def _segment_result(text: str):
    text = str(text or "").strip()
    segments = [SimpleNamespace(text=text)] if text else []
    return segments, SimpleNamespace(language=None)


def _cache_dir() -> Path:
    configured = os.getenv("ZARA_WHISPER_CPP_MODEL_DIR")
    if configured:
        return Path(configured).expanduser()

    xdg_cache = os.getenv("XDG_CACHE_HOME")
    root = Path(xdg_cache).expanduser() if xdg_cache else Path.home() / ".cache"
    return root / "zara" / "whisper.cpp"


def resolve_whisper_cpp_model(
    model: str,
    *,
    log: Callable[[str], None] = print,
) -> str:
    """Resolve a whisper.cpp GGML model name to a local model file."""
    path = Path(str(model)).expanduser()
    if path.is_file():
        return str(path.resolve())

    name = str(model).strip()
    if name == "large":
        name = "large-v3"
    elif name == "turbo":
        name = "large-v3-turbo"

    if name not in WHISPER_CPP_MODELS:
        choices = ", ".join(sorted(WHISPER_CPP_MODELS))
        raise ValueError(
            f"Unknown whisper.cpp model {model!r}; pass a GGML .bin file or one of: {choices}"
        )

    cache_dir = _cache_dir()
    cache_dir.mkdir(parents=True, exist_ok=True)
    target = cache_dir / f"ggml-{name}.bin"
    if target.is_file() and target.stat().st_size > 0:
        log(f"whisper.cpp model cached: {target}")
        return str(target.resolve())

    url = f"{MODEL_BASE_URL}/ggml-{name}.bin"
    partial = target.with_suffix(target.suffix + ".part")
    headers = {}
    hf_token = os.getenv("HF_TOKEN")
    if hf_token:
        headers["Authorization"] = f"Bearer {hf_token}"

    log(f"Downloading whisper.cpp model {name} to {target}")
    downloaded = 0
    reported = 0
    timeout = httpx.Timeout(connect=30.0, read=None, write=30.0, pool=30.0)

    try:
        with httpx.stream(
            "GET",
            url,
            headers=headers,
            follow_redirects=True,
            timeout=timeout,
        ) as response:
            response.raise_for_status()
            total = int(response.headers.get("content-length", "0") or 0)
            with partial.open("wb") as output:
                for chunk in response.iter_bytes(chunk_size=1024 * 1024):
                    if not chunk:
                        continue
                    output.write(chunk)
                    downloaded += len(chunk)
                    if downloaded - reported < DOWNLOAD_REPORT_BYTES:
                        continue
                    reported = downloaded
                    current = downloaded / (1024 * 1024)
                    if total > 0:
                        maximum = total / (1024 * 1024)
                        log(f"whisper.cpp model download: {current:.0f}/{maximum:.0f} MiB")
                    else:
                        log(f"whisper.cpp model download: {current:.0f} MiB")
        partial.replace(target)
    except Exception:
        partial.unlink(missing_ok=True)
        raise

    log(f"whisper.cpp model ready: {target}")
    return str(target.resolve())


def _free_local_port() -> int:
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
        sock.bind(("127.0.0.1", 0))
        return int(sock.getsockname()[1])


def _server_binary() -> str:
    configured = os.getenv("ZARA_WHISPER_CPP_SERVER_BIN")
    if configured:
        return configured

    binary = shutil.which("whisper-server")
    if binary:
        return binary

    raise RuntimeError(
        "whisper.cpp STT requires whisper-server on PATH; install Zara's Vulkan "
        "whisper.cpp package or set ZARA_WHISPER_CPP_SERVER_BIN"
    )


def _normalize_backend_device(device: str) -> str:
    normalized = str(device).strip().lower()
    if normalized == "cpu":
        return "cpu"
    if normalized == "vulkan":
        return "vulkan"
    if normalized == "cuda":
        # Zara's legacy dictation layer normalizes every GPU to the historical
        # faster-whisper `cuda` token before constructing WhisperModel. Inside
        # this provider compatibility boundary that token means Vulkan, not CUDA.
        return "vulkan"
    raise ValueError(
        f"whisper.cpp Zara backend supports cpu or vulkan, got {device!r}"
    )


class WhisperCppModel:
    """faster-whisper-compatible adapter backed by a resident whisper-server."""

    def __init__(
        self,
        model: str,
        *,
        device: str = "cpu",
        cpu_threads: int = 4,
        **_kwargs,
    ):
        model_path = Path(model).expanduser()
        if not model_path.is_file():
            raise ValueError(
                f"whisper.cpp requires a local GGML model file, got {model!r}"
            )

        self.model = str(model_path.resolve())
        self.device = _normalize_backend_device(device)
        self.cpu_threads = max(1, int(cpu_threads or 1))
        self.binary = _server_binary()
        self.port = _free_local_port()
        self.endpoint = f"http://127.0.0.1:{self.port}"
        self._request_lock = threading.Lock()
        self._client = httpx.Client(timeout=httpx.Timeout(120.0, connect=5.0))
        self._log = tempfile.TemporaryFile(mode="w+t")
        self._process: subprocess.Popen | None = None
        try:
            self._start_server()
        except Exception:
            self._client.close()
            self._log.close()
            raise
        atexit.register(self.close)

    def _command(self) -> list[str]:
        command = [
            self.binary,
            "--model",
            self.model,
            "--host",
            "127.0.0.1",
            "--port",
            str(self.port),
            "--threads",
            str(self.cpu_threads),
            "--no-flash-attn",
        ]
        if self.device == "cpu":
            command.append("--no-gpu")
        return command

    def _start_server(self) -> None:
        command = self._command()
        with _locked_worker_state() as state:
            pid = state.get("pid")
            identity = state.get("identity")
            if pid and identity and _worker_identity(pid) == identity:
                raise RuntimeError(
                    f"whisper.cpp worker PID {pid} remains alive; "
                    "speech transcription degraded until it exits"
                )
            if self.device == "vulkan" and state.get("gpu_disabled"):
                raise RuntimeError(
                    "Vulkan device lost; whisper.cpp GPU worker disabled "
                    "until worker state is cleared after recovery (select CPU meanwhile)"
                )
            self._process = subprocess.Popen(
                command,
                stdin=subprocess.DEVNULL,
                stdout=self._log,
                stderr=subprocess.STDOUT,
                text=True,
            )
            state["pid"] = self._process.pid
            state["identity"] = _worker_identity(self._process.pid)
        logger.info(
            "[WhisperCpp] worker start backend=whisper.cpp device=%s pid=%s argv=%r",
            self.device, self._process.pid, command,
        )

        timeout = float(os.getenv("ZARA_WHISPER_CPP_START_TIMEOUT", "120"))
        deadline = time.monotonic() + max(1.0, timeout)
        last_error: Exception | None = None

        while time.monotonic() < deadline:
            if self._process.poll() is not None:
                detail = self._startup_failure()
                self._record_device_loss(detail)
                self._clear_worker_state()
                self._process = None
                raise RuntimeError(detail)
            try:
                response = self._client.get(f"{self.endpoint}/", timeout=2.0)
                if response.status_code < 500:
                    return
            except httpx.HTTPError as error:
                last_error = error
            time.sleep(0.1)

        detail = self._startup_failure()
        if last_error:
            detail = f"{detail}; last HTTP error: {last_error}"
        self._record_device_loss(detail)
        try:
            self.close()
        except RuntimeError as error:
            detail = f"{detail}; {error}"
        raise RuntimeError(detail)

    def _clear_worker_state(self) -> None:
        with _locked_worker_state() as state:
            if state.get("pid") == self._process.pid:
                state.pop("pid", None)
                state.pop("identity", None)

    def _record_device_loss(self, detail: str) -> None:
        if self.device == "vulkan" and _device_lost(detail):
            with _locked_worker_state() as state:
                state["gpu_disabled"] = True
            logger.error("[WhisperCpp] Vulkan device loss; GPU worker disabled: %s", detail)

    def _startup_failure(self) -> str:
        self._log.flush()
        self._log.seek(0)
        output = self._log.read().strip()
        suffix = f": {output[-4000:]}" if output else ""
        return f"whisper.cpp {self.device} server failed to start{suffix}"

    def transcribe(self, audio, **kwargs):
        if self.device == "vulkan":
            with _locked_worker_state() as state:
                if state.get("gpu_disabled"):
                    raise RuntimeError(
                        "Vulkan device lost; whisper.cpp GPU worker disabled"
                    )
        data = {
            "response_format": "json",
            "temperature": "0.0",
            "no_timestamps": "true",
            "vad": "false",
        }

        beam_size = kwargs.get("beam_size")
        if beam_size is not None:
            data["beam_size"] = str(int(beam_size))

        language = kwargs.get("language")
        if language:
            data["language"] = str(language)

        prompt = kwargs.get("initial_prompt")
        if prompt:
            data["prompt"] = str(prompt)

        files = {"file": ("audio.wav", _wav_bytes(audio), "audio/wav")}
        with self._request_lock:
            try:
                response = self._client.post(
                    f"{self.endpoint}/inference",
                    data=data,
                    files=files,
                )
                response.raise_for_status()
            except httpx.HTTPError as error:
                detail = f"{error}; {self._startup_failure()}"
                self._record_device_loss(detail)
                raise RuntimeError(detail) from error

        content_type = response.headers.get("content-type", "")
        if "json" in content_type:
            payload = response.json()
            if isinstance(payload, dict):
                return _segment_result(payload.get("text", ""))
        return _segment_result(response.text)

    def close(self) -> None:
        process = self._process
        if process is not None and process.poll() is None:
            try:
                process.terminate()
            except ProcessLookupError:
                pass
            try:
                process.wait(timeout=2.0)
            except subprocess.TimeoutExpired:
                try:
                    process.kill()
                except ProcessLookupError:
                    pass
                try:
                    process.wait(timeout=2.0)
                except subprocess.TimeoutExpired as error:
                    logger.error(
                        "[WhisperCpp] worker PID %s remains alive after SIGKILL; "
                        "speech transcription degraded", process.pid,
                    )
                    raise RuntimeError(
                        f"whisper.cpp worker PID {process.pid} remains alive after SIGKILL"
                    ) from error
        if process is not None:
            self._clear_worker_state()
            self._process = None

        try:
            self._client.close()
        except Exception:
            pass

        try:
            self._log.close()
        except Exception:
            pass
