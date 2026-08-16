"""Persistent whisper.cpp speech-to-text backend for Zara.

The adapter keeps ``whisper-server`` alive so the model remains resident in
GPU memory between Zara wake/dictation requests. Zara still owns microphone
capture and VAD segmentation; whisper.cpp receives only the final 16 kHz WAV
chunk selected for transcription.
"""

from __future__ import annotations

import atexit
import io
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
        self._start_server()
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
        self._process = subprocess.Popen(
            self._command(),
            stdin=subprocess.DEVNULL,
            stdout=self._log,
            stderr=subprocess.STDOUT,
            text=True,
        )

        timeout = float(os.getenv("ZARA_WHISPER_CPP_START_TIMEOUT", "120"))
        deadline = time.monotonic() + max(1.0, timeout)
        last_error: Exception | None = None

        while time.monotonic() < deadline:
            if self._process.poll() is not None:
                raise RuntimeError(self._startup_failure())
            try:
                response = self._client.get(f"{self.endpoint}/")
                if response.status_code < 500:
                    return
            except httpx.HTTPError as error:
                last_error = error
            time.sleep(0.1)

        detail = self._startup_failure()
        if last_error:
            detail = f"{detail}; last HTTP error: {last_error}"
        self.close()
        raise RuntimeError(detail)

    def _startup_failure(self) -> str:
        self._log.flush()
        self._log.seek(0)
        output = self._log.read().strip()
        suffix = f": {output[-4000:]}" if output else ""
        return f"whisper.cpp {self.device} server failed to start{suffix}"

    def transcribe(self, audio, **kwargs):
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
            response = self._client.post(
                f"{self.endpoint}/inference",
                data=data,
                files=files,
            )
        response.raise_for_status()

        content_type = response.headers.get("content-type", "")
        if "json" in content_type:
            payload = response.json()
            if isinstance(payload, dict):
                return _segment_result(payload.get("text", ""))
        return _segment_result(response.text)

    def close(self) -> None:
        process = self._process
        self._process = None
        if process is not None and process.poll() is None:
            process.terminate()
            try:
                process.wait(timeout=2.0)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait(timeout=2.0)

        try:
            self._client.close()
        except Exception:
            pass

        try:
            self._log.close()
        except Exception:
            pass
