"""Observable faster-whisper model-file resolution for CLI startup.

A named faster-whisper model such as ``small`` is resolved through the
Hugging Face Hub before CTranslate2 can load it.  Keep that blocking network
phase separate from the wake listener's actual model-load phase so startup
never looks silently hung.
"""

from __future__ import annotations

import pathlib
import queue
import threading
import time
from typing import Callable, Optional


LogFn = Callable[[str], None]
DownloadFn = Callable[..., str]


def _default_download_model(model: str, **kwargs) -> str:
    from faster_whisper.utils import download_model

    return download_model(model, **kwargs)


def resolve_whisper_model_files(
    model: str,
    *,
    download_model_fn: Optional[DownloadFn] = None,
    log: LogFn = print,
    heartbeat_seconds: float = 10.0,
    clock: Callable[[], float] = time.monotonic,
) -> str:
    """Return a local model directory, reporting cache/download progress.

    Existing local directories are returned untouched. Named models are first
    resolved with ``local_files_only=True`` so an already cached model never
    needs a network request. On a cache miss, resolution/download runs in a
    daemon worker while the caller emits a conservative elapsed-time heartbeat.

    The heartbeat reports only what Zara can observe: that model-file
    resolution is still running. It does not invent provider-side percentages.
    """

    path = pathlib.Path(model).expanduser()
    if path.is_dir():
        resolved = str(path.resolve())
        log(f"Whisper model files already local: {resolved}")
        return resolved

    if heartbeat_seconds <= 0:
        raise ValueError("heartbeat_seconds must be positive")

    download = download_model_fn or _default_download_model

    log(f"Checking local Whisper model cache: {model}")
    try:
        cached_path = download(model, local_files_only=True)
    except Exception:
        cached_path = None

    if cached_path:
        log(f"Whisper model files cached: {model} ({cached_path})")
        return str(cached_path)

    log(f"Whisper model {model} not cached; resolving/downloading from Hugging Face")

    result_queue: queue.Queue = queue.Queue(maxsize=1)

    def worker() -> None:
        try:
            result_queue.put((True, download(model)))
        except Exception as error:
            result_queue.put((False, error))

    started = clock()
    thread = threading.Thread(
        target=worker,
        name=f"zara-whisper-download-{model}",
        daemon=True,
    )
    thread.start()

    while True:
        try:
            ok, value = result_queue.get(timeout=heartbeat_seconds)
        except queue.Empty:
            elapsed = max(0.0, clock() - started)
            log(
                f"Still resolving/downloading Whisper {model} "
                f"({elapsed:.0f}s elapsed)"
            )
            continue

        elapsed = max(0.0, clock() - started)
        if not ok:
            log(f"Whisper model {model} download failed after {elapsed:.1f}s: {value}")
            raise value

        log(f"Whisper model files ready: {model} ({elapsed:.1f}s, {value})")
        return str(value)
