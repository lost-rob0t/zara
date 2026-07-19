#!/usr/bin/env python3
"""
Zarathustra Dictation Helper - Optimized
-----------------------------------------
Memory-safe with bounded queues, lower latency, parallel transcription.
"""

import atexit
import os
import queue
import re
import signal
import shutil
import subprocess
import sys
import time
from collections import deque
from concurrent.futures import Future, ThreadPoolExecutor
from threading import Event, Thread, current_thread, main_thread

import numpy as np
import sounddevice as sd

from zara.audio import resolve_input_sample_rate, resample_audio

try:
    from faster_whisper import WhisperModel
except Exception as e:
    print("ERROR: faster-whisper not installed. Run: pip install faster-whisper", file=sys.stderr)
    sys.exit(1)

PIDFILE = os.getenv("ZARA_DICTATION_PIDFILE", "/tmp/zara_dictation.pid")
LOGFILE = os.getenv("ZARA_DICTATION_LOGFILE", "/tmp/zara_dictation.log")

DEFAULT_STOP_PHRASES = (
    "end voice",
    "stop voice",
    "stop dictation",
    "end dictation",
    "disable",
    "end quote",
)

TARGET_SAMPLE_RATE = 16000
INPUT_SAMPLE_RATE = None
CHANNELS = 1
CHUNK_SECONDS = 10

MAX_QUEUE_SIZE = 10
USE_XDO = shutil.which("xdotool") is not None
stop_event = Event()
audio_queue = queue.Queue(maxsize=MAX_QUEUE_SIZE)
chunk_queue = queue.Queue(maxsize=MAX_QUEUE_SIZE)


def write_pid():
    with open(PIDFILE, "w") as f:
        f.write(str(os.getpid()))


def remove_pid():
    try:
        os.remove(PIDFILE)
    except FileNotFoundError:
        pass


atexit.register(remove_pid)


def _handle_termination(signum, frame):
    stop_event.set()
    raise SystemExit(0)


def log(msg):
    ts = time.strftime("%H:%M:%S")
    with open(LOGFILE, "a") as f:
        f.write(f"[{ts}] {msg}\n")
    print(msg, flush=True)


def _get_input_sample_rate() -> float:
    global INPUT_SAMPLE_RATE
    if INPUT_SAMPLE_RATE is None:
        INPUT_SAMPLE_RATE, rate_note = resolve_input_sample_rate(
            TARGET_SAMPLE_RATE,
            channels=CHANNELS,
        )
        if rate_note:
            log(rate_note)
    return INPUT_SAMPLE_RATE


def record_audio(q, stop_event):
    """Audio capture with queue overflow protection"""
    def callback(indata, frames, time_info, status):
        if status:
            log(f"Audio stream warning: {status}")
        _put_latest(q, indata.copy())

    with sd.InputStream(
        samplerate=_get_input_sample_rate(),
        channels=CHANNELS,
        callback=callback,
    ):
        while not stop_event.is_set():
            time.sleep(0.1)



def chunk_audio(q, outq, stop_event):
    """Assemble audio chunks with no memory growth (deque instead of concat)"""
    target_frames = int(CHUNK_SECONDS * _get_input_sample_rate())
    buffer = deque()

    while not stop_event.is_set():
        try:
            data = q.get(timeout=0.5)
        except queue.Empty:
            continue

        # extend buffer without reallocating giant arrays
        buffer.extend(data[:, 0] if data.ndim > 1 else data)

        while len(buffer) >= target_frames:
            chunk = np.array([buffer.popleft() for _ in range(target_frames)], dtype=np.float32)

            _put_latest(outq, chunk)


def _put_latest(target_queue, item) -> None:
    try:
        target_queue.put_nowait(item)
    except queue.Full:
        try:
            target_queue.get_nowait()
        except queue.Empty:
            return
        try:
            target_queue.put_nowait(item)
        except queue.Full:
            return


def type_text(text):
    """Type text into focused window"""
    if USE_XDO:
        subprocess.run(["xdotool", "type", "--delay", "0", "--clearmodifiers", text])
    else:
        try:
            from pynput.keyboard import Controller
            kb = Controller()
            kb.type(text)
        except Exception as e:
            log(f"Typing failed: {e}")


def _normalize_stop_phrase(s: str) -> str:
    without_punctuation = re.sub(r"[^\w\s]", "", s.casefold())
    return " ".join(without_punctuation.split())


def _is_any_stop_phrase(text: str, stop_phrases: list[str]) -> bool:
    norm = _normalize_stop_phrase(text)
    return any(norm == _normalize_stop_phrase(p) for p in stop_phrases)


def _strip_dictation_tokens(text: str, stop_phrases: list[str]) -> str:
    if not text:
        return ""
    normalized = _normalize_stop_phrase(text)
    wake_tokens = ["zara", "hey zara", "zarathustra"]
    cleaned = normalized
    for token in wake_tokens:
        if cleaned.startswith(token):
            cleaned = cleaned[len(token):].strip()
            break
    for stop in stop_phrases:
        stop_norm = _normalize_stop_phrase(stop)
        if cleaned.endswith(stop_norm):
            cleaned = cleaned[: -len(stop_norm)].strip()
            break
    return cleaned


def transcribe_worker(model, chunk):
    """Worker function for parallel transcription"""
    if chunk.ndim > 1:
        chunk = chunk[:, 0]

    if INPUT_SAMPLE_RATE and INPUT_SAMPLE_RATE != TARGET_SAMPLE_RATE:
        chunk = resample_audio(chunk, INPUT_SAMPLE_RATE, TARGET_SAMPLE_RATE)

    audio_float = chunk.astype(np.float32)

    rms = np.sqrt(np.mean(audio_float**2))
    log(f"Audio RMS: {rms:.4f}")

    if rms < 0.001:
        log("Skipping: silence detected")
        return None

    try:
        segments, _ = model.transcribe(
            audio_float,
            beam_size=1,
            vad_filter=True,
            language="en",
            condition_on_previous_text=False,
            no_speech_threshold=0.4,
        )

        texts = []
        for seg in segments:
            text = seg.text.strip()
            if text:
                texts.append(text)

        result = " ".join(texts) if texts else None

        if result and result.lower().strip() in ["thank you", "thanks", "thank you."]:
            log(f"Filtered hallucination: {result}")
            return None

        return result
    except Exception as e:
        log(f"Transcription error: {e}")
        return None


def _parse_stop_phrases(stop_arg: str | None) -> list[str]:
    if not stop_arg:
        return []
    parts = [p.strip() for p in stop_arg.split(",")]
    return [p for p in parts if p]


def _resolve_stop_phrases(
    configured_phrases: list[str] | tuple[str, ...] | str | None,
) -> list[str]:
    env_multi = os.getenv("ZARA_STOP_PHRASES")
    env_single = os.getenv("ZARA_STOP_PHRASE")

    phrases: list[str] = []
    phrases.extend(_parse_stop_phrases(env_multi))
    if env_single:
        phrases.append(env_single.strip())
    if isinstance(configured_phrases, str):
        phrases.extend(_parse_stop_phrases(configured_phrases))
    elif configured_phrases:
        phrases.extend(configured_phrases)

    if not phrases:
        phrases = list(DEFAULT_STOP_PHRASES)

    out = []
    seen = set()
    for phrase in phrases:
        normalized = _normalize_stop_phrase(phrase)
        if normalized and normalized not in seen:
            seen.add(normalized)
            out.append(normalized)
    return out


def _commit_ready_transcriptions(
    pending: dict[int, Future],
    next_sequence: int,
    stop_phrases: list[str],
    run_stop_event: Event,
    typing_sink=type_text,
) -> int:
    while next_sequence in pending and pending[next_sequence].done():
        future = pending.pop(next_sequence)
        try:
            text = future.result()
            if text and _is_any_stop_phrase(text, stop_phrases):
                log(f"Stop phrase heard ({text!r}); stopping dictation")
                run_stop_event.set()
            elif text:
                cleaned = _strip_dictation_tokens(text, stop_phrases)
                if cleaned:
                    log(f"Typed: {cleaned}")
                    typing_sink(cleaned + " ")
        except Exception as error:
            log(f"Future error: {error}")
        next_sequence += 1
        if run_stop_event.is_set():
            break
    return next_sequence


def _drain_queue(target_queue) -> None:
    while True:
        try:
            target_queue.get_nowait()
        except queue.Empty:
            return


def _reset_runtime_state() -> tuple[Event, queue.Queue, queue.Queue]:
    global INPUT_SAMPLE_RATE, audio_queue, chunk_queue, stop_event
    INPUT_SAMPLE_RATE = None
    stop_event = Event()
    audio_queue = queue.Queue(maxsize=MAX_QUEUE_SIZE)
    chunk_queue = queue.Queue(maxsize=MAX_QUEUE_SIZE)
    return stop_event, audio_queue, chunk_queue


def main(model_name="small", device="cpu", threads=None, workers=2, stop_phrases=None):
    run_stop_event, run_audio_queue, run_chunk_queue = _reset_runtime_state()
    resolved_stop_phrases = _resolve_stop_phrases(stop_phrases)
    started_threads: list[Thread] = []
    executor = None
    pending: dict[int, Future] = {}
    previous_sigterm = None

    try:
        if current_thread() is main_thread():
            previous_sigterm = signal.getsignal(signal.SIGTERM)
            signal.signal(signal.SIGTERM, _handle_termination)

        log(
            "Starting Zara Dictation Helper (Optimized); "
            f"stop_phrases={resolved_stop_phrases!r}"
        )
        write_pid()
        _get_input_sample_rate()

        if threads is None:
            threads = int(os.getenv("ZARA_THREADS", os.cpu_count() or 1))
        log(f"Using {threads} Whisper threads, {workers} parallel workers")

        compute = "float16" if device == "cuda" else "int8"
        log(f"Loading Whisper model '{model_name}' on {device} ({compute=})")
        try:
            model = WhisperModel(
                model_name,
                device=device,
                compute_type=compute,
                num_workers=threads,
            )
        except Exception as error:
            log(f"Failed with compute_type={compute}: {error}")
            log("Retrying with compute_type='int8'")
            model = WhisperModel(
                model_name,
                device=device,
                compute_type="int8",
                num_workers=threads,
            )

        recorder = Thread(
            target=record_audio,
            args=(run_audio_queue, run_stop_event),
            daemon=True,
        )
        recorder.start()
        started_threads.append(recorder)
        chunker = Thread(
            target=chunk_audio,
            args=(run_audio_queue, run_chunk_queue, run_stop_event),
            daemon=True,
        )
        chunker.start()
        started_threads.append(chunker)

        executor = ThreadPoolExecutor(max_workers=workers)
        max_pending = workers * 2
        next_submit = 0
        next_commit = 0

        while not run_stop_event.is_set():
            next_commit = _commit_ready_transcriptions(
                pending,
                next_commit,
                resolved_stop_phrases,
                run_stop_event,
            )
            if run_stop_event.is_set():
                break
            if len(pending) >= max_pending:
                time.sleep(0.05)
                continue
            try:
                chunk = run_chunk_queue.get(timeout=0.5)
            except queue.Empty:
                continue
            pending[next_submit] = executor.submit(
                transcribe_worker,
                model,
                chunk,
            )
            next_submit += 1
    finally:
        run_stop_event.set()
        for future in pending.values():
            future.cancel()
        if executor is not None:
            executor.shutdown(wait=True, cancel_futures=True)
        for worker_thread in started_threads:
            worker_thread.join()
        _drain_queue(run_audio_queue)
        _drain_queue(run_chunk_queue)
        remove_pid()
        if previous_sigterm is not None:
            signal.signal(signal.SIGTERM, previous_sigterm)
        _reset_runtime_state()
        log("Dictation stopped.")

    return 0


def cli_main(argv=None) -> int:
    arguments = list(sys.argv[1:] if argv is None else argv)
    if arguments and arguments[0] == "devices":
        print("Available audio input devices:\n")
        devices = sd.query_devices()
        for i, dev in enumerate(devices):
            if dev['max_input_channels'] > 0:
                print(f"{i}: {dev['name']} (inputs: {dev['max_input_channels']})")
        print("\nSet device with: export SD_DEVICE=<number>")
        return 0

    if arguments and arguments[0] == "test":
        print("Audio level test mode - speak into your mic")
        print("Press Ctrl+C to stop\n")

        def test_callback(indata, frames, time_info, status):
            rms = np.sqrt(np.mean(indata**2))
            bars = "#" * int(rms * 500)
            print(f"Level: {rms:.4f} |{bars}", end='\r')

        try:
            INPUT_SAMPLE_RATE, rate_note = resolve_input_sample_rate(
                TARGET_SAMPLE_RATE,
                channels=CHANNELS,
            )
            if rate_note:
                log(rate_note)
            with sd.InputStream(
                samplerate=INPUT_SAMPLE_RATE,
                channels=CHANNELS,
                callback=test_callback,
            ):
                while True:
                    time.sleep(0.1)
        except KeyboardInterrupt:
            print("\nTest complete")
            return 0

    model = arguments[0] if len(arguments) > 0 else "small"
    device = arguments[1] if len(arguments) > 1 else "cpu"
    threads = int(arguments[2]) if len(arguments) > 2 else None
    workers = int(arguments[3]) if len(arguments) > 3 else 2
    cli_stop = arguments[4] if len(arguments) > 4 else None

    try:
        return main(model, device, threads, workers, stop_phrases=cli_stop)
    except KeyboardInterrupt:
        log("Interrupted by user.")
        return 130


if __name__ == "__main__":
    raise SystemExit(cli_main())
