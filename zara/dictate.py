#!/usr/bin/env python3
"""
Zarathustra Dictation Helper - Optimized
-----------------------------------------
Memory-safe with bounded queues, lower latency, parallel transcription.
"""

import os
import sys
import time
import queue
import subprocess
import shutil
import numpy as np
import sounddevice as sd
from threading import Thread, Event
from concurrent.futures import ThreadPoolExecutor
import gc

from collections import deque

from zara.audio import resolve_input_sample_rate, resample_audio

try:
    from faster_whisper import WhisperModel
except Exception as e:
    print("ERROR: faster-whisper not installed. Run: pip install faster-whisper", file=sys.stderr)
    sys.exit(1)

PIDFILE = "/tmp/zara_dictation.pid"
LOGFILE = "/tmp/zara_dictation.log"

# Stop phrase support:
# - Defaults: "end voice", "stop voice", "stop dictation"
# - Override with env:
#     ZARA_STOP_PHRASE="end voice"             (single)
#     ZARA_STOP_PHRASES="end voice,stop voice" (comma-separated)
# - Or pass as 5th arg: zara-dictate <model> <device> <threads?> <workers?> "end voice,stop voice"
DEFAULT_STOP_PHRASES = ["end voice", "stop voice", "stop dictation", "end dictation" "disable", "end quote"]

TARGET_SAMPLE_RATE = 16000
INPUT_SAMPLE_RATE = None
CHANNELS = 1
CHUNK_SECONDS = 10  # Reduced from 5 for lower latency

# Bounded queues prevent memory bloat
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
        try:
            q.put_nowait(indata.copy())
        except queue.Full:
            # Drop oldest frame if queue is full (prevents backlog)
            try:
                q.get_nowait()
                q.put_nowait(indata.copy())
            except:
                pass

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

            try:
                outq.put_nowait(chunk)
            except queue.Full:
                try:
                    outq.get_nowait()
                    outq.put_nowait(chunk)
                except:
                    pass
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
    return " ".join(s.lower().strip().split())


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


def transcribe_worker(model, chunk, stop_phrases: list[str]):
    """Worker function for parallel transcription"""
    if chunk.ndim > 1:
        chunk = chunk[:, 0]

    if INPUT_SAMPLE_RATE and INPUT_SAMPLE_RATE != TARGET_SAMPLE_RATE:
        chunk = resample_audio(chunk, INPUT_SAMPLE_RATE, TARGET_SAMPLE_RATE)

    audio_float = chunk.astype(np.float32)

    # Silence detection - prevent hallucinations
    rms = np.sqrt(np.mean(audio_float**2))
    log(f"Audio RMS: {rms:.4f}")  # Debug: see audio levels

    if rms < 0.001:  # Much lower threshold - only block dead silence
        log("Skipping: silence detected")
        return None

    try:
        # Re-enable VAD to prevent hallucinations
        segments, info = model.transcribe(
            audio_float,
            beam_size=1,
            vad_filter=True,
            language="en",
            condition_on_previous_text=False,
            no_speech_threshold=0.4  # Lower threshold = more sensitive
        )

        texts = []
        for seg in segments:
            text = seg.text.strip()
            if text:
                texts.append(text)

        result = " ".join(texts) if texts else None

        # Only block if it's JUST a hallucination phrase alone
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


def _resolve_stop_phrases(cli_stop_arg: str | None) -> list[str]:
    env_multi = os.getenv("ZARA_STOP_PHRASES")
    env_single = os.getenv("ZARA_STOP_PHRASE")

    phrases = []
    phrases.extend(_parse_stop_phrases(env_multi))
    if env_single:
        phrases.append(env_single.strip())
    phrases.extend(_parse_stop_phrases(cli_stop_arg))

    if not phrases:
        phrases = list(DEFAULT_STOP_PHRASES)

    # normalize + dedupe
    out = []
    seen = set()
    for p in phrases:
        n = _normalize_stop_phrase(p)
        if n and n not in seen:
            seen.add(n)
            out.append(p)
    return out


def main(model_name="small", device="cpu", threads=None, workers=2, stop_phrases=None):
    if stop_phrases is None:
        stop_phrases = list(DEFAULT_STOP_PHRASES)
    log(f"Starting Zara Dictation Helper (Optimized); stop_phrases={stop_phrases!r}")
    write_pid()

    _get_input_sample_rate()

    # Determine thread count for Whisper
    if threads is None:
        threads = int(os.getenv("ZARA_THREADS", os.cpu_count() or 1))
    log(f"Using {threads} Whisper threads, {workers} parallel workers")

    if device == "cpu":
        compute = "int8"
    elif device == "cuda":
        compute = "float16"
    else:
        compute = "int8"

    log(f"Loading Whisper model '{model_name}' on {device} ({compute=})")

    try:
        model = WhisperModel(
            model_name,
            device=device,
            compute_type=compute,
            num_workers=threads
        )
    except Exception as e:
        log(f"Failed with compute_type={compute}: {e}")
        log("Retrying with compute_type='int8'")
        model = WhisperModel(
            model_name,
            device=device,
            compute_type="int8",
            num_workers=threads
        )

    # Start audio pipeline
    rec_thread = Thread(target=record_audio, args=(audio_queue, stop_event), daemon=True)
    ch_thread = Thread(target=chunk_audio, args=(audio_queue, chunk_queue, stop_event), daemon=True)
    rec_thread.start()
    ch_thread.start()

    # Use ThreadPoolExecutor for parallel transcription
    with ThreadPoolExecutor(max_workers=workers) as executor:
        futures = []
        max_pending = workers * 2  # Limit pending jobs

        while not stop_event.is_set():

            # Process finished futures in-place to avoid memory buildup
            i = 0
            while i < len(futures):
                f = futures[i]
                if f.done():
                    try:
                        text = f.result(timeout=0.1)
                        if text:
                            if _is_any_stop_phrase(text, stop_phrases):
                                log(f"Stop phrase heard ({text!r}); stopping dictation")
                                stop_event.set()
                                break
                            cleaned = _strip_dictation_tokens(text, stop_phrases)
                            if cleaned:
                                log(f"Typed: {cleaned}")
                                type_text(cleaned + " ")
                    except Exception as e:
                        log(f"Future error: {e}")

                    del futures[i]   # Remove reference to allow GC
                    gc.collect()      # Force cleanup to keep RAM flat
                else:
                    i += 1

            # Pull new chunks only if room
            if len(futures) < max_pending:
                try:
                    chunk = chunk_queue.get(timeout=0.5)
                    future = executor.submit(transcribe_worker, model, chunk, stop_phrases)
                    futures.append(future)
                except queue.Empty:
                    continue
            else:
                time.sleep(0.05)

    remove_pid()
    log("Dictation stopped.")



if __name__ == "__main__":
    # List audio devices
    if len(sys.argv) > 1 and sys.argv[1] == "devices":
        print("Available audio input devices:\n")
        devices = sd.query_devices()
        for i, dev in enumerate(devices):
            if dev['max_input_channels'] > 0:
                print(f"{i}: {dev['name']} (inputs: {dev['max_input_channels']})")
        print("\nSet device with: export SD_DEVICE=<number>")
        sys.exit(0)

    # Test mode: just show audio levels
    if len(sys.argv) > 1 and sys.argv[1] == "test":
        print("Audio level test mode - speak into your mic")
        print("Press Ctrl+C to stop\n")

        def test_callback(indata, frames, time_info, status):
            rms = np.sqrt(np.mean(indata**2))
            bars = "#" * int(rms * 500)  # Visual level meter
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
            sys.exit(0)

    # Normal mode
    model = sys.argv[1] if len(sys.argv) > 1 else "small"
    device = sys.argv[2] if len(sys.argv) > 2 else "cpu"
    threads = int(sys.argv[3]) if len(sys.argv) > 3 else None
    workers = int(sys.argv[4]) if len(sys.argv) > 4 else 2
    cli_stop = sys.argv[5] if len(sys.argv) > 5 else None

    stop_phrases = _resolve_stop_phrases(cli_stop)

    try:
        main(model, device, threads, workers, stop_phrases=stop_phrases)
    except KeyboardInterrupt:
        stop_event.set()
        remove_pid()
        log("Interrupted by user.")
