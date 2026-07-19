import queue
from concurrent.futures import Future

import pytest

import zara.dictate as dictate


def clear_stop_environment(monkeypatch):
    monkeypatch.delenv("ZARA_STOP_PHRASE", raising=False)
    monkeypatch.delenv("ZARA_STOP_PHRASES", raising=False)


@pytest.mark.parametrize(
    "phrase",
    [
        "end dictation",
        "disable",
        "end quote",
        "END DICTATION!",
        "Disable.",
        "  end   quote? ",
    ],
)
def test_canonical_stop_phrase_table_handles_case_and_punctuation(
    monkeypatch,
    phrase,
):
    clear_stop_environment(monkeypatch)
    resolved = dictate._resolve_stop_phrases(None)

    assert dictate._is_any_stop_phrase(phrase, resolved)
    assert "end dictation" in resolved
    assert "disable" in resolved
    assert "end quote" in resolved


def test_configured_and_environment_stop_phrases_are_combined(monkeypatch):
    clear_stop_environment(monkeypatch)
    monkeypatch.setenv("ZARA_STOP_PHRASES", "Environment Stop!,end quote")
    monkeypatch.setenv("ZARA_STOP_PHRASE", "Single Stop.")

    resolved = dictate._resolve_stop_phrases(["Configured Stop?", "END QUOTE"])

    assert resolved == [
        "environment stop",
        "end quote",
        "single stop",
        "configured stop",
    ]


def test_completed_transcriptions_commit_only_in_capture_order(monkeypatch):
    monkeypatch.setattr(dictate, "log", lambda message: None)
    first = Future()
    second = Future()
    second.set_result("second")
    pending = {0: first, 1: second}
    typed = []
    run_stop_event = dictate.Event()

    next_sequence = dictate._commit_ready_transcriptions(
        pending,
        0,
        list(dictate.DEFAULT_STOP_PHRASES),
        run_stop_event,
        typing_sink=typed.append,
    )

    assert next_sequence == 0
    assert typed == []

    first.set_result("first")
    next_sequence = dictate._commit_ready_transcriptions(
        pending,
        next_sequence,
        list(dictate.DEFAULT_STOP_PHRASES),
        run_stop_event,
        typing_sink=typed.append,
    )

    assert next_sequence == 2
    assert typed == ["first ", "second "]


def test_queue_overflow_drops_oldest_item():
    target = queue.Queue(maxsize=2)
    target.put_nowait("first")
    target.put_nowait("second")

    dictate._put_latest(target, "third")

    assert target.get_nowait() == "second"
    assert target.get_nowait() == "third"


def configure_main_test(monkeypatch, tmp_path):
    pidfile = tmp_path / "dictation.pid"
    monkeypatch.setattr(dictate, "PIDFILE", str(pidfile))
    monkeypatch.setattr(dictate, "LOGFILE", str(tmp_path / "dictation.log"))
    monkeypatch.setattr(dictate, "log", lambda message: None)
    monkeypatch.setattr(dictate, "_get_input_sample_rate", lambda: 16000.0)
    clear_stop_environment(monkeypatch)
    return pidfile


def test_model_exception_cleans_pid_and_resets_state(monkeypatch, tmp_path):
    pidfile = configure_main_test(monkeypatch, tmp_path)

    def fail_model(*args, **kwargs):
        raise RuntimeError("model failed")

    monkeypatch.setattr(dictate, "WhisperModel", fail_model)

    with pytest.raises(RuntimeError, match="model failed"):
        dictate.main()

    assert not pidfile.exists()
    assert not dictate.stop_event.is_set()
    assert dictate.audio_queue.empty()
    assert dictate.chunk_queue.empty()


def test_keyboard_interrupt_cleans_pid_and_resets_state(monkeypatch, tmp_path):
    pidfile = configure_main_test(monkeypatch, tmp_path)

    def interrupt():
        raise KeyboardInterrupt

    monkeypatch.setattr(dictate, "_get_input_sample_rate", interrupt)

    with pytest.raises(KeyboardInterrupt):
        dictate.main()

    assert not pidfile.exists()
    assert not dictate.stop_event.is_set()
    assert dictate.audio_queue.empty()
    assert dictate.chunk_queue.empty()


def test_cli_keyboard_interrupt_does_not_leave_stop_state_set(monkeypatch):
    monkeypatch.setattr(
        dictate,
        "main",
        lambda *args, **kwargs: (_ for _ in ()).throw(KeyboardInterrupt()),
    )
    monkeypatch.setattr(dictate, "log", lambda message: None)
    dictate._reset_runtime_state()

    assert dictate.cli_main([]) == 130
    assert not dictate.stop_event.is_set()


def test_thread_startup_exception_joins_started_threads(monkeypatch, tmp_path):
    pidfile = configure_main_test(monkeypatch, tmp_path)
    monkeypatch.setattr(dictate, "WhisperModel", lambda *args, **kwargs: object())
    threads = []

    class FakeThread:
        def __init__(self, target, args, daemon):
            self.joined = False
            threads.append(self)

        def start(self):
            pass

        def join(self):
            self.joined = True

    monkeypatch.setattr(dictate, "Thread", FakeThread)
    monkeypatch.setattr(
        dictate,
        "ThreadPoolExecutor",
        lambda max_workers: (_ for _ in ()).throw(RuntimeError("executor failed")),
    )

    with pytest.raises(RuntimeError, match="executor failed"):
        dictate.main()

    assert not pidfile.exists()
    assert len(threads) == 2
    assert all(thread.joined for thread in threads)


def test_repeated_runs_use_fresh_state_and_clean_resources(monkeypatch, tmp_path):
    pidfile = configure_main_test(monkeypatch, tmp_path)
    monkeypatch.setattr(dictate, "WhisperModel", lambda *args, **kwargs: object())
    threads = []
    executors = []

    class FakeThread:
        def __init__(self, target, args, daemon):
            self.run_stop_event = args[-1]
            self.joined = False
            threads.append(self)

        def start(self):
            self.run_stop_event.set()

        def join(self):
            self.joined = True

    class FakeExecutor:
        def __init__(self, max_workers):
            self.shutdown_called = False
            executors.append(self)

        def shutdown(self, wait, cancel_futures):
            self.shutdown_called = wait and cancel_futures

    monkeypatch.setattr(dictate, "Thread", FakeThread)
    monkeypatch.setattr(dictate, "ThreadPoolExecutor", FakeExecutor)

    assert dictate.main() == 0
    first_public_event = dictate.stop_event
    assert dictate.main() == 0

    assert not pidfile.exists()
    assert len(threads) == 4
    assert all(thread.joined for thread in threads)
    assert all(executor.shutdown_called for executor in executors)
    assert dictate.stop_event is not first_public_event
    assert not dictate.stop_event.is_set()
    assert dictate.audio_queue.empty()
    assert dictate.chunk_queue.empty()
