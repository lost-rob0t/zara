import time

import pytest

from zara.whisper_loader import resolve_whisper_model_files


def test_local_model_directory_bypasses_hub(tmp_path):
    messages = []

    resolved = resolve_whisper_model_files(
        str(tmp_path),
        download_model_fn=lambda *_args, **_kwargs: pytest.fail("hub should not be used"),
        log=messages.append,
    )

    assert resolved == str(tmp_path.resolve())
    assert messages == [f"Whisper model files already local: {tmp_path.resolve()}"]


def test_cached_named_model_is_reported_without_network_download():
    calls = []
    messages = []

    def download(model, **kwargs):
        calls.append((model, kwargs))
        assert kwargs == {"local_files_only": True}
        return "/cache/faster-whisper-small"

    resolved = resolve_whisper_model_files(
        "small",
        download_model_fn=download,
        log=messages.append,
    )

    assert resolved == "/cache/faster-whisper-small"
    assert calls == [("small", {"local_files_only": True})]
    assert messages[0] == "Checking local Whisper model cache: small"
    assert messages[1].startswith("Whisper model files cached: small")


def test_cache_miss_reports_download_and_heartbeat():
    calls = []
    messages = []

    def download(model, **kwargs):
        calls.append((model, kwargs))
        if kwargs.get("local_files_only"):
            raise RuntimeError("not cached")
        time.sleep(0.04)
        return "/cache/faster-whisper-small"

    resolved = resolve_whisper_model_files(
        "small",
        download_model_fn=download,
        log=messages.append,
        heartbeat_seconds=0.01,
    )

    assert resolved == "/cache/faster-whisper-small"
    assert calls[0] == ("small", {"local_files_only": True})
    assert calls[1] == ("small", {})
    assert any("not cached; resolving/downloading" in message for message in messages)
    assert any("Still resolving/downloading Whisper small" in message for message in messages)
    assert any("Whisper model files ready: small" in message for message in messages)


def test_download_failure_is_reported_and_reraised():
    messages = []

    def download(_model, **kwargs):
        if kwargs.get("local_files_only"):
            raise RuntimeError("not cached")
        raise RuntimeError("hub unavailable")

    with pytest.raises(RuntimeError, match="hub unavailable"):
        resolve_whisper_model_files(
            "small",
            download_model_fn=download,
            log=messages.append,
            heartbeat_seconds=0.01,
        )

    assert any("download failed" in message for message in messages)


def test_invalid_heartbeat_interval_is_rejected():
    with pytest.raises(ValueError, match="heartbeat_seconds must be positive"):
        resolve_whisper_model_files(
            "small",
            download_model_fn=lambda *_args, **_kwargs: "/unused",
            heartbeat_seconds=0,
        )
