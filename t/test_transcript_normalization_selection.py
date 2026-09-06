from __future__ import annotations

from zara.config import ZaraConfig
from zara.transcript_normalization import NormalizationStatus, TranscriptNormalizationResult
from zara.transcript_normalization_selection import TranscriptNormalizationSelection


def _config(tmp_path, body: str) -> ZaraConfig:
    path = tmp_path / "config.toml"
    path.write_text(body)
    return ZaraConfig(str(path))


def test_zara_config_selects_transcript_normalizer_from_voice_table(tmp_path):
    config = _config(
        tmp_path,
        """
[voice.transcript_normalization]
backend = "s1-mini"
failure_policy = "fail-turn"

[voice.transcript_normalization.s1_mini]
endpoint = "http://127.0.0.1:9000"
model = "local/s1-mini"
styling = "formal"
structure = "lists"
context = "email"
timeout_ms = 1250
""",
    )

    selected = config.get_transcript_normalization_config()
    assert selected.backend == "s1-mini"
    assert selected.failure_policy == "fail-turn"
    assert selected.s1_mini.endpoint == "http://127.0.0.1:9000"
    assert selected.s1_mini.timeout_seconds == 1.25


def test_normalizer_selection_reports_honest_local_status_without_transcript_data(tmp_path):
    config = _config(
        tmp_path,
        """
[voice.transcript_normalization]
backend = "s1-mini"

[voice.transcript_normalization.s1_mini]
endpoint = "http://127.0.0.1:9000"
model = "local/s1-mini"
""",
    )
    selection = TranscriptNormalizationSelection.from_config(config)

    initial = selection.diagnostics()
    assert initial == {
        "backend": "s1-mini",
        "model": "local/s1-mini",
        "endpoint_locality": "local",
        "state": "degraded",
        "last_error_class": None,
        "reload": "restart-required",
    }

    selection.record_result(
        TranscriptNormalizationResult(
            text="cleaned private transcript",
            status=NormalizationStatus.SUCCESS,
            backend="s1-mini",
            model="local/s1-mini",
            version="v1",
        )
    )
    ready = selection.diagnostics()
    assert ready["state"] == "ready"
    assert "cleaned private transcript" not in repr(ready)

    selection.record_result(
        TranscriptNormalizationResult(
            text="",
            status=NormalizationStatus.UNAVAILABLE,
            backend="s1-mini",
            model="local/s1-mini",
            version="v1",
        )
    )
    unavailable = selection.diagnostics()
    assert unavailable["state"] == "unavailable"
    assert unavailable["last_error_class"] == "unavailable"


def test_remote_normalizer_is_explicitly_nonlocal_and_off_is_identity_ready(tmp_path):
    remote = _config(
        tmp_path,
        """
[voice.transcript_normalization]
backend = "s1-mini"

[voice.transcript_normalization.s1_mini]
endpoint = "https://normalizer.example.test"
""",
    )
    assert TranscriptNormalizationSelection.from_config(remote).diagnostics()[
        "endpoint_locality"
    ] == "remote"

    off = _config(tmp_path, "[voice.transcript_normalization]\nbackend = \"off\"\n")
    assert TranscriptNormalizationSelection.from_config(off).diagnostics() == {
        "backend": "off",
        "model": "",
        "endpoint_locality": "local",
        "state": "ready",
        "last_error_class": None,
        "reload": "restart-required",
    }
