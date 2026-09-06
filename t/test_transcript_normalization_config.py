from __future__ import annotations

import math

import pytest

from zara.transcript_normalization_config import (
    TranscriptNormalizationConfig,
    TranscriptNormalizationConfigError,
)


def test_defaults_keep_normalization_off_and_local():
    config = TranscriptNormalizationConfig.from_mapping(None)
    assert config.backend == "off"
    assert config.failure_policy == "raw"
    assert config.language_policy == "auto"
    assert config.s1_mini.endpoint == "http://127.0.0.1:11434"
    assert config.s1_mini.is_local is True


def test_valid_s1_mini_selection_projects_provider_values():
    config = TranscriptNormalizationConfig.from_mapping(
        {
            "backend": "s1-mini",
            "failure_policy": "fail-turn",
            "s1_mini": {
                "runtime": "openai-compatible",
                "endpoint": "http://localhost:9000",
                "model": "local/s1-mini-q4",
                "styling": "formal",
                "structure": "lists",
                "context": "email",
                "timeout_ms": 1250,
            },
        }
    )
    assert config.backend == "s1-mini"
    assert config.failure_policy == "fail-turn"
    assert config.s1_mini.model == "local/s1-mini-q4"
    assert config.s1_mini.styling == "formal"
    assert config.s1_mini.structure == "lists"
    assert config.s1_mini.context == "email"
    assert config.s1_mini.timeout_seconds == 1.25
    assert config.s1_mini.is_local is True


@pytest.mark.parametrize("backend", ["s1", "remote", "", 1, True])
def test_unknown_backend_is_rejected(backend):
    with pytest.raises(TranscriptNormalizationConfigError, match="backend"):
        TranscriptNormalizationConfig.from_mapping({"backend": backend})


@pytest.mark.parametrize("policy", ["fallback", "ignore", "", 1])
def test_unknown_failure_policy_is_rejected(policy):
    with pytest.raises(TranscriptNormalizationConfigError, match="failure_policy"):
        TranscriptNormalizationConfig.from_mapping({"failure_policy": policy})


@pytest.mark.parametrize("runtime", ["ollama", "http", "", 1])
def test_unknown_s1_runtime_is_rejected(runtime):
    with pytest.raises(TranscriptNormalizationConfigError, match="runtime"):
        TranscriptNormalizationConfig.from_mapping(
            {"backend": "s1-mini", "s1_mini": {"runtime": runtime}}
        )


@pytest.mark.parametrize(
    "endpoint",
    [
        "127.0.0.1:11434",
        "file:///tmp/model",
        "http://user:secret@127.0.0.1:11434",
        "http://127.0.0.1:11434/v1",
        "http://127.0.0.1:11434/?token=secret",
        "http://127.0.0.1:11434/#fragment",
    ],
)
def test_endpoint_requires_credential_free_origin(endpoint):
    with pytest.raises(TranscriptNormalizationConfigError, match="endpoint"):
        TranscriptNormalizationConfig.from_mapping(
            {"backend": "s1-mini", "s1_mini": {"endpoint": endpoint}}
        )


@pytest.mark.parametrize(
    ("endpoint", "is_local"),
    [
        ("http://localhost:11434", True),
        ("http://127.0.0.1:11434", True),
        ("http://[::1]:11434", True),
        ("https://normalizer.example.test", False),
        ("http://10.70.70.20:11434", False),
    ],
)
def test_endpoint_locality_is_explicit(endpoint, is_local):
    config = TranscriptNormalizationConfig.from_mapping(
        {"backend": "s1-mini", "s1_mini": {"endpoint": endpoint}}
    )
    assert config.s1_mini.is_local is is_local


@pytest.mark.parametrize(
    "timeout_ms",
    [49, 30001, 0, -1, math.nan, math.inf, -math.inf, True],
)
def test_timeout_is_finite_and_bounded(timeout_ms):
    with pytest.raises(TranscriptNormalizationConfigError, match="timeout_ms"):
        TranscriptNormalizationConfig.from_mapping(
            {"backend": "s1-mini", "s1_mini": {"timeout_ms": timeout_ms}}
        )


@pytest.mark.parametrize(
    ("field", "value"),
    [
        ("styling", "balanced"),
        ("structure", "table"),
        ("context", "shell"),
    ],
)
def test_provider_controls_match_the_landed_s1_contract(field, value):
    with pytest.raises(TranscriptNormalizationConfigError, match=field):
        TranscriptNormalizationConfig.from_mapping(
            {"backend": "s1-mini", "s1_mini": {field: value}}
        )
