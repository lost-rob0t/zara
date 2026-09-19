from __future__ import annotations

import pytest

from zara.config import ConfigError, ZaraConfig


def validate_runtime_endpoint(endpoint: str) -> None:
    config = {
        "runtime": {
            "backend": "zara-python",
            "prolog_rlm_endpoint": endpoint,
        }
    }
    ZaraConfig.__new__(ZaraConfig)._validate_config(config)


@pytest.mark.parametrize(
    "endpoint",
    (
        "http://localhost:18765",
        "http://example.test:18765",
        "http://192.168.1.10:18765",
        "https://127.0.0.1:18765",
        "http://user:secret@127.0.0.1:18765",
        "http://127.0.0.1:18765?token=secret",
    ),
)
def test_runtime_config_rejects_nonliteral_or_authority_bearing_sidecar_endpoints(
    endpoint: str,
) -> None:
    with pytest.raises(ConfigError, match="explicit literal-loopback HTTP"):
        validate_runtime_endpoint(endpoint)


@pytest.mark.parametrize(
    "endpoint",
    (
        "http://127.0.0.1:18765",
        "http://[::1]:18765",
    ),
)
def test_runtime_config_accepts_literal_loopback_sidecar_endpoints(endpoint: str) -> None:
    validate_runtime_endpoint(endpoint)
