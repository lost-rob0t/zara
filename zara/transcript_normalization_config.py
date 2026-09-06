"""Validated configuration descriptor for transcript normalization."""

from __future__ import annotations

import ipaddress
import math
from dataclasses import dataclass
from urllib.parse import urlsplit


_BACKENDS = {"off", "s1-mini"}
_FAILURE_POLICIES = {"raw", "fail-turn"}
_STYLING = {"casual", "semi-casual", "semi-formal", "formal"}
_STRUCTURE = {"prose", "lists"}
_CONTEXT = {"general", "email"}


class TranscriptNormalizationConfigError(ValueError):
    """Raised when transcript-normalizer configuration is invalid."""


@dataclass(frozen=True)
class S1MiniConfig:
    endpoint: str = "http://127.0.0.1:11434"
    model: str = "superwhisper/s1-mini"
    styling: str = "semi-formal"
    structure: str = "prose"
    context: str = "general"
    timeout_seconds: float = 1.0

    @property
    def is_local(self) -> bool:
        host = urlsplit(self.endpoint).hostname
        if host in {"localhost", "localhost.localdomain"}:
            return True
        try:
            return ipaddress.ip_address(host or "").is_loopback
        except ValueError:
            return False


@dataclass(frozen=True)
class TranscriptNormalizationConfig:
    backend: str = "off"
    failure_policy: str = "raw"
    language_policy: str = "auto"
    s1_mini: S1MiniConfig = S1MiniConfig()

    @classmethod
    def from_mapping(cls, value: object) -> "TranscriptNormalizationConfig":
        if value is None:
            return cls()
        if not isinstance(value, dict):
            raise TranscriptNormalizationConfigError(
                "voice.transcript_normalization must be a TOML table"
            )
        backend = _choice("backend", value.get("backend", "off"), _BACKENDS)
        failure_policy = _choice(
            "failure_policy", value.get("failure_policy", "raw"), _FAILURE_POLICIES
        )
        language_policy = value.get("language_policy", "auto")
        if language_policy != "auto":
            raise TranscriptNormalizationConfigError("language_policy must be 'auto'")
        raw_s1 = value.get("s1_mini", {})
        if not isinstance(raw_s1, dict):
            raise TranscriptNormalizationConfigError("s1_mini must be a TOML table")
        runtime = raw_s1.get("runtime", "openai-compatible")
        if runtime != "openai-compatible":
            raise TranscriptNormalizationConfigError(
                "s1_mini.runtime must be 'openai-compatible'"
            )
        endpoint = _endpoint(raw_s1.get("endpoint", "http://127.0.0.1:11434"))
        model = _text("s1_mini.model", raw_s1.get("model", "superwhisper/s1-mini"), 256)
        styling = _choice("s1_mini.styling", raw_s1.get("styling", "semi-formal"), _STYLING)
        structure = _choice("s1_mini.structure", raw_s1.get("structure", "prose"), _STRUCTURE)
        context = _choice("s1_mini.context", raw_s1.get("context", "general"), _CONTEXT)
        timeout_ms = raw_s1.get("timeout_ms", 1000)
        if isinstance(timeout_ms, bool) or not isinstance(timeout_ms, (int, float)):
            raise TranscriptNormalizationConfigError("s1_mini.timeout_ms must be numeric")
        timeout = float(timeout_ms)
        if not math.isfinite(timeout) or not 50 <= timeout <= 30000:
            raise TranscriptNormalizationConfigError(
                "s1_mini.timeout_ms must be finite and between 50 and 30000"
            )
        return cls(
            backend=backend,
            failure_policy=failure_policy,
            language_policy=language_policy,
            s1_mini=S1MiniConfig(
                endpoint=endpoint,
                model=model,
                styling=styling,
                structure=structure,
                context=context,
                timeout_seconds=timeout / 1000.0,
            ),
        )


def _choice(field: str, value: object, allowed: set[str]) -> str:
    if not isinstance(value, str) or value not in allowed:
        raise TranscriptNormalizationConfigError(
            f"{field} must be one of {', '.join(sorted(allowed))}"
        )
    return value


def _text(field: str, value: object, maximum: int) -> str:
    if not isinstance(value, str) or not value.strip() or len(value) > maximum:
        raise TranscriptNormalizationConfigError(f"{field} must be bounded non-empty text")
    return value.strip()


def _endpoint(value: object) -> str:
    endpoint = _text("s1_mini.endpoint", value, 2048).rstrip("/")
    parsed = urlsplit(endpoint)
    if parsed.scheme not in {"http", "https"} or not parsed.hostname:
        raise TranscriptNormalizationConfigError(
            "s1_mini.endpoint must be an absolute http(s) origin"
        )
    if parsed.username is not None or parsed.password is not None:
        raise TranscriptNormalizationConfigError(
            "s1_mini.endpoint must not contain credentials"
        )
    if parsed.path not in {"", "/"} or parsed.query or parsed.fragment:
        raise TranscriptNormalizationConfigError(
            "s1_mini.endpoint must be an origin without /v1, query, or fragment"
        )
    return endpoint


__all__ = [
    "S1MiniConfig",
    "TranscriptNormalizationConfig",
    "TranscriptNormalizationConfigError",
]
