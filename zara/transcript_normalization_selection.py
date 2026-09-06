"""Canonical transcript-normalizer selection and bounded status projection.

Selection reads Zara's existing TOML authority and builds the provider-neutral
normalization service.  It deliberately stores status metadata only: transcript
text and credentials never enter diagnostics.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any

from zara.s1_mini_normalizer import S1MiniTranscriptNormalizer
from zara.transcript_normalization import (
    NormalizationStatus,
    TranscriptNormalizationRegistry,
    TranscriptNormalizationResult,
    TranscriptNormalizationService,
)
from zara.transcript_normalization_config import TranscriptNormalizationConfig


_READY = {NormalizationStatus.SUCCESS}
_UNAVAILABLE = {NormalizationStatus.UNAVAILABLE}


@dataclass
class TranscriptNormalizationSelection:
    """One immutable provider selection plus non-sensitive runtime status."""

    configuration: TranscriptNormalizationConfig
    service: TranscriptNormalizationService
    _last_status: NormalizationStatus | None = None

    @classmethod
    def from_config(cls, config: Any) -> "TranscriptNormalizationSelection":
        """Select from ZaraConfig without creating another configuration store."""
        get = getattr(config, "get", None)
        if not callable(get):
            raise TypeError("config must provide ZaraConfig-compatible get(section, key, default)")

        raw = get("voice", "transcript_normalization", None)
        selected = TranscriptNormalizationConfig.from_mapping(raw)
        registry = TranscriptNormalizationRegistry()

        if selected.backend == "s1-mini":
            provider = S1MiniTranscriptNormalizer(
                endpoint=selected.s1_mini.endpoint,
                model=selected.s1_mini.model,
                styling=selected.s1_mini.styling,
                structure=selected.s1_mini.structure,
                context=selected.s1_mini.context,
                request_timeout=selected.s1_mini.timeout_seconds,
            )
            registry.register(provider)

        return cls(
            configuration=selected,
            service=TranscriptNormalizationService(registry=registry),
        )

    def record_result(self, result: TranscriptNormalizationResult) -> None:
        """Record bounded status only; never retain transcript contents."""
        if not isinstance(result, TranscriptNormalizationResult):
            raise TypeError("result must be a TranscriptNormalizationResult")
        if not isinstance(result.status, NormalizationStatus):
            raise TypeError("result.status must be a NormalizationStatus")
        if result.backend != self.configuration.backend:
            raise ValueError("normalization result backend does not match selected backend")
        self._last_status = result.status

    def diagnostics(self) -> dict[str, str | None]:
        """Return a credential- and transcript-free operator status snapshot."""
        backend = self.configuration.backend
        if backend == "off":
            return {
                "backend": "off",
                "model": "",
                "endpoint_locality": "local",
                "state": "ready",
                "last_error_class": None,
                "reload": "restart-required",
            }

        status = self._last_status
        if status in _READY:
            state = "ready"
            error = None
        elif status in _UNAVAILABLE:
            state = "unavailable"
            error = status.value
        elif status is None:
            state = "degraded"
            error = None
        else:
            state = "degraded"
            error = status.value

        return {
            "backend": backend,
            "model": self.configuration.s1_mini.model,
            "endpoint_locality": (
                "local" if self.configuration.s1_mini.is_local else "remote"
            ),
            "state": state,
            "last_error_class": error,
            "reload": "restart-required",
        }


__all__ = ["TranscriptNormalizationSelection"]
