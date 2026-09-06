"""Provider-neutral post-STT transcript normalization contract.

This module owns normalization mechanics only. Voice routing, persistence, and
fallback policy remain above this boundary so a normalizer cannot gain tool or
action authority.
"""

from __future__ import annotations

import asyncio
import enum
from dataclasses import dataclass
from typing import Any, Callable, Iterable, Optional


DEFAULT_MAX_INPUT_CHARS = 32768
DEFAULT_MAX_OUTPUT_CHARS = 32768
DEFAULT_TIMEOUT_SECONDS = 2.0
MAX_BACKEND_NAME_LENGTH = 64
MAX_MODEL_ID_LENGTH = 128
MAX_VERSION_LENGTH = 64


class NormalizationStatus(str, enum.Enum):
    SUCCESS = "success"
    UNAVAILABLE = "unavailable"
    UNSUPPORTED_LANGUAGE = "unsupported_language"
    TIMEOUT = "timeout"
    CANCELLED = "cancelled"
    INVALID_OUTPUT = "invalid_output"


class TranscriptNormalizerUnavailable(RuntimeError):
    """The requested normalization backend is not registered or ready."""


@dataclass(frozen=True)
class TranscriptNormalizationRequest:
    raw_transcript: str
    language: str
    turn_id: str
    trace_id: str = ""

    def __post_init__(self) -> None:
        if not isinstance(self.raw_transcript, str):
            raise TypeError("raw transcript must be text")
        if not isinstance(self.language, str):
            raise TypeError("normalization language must be text")
        if not isinstance(self.turn_id, str) or not self.turn_id:
            raise ValueError("normalization turn_id must be non-empty text")
        if not isinstance(self.trace_id, str):
            raise TypeError("normalization trace_id must be text")


@dataclass(frozen=True)
class TranscriptNormalizationResult:
    text: str
    status: NormalizationStatus
    backend: str
    model: str = ""
    version: str = ""


class IdentityTranscriptNormalizer:
    """The canonical off backend; byte-preserving and side-effect free."""

    name = "off"
    model = ""
    version = "1"

    async def normalize(self, request: TranscriptNormalizationRequest) -> str:
        return request.raw_transcript


class TranscriptNormalizationRegistry:
    """Small explicit provider registry without coordinator if/elif growth."""

    def __init__(self, providers: Iterable[Any] = ()) -> None:
        self._providers: dict[str, Any] = {}
        for provider in providers:
            self.register(provider)

    @staticmethod
    def _provider_name(provider: Any) -> str:
        name = getattr(provider, "name", None)
        if not isinstance(name, str) or not name or len(name) > MAX_BACKEND_NAME_LENGTH:
            raise ValueError(
                f"normalizer backend name must contain 1 to {MAX_BACKEND_NAME_LENGTH} characters"
            )
        normalize = getattr(provider, "normalize", None)
        if not callable(normalize):
            raise TypeError("normalizer backend must define normalize(request)")
        return name

    def register(self, provider: Any) -> None:
        name = self._provider_name(provider)
        if name in self._providers:
            raise ValueError(f"normalizer backend {name!r} is already registered")
        self._providers[name] = provider

    def replace(self, provider: Any) -> None:
        name = self._provider_name(provider)
        self._providers[name] = provider

    def get(self, name: str) -> Any:
        provider = self._providers.get(name)
        if provider is None:
            raise TranscriptNormalizerUnavailable(
                f"unknown transcript normalizer backend {name!r}"
            )
        return provider

    def names(self) -> tuple[str, ...]:
        return tuple(sorted(self._providers))


class TranscriptNormalizationService:
    """Bounded normalization execution with stale-turn fencing."""

    def __init__(
        self,
        *,
        registry: Optional[TranscriptNormalizationRegistry] = None,
        max_input_chars: int = DEFAULT_MAX_INPUT_CHARS,
        max_output_chars: int = DEFAULT_MAX_OUTPUT_CHARS,
        is_turn_current: Optional[Callable[[str], bool]] = None,
    ) -> None:
        if max_input_chars <= 0 or max_output_chars <= 0:
            raise ValueError("normalization bounds must be positive")
        self.registry = registry or TranscriptNormalizationRegistry()
        if "off" not in self.registry.names():
            self.registry.register(IdentityTranscriptNormalizer())
        self.max_input_chars = int(max_input_chars)
        self.max_output_chars = int(max_output_chars)
        self._is_turn_current = is_turn_current or (lambda _turn_id: True)

    async def normalize(
        self,
        request: TranscriptNormalizationRequest,
        *,
        backend: str = "off",
        timeout: float = DEFAULT_TIMEOUT_SECONDS,
    ) -> TranscriptNormalizationResult:
        if not isinstance(request, TranscriptNormalizationRequest):
            raise TypeError("request must be a TranscriptNormalizationRequest")
        if isinstance(timeout, bool) or not isinstance(timeout, (int, float)):
            raise TypeError("normalization timeout must be numeric")
        timeout_value = float(timeout)
        if timeout_value <= 0:
            raise ValueError("normalization timeout must be positive")

        if len(request.raw_transcript) > self.max_input_chars:
            return self._result(NormalizationStatus.INVALID_OUTPUT, backend)
        if not self._is_turn_current(request.turn_id):
            return self._result(NormalizationStatus.CANCELLED, backend)

        try:
            provider = self.registry.get(backend)
        except TranscriptNormalizerUnavailable:
            return self._result(NormalizationStatus.UNAVAILABLE, backend)

        try:
            raw_result = await asyncio.wait_for(
                provider.normalize(request),
                timeout=timeout_value,
            )
        except TimeoutError:
            return self._provider_result(provider, NormalizationStatus.TIMEOUT)
        except TranscriptNormalizerUnavailable:
            return self._provider_result(provider, NormalizationStatus.UNAVAILABLE)

        if not self._is_turn_current(request.turn_id):
            return self._provider_result(provider, NormalizationStatus.CANCELLED)

        if isinstance(raw_result, TranscriptNormalizationResult):
            status = raw_result.status
            text = raw_result.text
        else:
            status = NormalizationStatus.SUCCESS
            text = raw_result

        if not isinstance(status, NormalizationStatus):
            return self._provider_result(provider, NormalizationStatus.INVALID_OUTPUT)
        if status is not NormalizationStatus.SUCCESS:
            return self._provider_result(provider, status)
        if not isinstance(text, str) or not text or len(text) > self.max_output_chars:
            return self._provider_result(provider, NormalizationStatus.INVALID_OUTPUT)

        return self._provider_result(provider, NormalizationStatus.SUCCESS, text=text)

    def _result(
        self,
        status: NormalizationStatus,
        backend: str,
        *,
        text: str = "",
    ) -> TranscriptNormalizationResult:
        return TranscriptNormalizationResult(text=text, status=status, backend=backend)

    def _provider_result(
        self,
        provider: Any,
        status: NormalizationStatus,
        *,
        text: str = "",
    ) -> TranscriptNormalizationResult:
        return TranscriptNormalizationResult(
            text=text,
            status=status,
            backend=str(getattr(provider, "name", ""))[:MAX_BACKEND_NAME_LENGTH],
            model=str(getattr(provider, "model", ""))[:MAX_MODEL_ID_LENGTH],
            version=str(getattr(provider, "version", ""))[:MAX_VERSION_LENGTH],
        )


def select_normalized_text(
    raw_transcript: str,
    result: TranscriptNormalizationResult,
    *,
    failure_policy: str,
) -> str:
    """Apply an explicit caller-owned failure policy to a normalization result."""

    if failure_policy not in {"raw", "fail-turn"}:
        raise ValueError("normalization failure policy must be 'raw' or 'fail-turn'")
    if result.status is NormalizationStatus.SUCCESS:
        return result.text
    if failure_policy == "raw":
        return raw_transcript
    raise RuntimeError(f"transcript normalization failed: {result.status.value}")


__all__ = [
    "DEFAULT_MAX_INPUT_CHARS",
    "DEFAULT_MAX_OUTPUT_CHARS",
    "DEFAULT_TIMEOUT_SECONDS",
    "IdentityTranscriptNormalizer",
    "NormalizationStatus",
    "TranscriptNormalizationRegistry",
    "TranscriptNormalizationRequest",
    "TranscriptNormalizationResult",
    "TranscriptNormalizationService",
    "TranscriptNormalizerUnavailable",
    "select_normalized_text",
]
