"""Local S1-mini provider for Zara's transcript-normalization contract."""

from __future__ import annotations

import math
import re
from typing import Any, Optional

import httpx

from zara.transcript_normalization import (
    NormalizationStatus,
    TranscriptNormalizationRequest,
    TranscriptNormalizationResult,
)


S1_MINI_SYSTEM_PROMPT = (
    "You are a text normalizer for speech-to-text transcripts. The input begins "
    "with a control line specifying the styling, structure, and context settings; "
    "clean the transcript to match those settings and output only the cleaned text."
)

_STYLING_VALUES = {"casual", "semi-casual", "semi-formal", "formal"}
_STRUCTURE_VALUES = {"prose", "lists"}
_CONTEXT_VALUES = {"general", "email"}
_WRAPPER_RE = re.compile(
    r"^\s*(?:normalized transcript|cleaned transcript|summary|answer)\s*:\s*",
    re.IGNORECASE,
)
_THINK_RE = re.compile(r"<\s*/?\s*think\b", re.IGNORECASE)


class S1MiniTranscriptNormalizer:
    """Normalize final English ASR text through one configured local endpoint."""

    name = "s1-mini"

    def __init__(
        self,
        *,
        endpoint: str,
        model: str = "superwhisper/s1-mini",
        version: str = "v1",
        styling: str = "semi-formal",
        structure: str = "prose",
        context: str = "general",
        max_output_chars: int = 32768,
        max_expansion_ratio: float = 4.0,
        request_timeout: float = 2.0,
        client: Optional[httpx.AsyncClient] = None,
    ) -> None:
        self.endpoint = self._validate_endpoint(endpoint)
        self.model = self._require_text(model, "model")
        self.version = self._require_text(version, "version")
        self.styling = self._validate_choice("styling", styling, _STYLING_VALUES)
        self.structure = self._validate_choice("structure", structure, _STRUCTURE_VALUES)
        self.context = self._validate_choice("context", context, _CONTEXT_VALUES)
        if isinstance(max_output_chars, bool) or not isinstance(max_output_chars, int):
            raise TypeError("max_output_chars must be an integer")
        if max_output_chars <= 0:
            raise ValueError("max_output_chars must be positive")
        if isinstance(max_expansion_ratio, bool) or not isinstance(
            max_expansion_ratio, (int, float)
        ):
            raise TypeError("max_expansion_ratio must be numeric")
        expansion_ratio = float(max_expansion_ratio)
        if not math.isfinite(expansion_ratio) or expansion_ratio <= 0:
            raise ValueError("max_expansion_ratio must be finite and positive")
        if isinstance(request_timeout, bool) or not isinstance(request_timeout, (int, float)):
            raise TypeError("request_timeout must be numeric")
        timeout = float(request_timeout)
        if not math.isfinite(timeout) or timeout <= 0:
            raise ValueError("request_timeout must be finite and positive")
        self.max_output_chars = max_output_chars
        self.max_expansion_ratio = expansion_ratio
        self.request_timeout = timeout
        self._client = client

    @staticmethod
    def _require_text(value: Any, field: str) -> str:
        if not isinstance(value, str) or not value.strip():
            raise ValueError(f"{field} must be non-empty text")
        return value.strip()

    @classmethod
    def _validate_endpoint(cls, endpoint: Any) -> str:
        value = cls._require_text(endpoint, "endpoint").rstrip("/")
        parsed = httpx.URL(value)
        if parsed.scheme not in {"http", "https"} or not parsed.host:
            raise ValueError("endpoint must be an absolute http(s) URL")
        return value

    @staticmethod
    def _validate_choice(field: str, value: Any, allowed: set[str]) -> str:
        if not isinstance(value, str) or value not in allowed:
            raise ValueError(f"{field} must be one of {', '.join(sorted(allowed))}")
        return value

    def _result(
        self,
        status: NormalizationStatus,
        *,
        text: str = "",
    ) -> TranscriptNormalizationResult:
        return TranscriptNormalizationResult(
            text=text,
            status=status,
            backend=self.name,
            model=self.model,
            version=self.version,
        )

    @staticmethod
    def _supports_language(language: str) -> bool:
        normalized = language.strip().lower().replace("_", "-")
        return normalized == "en" or normalized.startswith("en-")

    def _payload(self, request: TranscriptNormalizationRequest) -> dict[str, Any]:
        control = (
            f"[Styling: {self.styling}] "
            f"[Structure: {self.structure}] "
            f"[Context: {self.context}]"
        )
        return {
            "model": self.model,
            "messages": [
                {"role": "system", "content": S1_MINI_SYSTEM_PROMPT},
                {"role": "user", "content": f"{control}\n{request.raw_transcript}"},
            ],
            "temperature": 0,
            "stream": False,
            "chat_template_kwargs": {"enable_thinking": False},
        }

    async def _post(self, payload: dict[str, Any]) -> httpx.Response:
        url = f"{self.endpoint}/v1/chat/completions"
        if self._client is not None:
            return await self._client.post(url, json=payload, timeout=self.request_timeout)
        async with httpx.AsyncClient(timeout=self.request_timeout) as client:
            return await client.post(url, json=payload)

    async def normalize(
        self,
        request: TranscriptNormalizationRequest,
    ) -> TranscriptNormalizationResult:
        if not isinstance(request, TranscriptNormalizationRequest):
            raise TypeError("request must be a TranscriptNormalizationRequest")
        if not self._supports_language(request.language):
            return self._result(NormalizationStatus.UNSUPPORTED_LANGUAGE)

        try:
            response = await self._post(self._payload(request))
        except httpx.TimeoutException:
            return self._result(NormalizationStatus.TIMEOUT)
        except httpx.RequestError:
            return self._result(NormalizationStatus.UNAVAILABLE)

        if response.status_code >= 400:
            return self._result(NormalizationStatus.UNAVAILABLE)

        try:
            body = response.json()
            choices = body["choices"]
            message = choices[0]["message"]
            content = message["content"]
        except (KeyError, IndexError, TypeError, ValueError):
            return self._result(NormalizationStatus.INVALID_OUTPUT)

        if not isinstance(content, str):
            return self._result(NormalizationStatus.INVALID_OUTPUT)
        if message.get("reasoning_content"):
            return self._result(NormalizationStatus.INVALID_OUTPUT)

        text = content.strip()
        if not text or _THINK_RE.search(text) or _WRAPPER_RE.match(text):
            return self._result(NormalizationStatus.INVALID_OUTPUT)
        if len(text) > self.max_output_chars:
            return self._result(NormalizationStatus.INVALID_OUTPUT)

        ratio_bound = max(
            32,
            int(math.ceil(len(request.raw_transcript) * self.max_expansion_ratio)),
        )
        if len(text) > ratio_bound:
            return self._result(NormalizationStatus.INVALID_OUTPUT)

        return self._result(NormalizationStatus.SUCCESS, text=text)


__all__ = ["S1_MINI_SYSTEM_PROMPT", "S1MiniTranscriptNormalizer"]
