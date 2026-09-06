from __future__ import annotations

import asyncio

import pytest

from zara.transcript_normalization import (
    IdentityTranscriptNormalizer,
    NormalizationStatus,
    TranscriptNormalizationRegistry,
    TranscriptNormalizationRequest,
    TranscriptNormalizationResult,
    TranscriptNormalizationService,
    TranscriptNormalizerUnavailable,
    select_normalized_text,
)


def _request(text: str = "Um, hello") -> TranscriptNormalizationRequest:
    return TranscriptNormalizationRequest(
        raw_transcript=text,
        language="en",
        turn_id="turn-1",
        trace_id="trace-1",
    )


@pytest.mark.asyncio
async def test_identity_backend_preserves_final_transcript_byte_for_byte():
    service = TranscriptNormalizationService()
    result = await service.normalize(_request("  Um... hello!  "), backend="off")
    assert result == TranscriptNormalizationResult(
        text="  Um... hello!  ",
        status=NormalizationStatus.SUCCESS,
        backend="off",
        model="",
        version="1",
    )


def test_registry_rejects_duplicate_backend_and_unknown_lookup():
    registry = TranscriptNormalizationRegistry()
    registry.register(IdentityTranscriptNormalizer())
    with pytest.raises(ValueError, match="already registered"):
        registry.register(IdentityTranscriptNormalizer())
    with pytest.raises(TranscriptNormalizerUnavailable, match="unknown"):
        registry.get("missing")


@pytest.mark.asyncio
async def test_service_rejects_oversized_input_before_provider_execution():
    entered = False

    class Provider:
        name = "fake"
        model = "fake-model"
        version = "1"

        async def normalize(self, request):
            nonlocal entered
            entered = True
            return request.raw_transcript

    registry = TranscriptNormalizationRegistry([Provider()])
    service = TranscriptNormalizationService(registry=registry, max_input_chars=8)
    result = await service.normalize(_request("x" * 9), backend="fake")
    assert result.status is NormalizationStatus.INVALID_OUTPUT
    assert result.text == ""
    assert entered is False


@pytest.mark.asyncio
async def test_provider_timeout_is_typed_and_bounded():
    class Provider:
        name = "slow"
        model = "slow-model"
        version = "7"

        async def normalize(self, request):
            await asyncio.sleep(10)
            return request.raw_transcript

    service = TranscriptNormalizationService(registry=TranscriptNormalizationRegistry([Provider()]))
    result = await service.normalize(_request(), backend="slow", timeout=0.01)
    assert result.status is NormalizationStatus.TIMEOUT
    assert result.text == ""
    assert result.backend == "slow"
    assert result.model == "slow-model"
    assert result.version == "7"


@pytest.mark.asyncio
async def test_provider_unavailable_and_unsupported_language_are_explicit():
    class Provider:
        name = "english-only"
        model = "fake"
        version = "1"

        async def normalize(self, request):
            return TranscriptNormalizationResult(
                text="",
                status=NormalizationStatus.UNSUPPORTED_LANGUAGE,
                backend=self.name,
                model=self.model,
                version=self.version,
            )

    service = TranscriptNormalizationService(registry=TranscriptNormalizationRegistry([Provider()]))
    unsupported = await service.normalize(
        TranscriptNormalizationRequest("bonjour", "fr", "turn-2"), backend="english-only"
    )
    unavailable = await service.normalize(_request(), backend="missing")
    assert unsupported.status is NormalizationStatus.UNSUPPORTED_LANGUAGE
    assert unavailable.status is NormalizationStatus.UNAVAILABLE


@pytest.mark.asyncio
async def test_stale_turn_fails_closed_before_and_after_provider_execution():
    current = {"turn-1"}
    entered = asyncio.Event()
    release = asyncio.Event()

    class Provider:
        name = "fake"
        model = "fake"
        version = "1"

        async def normalize(self, request):
            entered.set()
            await release.wait()
            return "clean"

    service = TranscriptNormalizationService(
        registry=TranscriptNormalizationRegistry([Provider()]),
        is_turn_current=lambda turn_id: turn_id in current,
    )
    stale_before = await service.normalize(
        TranscriptNormalizationRequest("old", "en", "turn-old"), backend="fake"
    )
    task = asyncio.create_task(service.normalize(_request(), backend="fake"))
    await asyncio.wait_for(entered.wait(), timeout=1)
    current.clear()
    release.set()
    stale_after = await task
    assert stale_before.status is NormalizationStatus.CANCELLED
    assert stale_after.status is NormalizationStatus.CANCELLED
    assert stale_after.text == ""


@pytest.mark.asyncio
async def test_oversized_or_empty_provider_output_is_invalid():
    class Provider:
        name = "fake"
        model = "fake"
        version = "1"

        def __init__(self, value):
            self.value = value

        async def normalize(self, request):
            return self.value

    registry = TranscriptNormalizationRegistry([Provider("")])
    service = TranscriptNormalizationService(registry=registry, max_output_chars=4)
    empty = await service.normalize(_request(), backend="fake")
    registry.replace(Provider("12345"))
    oversized = await service.normalize(_request(), backend="fake")
    assert empty.status is NormalizationStatus.INVALID_OUTPUT
    assert oversized.status is NormalizationStatus.INVALID_OUTPUT


def test_fallback_policy_is_explicit_and_never_changes_authority():
    raw = "don't delete it"
    failed = TranscriptNormalizationResult(
        text="", status=NormalizationStatus.TIMEOUT, backend="fake"
    )
    success = TranscriptNormalizationResult(
        text="do not delete it", status=NormalizationStatus.SUCCESS, backend="fake"
    )
    assert select_normalized_text(raw, success, failure_policy="raw") == "do not delete it"
    assert select_normalized_text(raw, failed, failure_policy="raw") == raw
    with pytest.raises(RuntimeError, match="normalization failed"):
        select_normalized_text(raw, failed, failure_policy="fail-turn")
    with pytest.raises(ValueError, match="failure policy"):
        select_normalized_text(raw, failed, failure_policy="guess")
