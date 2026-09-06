from __future__ import annotations

import httpx
import pytest

from zara.s1_mini_normalizer import S1_MINI_SYSTEM_PROMPT, S1MiniTranscriptNormalizer
from zara.transcript_normalization import (
    NormalizationStatus,
    TranscriptNormalizationRequest,
    TranscriptNormalizationService,
)


def _request(text: str = "um send the report friday") -> TranscriptNormalizationRequest:
    return TranscriptNormalizationRequest(
        raw_transcript=text,
        language="en",
        turn_id="turn-1",
        trace_id="trace-1",
    )


def _client(handler) -> httpx.AsyncClient:
    return httpx.AsyncClient(transport=httpx.MockTransport(handler))


@pytest.mark.asyncio
async def test_s1_mini_builds_exact_deterministic_openai_request():
    seen = []

    def handler(request: httpx.Request) -> httpx.Response:
        seen.append(request)
        body = __import__("json").loads(request.content)
        assert request.url == "http://127.0.0.1:8000/v1/chat/completions"
        assert body == {
            "model": "superwhisper/s1-mini",
            "messages": [
                {"role": "system", "content": S1_MINI_SYSTEM_PROMPT},
                {
                    "role": "user",
                    "content": (
                        "[Styling: semi-formal] [Structure: prose] [Context: general]\n"
                        "um send the report friday"
                    ),
                },
            ],
            "temperature": 0,
            "stream": False,
            "chat_template_kwargs": {"enable_thinking": False},
        }
        return httpx.Response(
            200,
            json={"choices": [{"message": {"content": "Send the report Friday."}}]},
        )

    async with _client(handler) as client:
        provider = S1MiniTranscriptNormalizer(
            endpoint="http://127.0.0.1:8000",
            client=client,
        )
        result = await provider.normalize(_request())

    assert result.status is NormalizationStatus.SUCCESS
    assert result.text == "Send the report Friday."
    assert result.backend == "s1-mini"
    assert result.model == "superwhisper/s1-mini"
    assert result.version == "v1"
    assert len(seen) == 1


@pytest.mark.asyncio
async def test_s1_mini_uses_validated_control_values_and_configured_model():
    payload = {}

    def handler(request: httpx.Request) -> httpx.Response:
        payload.update(__import__("json").loads(request.content))
        return httpx.Response(200, json={"choices": [{"message": {"content": "- One\n- Two\n- Three"}}]})

    async with _client(handler) as client:
        provider = S1MiniTranscriptNormalizer(
            endpoint="http://localhost:9000/",
            model="local/s1-mini-q4",
            styling="formal",
            structure="lists",
            context="email",
            client=client,
        )
        result = await provider.normalize(_request("one two three"))

    assert payload["model"] == "local/s1-mini-q4"
    assert payload["messages"][1]["content"].startswith(
        "[Styling: formal] [Structure: lists] [Context: email]\n"
    )
    assert result.status is NormalizationStatus.SUCCESS


@pytest.mark.parametrize(
    ("field", "value"),
    [
        ("styling", "wild"),
        ("structure", "table"),
        ("context", "shell"),
    ],
)
def test_s1_mini_rejects_untrained_control_values(field: str, value: str):
    kwargs = {field: value}
    with pytest.raises(ValueError, match=field):
        S1MiniTranscriptNormalizer(endpoint="http://127.0.0.1:8000", **kwargs)


@pytest.mark.asyncio
async def test_s1_mini_rejects_unsupported_language_without_network():
    entered = False

    def handler(request: httpx.Request) -> httpx.Response:
        nonlocal entered
        entered = True
        return httpx.Response(500)

    async with _client(handler) as client:
        provider = S1MiniTranscriptNormalizer(endpoint="http://127.0.0.1:8000", client=client)
        result = await provider.normalize(
            TranscriptNormalizationRequest("bonjour", "fr", "turn-1")
        )

    assert result.status is NormalizationStatus.UNSUPPORTED_LANGUAGE
    assert entered is False


@pytest.mark.asyncio
@pytest.mark.parametrize(
    ("failure", "expected"),
    [
        ("connect", NormalizationStatus.UNAVAILABLE),
        ("missing", NormalizationStatus.UNAVAILABLE),
        ("timeout", NormalizationStatus.TIMEOUT),
        ("malformed", NormalizationStatus.INVALID_OUTPUT),
    ],
)
async def test_s1_mini_maps_endpoint_failures_to_typed_status(failure, expected):
    def handler(request: httpx.Request) -> httpx.Response:
        if failure == "connect":
            raise httpx.ConnectError("offline", request=request)
        if failure == "timeout":
            raise httpx.ReadTimeout("slow", request=request)
        if failure == "missing":
            return httpx.Response(404, json={"error": {"message": "model not found"}})
        return httpx.Response(200, content=b"not-json")

    async with _client(handler) as client:
        provider = S1MiniTranscriptNormalizer(endpoint="http://127.0.0.1:8000", client=client)
        result = await provider.normalize(_request())

    assert result.status is expected
    assert result.text == ""


@pytest.mark.asyncio
@pytest.mark.parametrize(
    "payload",
    [
        {"choices": []},
        {"choices": [{"message": {"content": ""}}]},
        {"choices": [{"message": {"content": 42}}]},
        {"choices": [{"message": {"content": "<think>hidden</think>Hello"}}]},
        {"choices": [{"message": {"content": "Normalized transcript: Hello."}}]},
        {"choices": [{"message": {"content": "Hello.", "reasoning_content": "secret"}}]},
    ],
)
async def test_s1_mini_rejects_empty_malformed_reasoning_and_wrappers(payload):
    def handler(request: httpx.Request) -> httpx.Response:
        return httpx.Response(200, json=payload)

    async with _client(handler) as client:
        provider = S1MiniTranscriptNormalizer(endpoint="http://127.0.0.1:8000", client=client)
        result = await provider.normalize(_request("hello"))

    assert result.status is NormalizationStatus.INVALID_OUTPUT
    assert result.text == ""


@pytest.mark.asyncio
async def test_s1_mini_rejects_absolute_and_ratio_expansion():
    outputs = iter(["12345", "this output expanded far beyond input"])

    def handler(request: httpx.Request) -> httpx.Response:
        return httpx.Response(200, json={"choices": [{"message": {"content": next(outputs)}}]})

    async with _client(handler) as client:
        provider = S1MiniTranscriptNormalizer(
            endpoint="http://127.0.0.1:8000",
            client=client,
            max_output_chars=4,
            max_expansion_ratio=2.0,
        )
        absolute = await provider.normalize(_request("1234"))
        ratio = await provider.normalize(_request("hi"))

    assert absolute.status is NormalizationStatus.INVALID_OUTPUT
    assert ratio.status is NormalizationStatus.INVALID_OUTPUT


@pytest.mark.asyncio
async def test_s1_mini_stale_turn_uses_canonical_service_fence_before_endpoint():
    entered = False

    def handler(request: httpx.Request) -> httpx.Response:
        nonlocal entered
        entered = True
        return httpx.Response(200, json={"choices": [{"message": {"content": "Hello."}}]})

    async with _client(handler) as client:
        provider = S1MiniTranscriptNormalizer(endpoint="http://127.0.0.1:8000", client=client)
        service = TranscriptNormalizationService(
            registry=__import__("zara.transcript_normalization", fromlist=["TranscriptNormalizationRegistry"]).TranscriptNormalizationRegistry([provider]),
            is_turn_current=lambda _turn_id: False,
        )
        result = await service.normalize(_request(), backend="s1-mini")

    assert result.status is NormalizationStatus.CANCELLED
    assert entered is False


@pytest.mark.asyncio
async def test_s1_mini_projects_representative_cleanup_fixture_outputs():
    fixtures = {
        "um uh send it": "Send it.",
        "i need no wait we need it": "We need it.",
        "the the report": "The report.",
        "call me at five thirty": "Call me at 5:30.",
        "it costs twenty three dollars": "It costs $23.",
        "march third twenty twenty six": "March 3, 2026.",
        "email support at example dot com": "Email support@example.com.",
        "already clean input.": "Already clean input.",
    }

    def handler(request: httpx.Request) -> httpx.Response:
        body = __import__("json").loads(request.content)
        raw = body["messages"][1]["content"].split("\n", 1)[1]
        return httpx.Response(200, json={"choices": [{"message": {"content": fixtures[raw]}}]})

    async with _client(handler) as client:
        provider = S1MiniTranscriptNormalizer(
            endpoint="http://127.0.0.1:8000",
            client=client,
            max_expansion_ratio=4.0,
        )
        for raw, clean in fixtures.items():
            result = await provider.normalize(_request(raw))
            assert result.status is NormalizationStatus.SUCCESS
            assert result.text == clean
