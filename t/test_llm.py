import asyncio
from contextlib import asynccontextmanager

import pytest
from aiohttp import web

from zara.llm import ChatHistory, LLMClient


@asynccontextmanager
async def fake_server(handler):
    application = web.Application()
    application.router.add_post("/llm", handler)
    runner = web.AppRunner(application)
    await runner.setup()
    site = web.TCPSite(runner, "127.0.0.1", 0)
    await site.start()
    port = site._server.sockets[0].getsockname()[1]
    try:
        yield f"http://127.0.0.1:{port}/llm"
    finally:
        await runner.cleanup()


def make_client(provider, endpoint="http://provider.test/llm", **kwargs):
    api_key = "literal-key" if provider in {"anthropic", "openai"} else None
    return LLMClient(
        provider=provider,
        model="test-model",
        endpoint=endpoint,
        api_key=api_key,
        retry_delay=0,
        **kwargs,
    )


@pytest.mark.parametrize("provider", ["anthropic", "openai", "ollama"])
def test_provider_golden_requests(provider):
    client = make_client(provider)
    history = [{"role": "assistant", "content": "earlier"}]

    headers, payload = client.serialize_request(
        "now", system_prompt="system", chat_history=history, max_tokens=77
    )

    assert payload["model"] == "test-model"
    if provider == "anthropic":
        assert headers == {
            "Content-Type": "application/json",
            "x-api-key": "literal-key",
            "anthropic-version": "2023-06-01",
        }
        assert payload == {
            "model": "test-model",
            "max_tokens": 77,
            "system": "system",
            "messages": [
                {"role": "assistant", "content": "earlier"},
                {"role": "user", "content": "now"},
            ],
        }
    elif provider == "openai":
        assert headers["Authorization"] == "Bearer literal-key"
        assert payload["messages"][0] == {"role": "system", "content": "system"}
        assert payload["max_tokens"] == 77
    else:
        assert "Authorization" not in headers
        assert payload["stream"] is False
        assert payload["options"] == {"num_predict": 77}


def test_timeout_dimensions_are_configured_independently():
    client = make_client(
        "ollama", connect_timeout=1.0, read_timeout=2.0, total_timeout=3.0
    )

    assert client.timeout.connect == 1.0
    assert client.timeout.sock_read == 2.0
    assert client.timeout.total == 3.0


@pytest.mark.asyncio
@pytest.mark.parametrize(
    ("provider", "response", "expected"),
    [
        ("anthropic", {"content": [{"type": "text", "text": "anthropic"}]}, "anthropic"),
        ("openai", {"choices": [{"message": {"content": "openai"}}]}, "openai"),
        ("ollama", {"message": {"content": "ollama"}}, "ollama"),
    ],
)
async def test_provider_success_is_typed_and_headers_are_literal(provider, response, expected):
    captured = {}

    async def handler(request):
        captured["headers"] = dict(request.headers)
        captured["payload"] = await request.json()
        return web.json_response(response)

    async with fake_server(handler) as endpoint:
        client = make_client(provider, endpoint=endpoint)
        result = await client.query_async("hello", system_prompt="system")
        await client.close()

    assert result.success and result.text == expected and not result.error
    if provider == "openai":
        assert captured["headers"]["Authorization"] == "Bearer literal-key"
    if provider == "anthropic":
        assert captured["headers"]["x-api-key"] == "literal-key"
        assert captured["payload"]["system"] == "system"
        assert all(
            message["role"] != "system" for message in captured["payload"]["messages"]
        )


@pytest.mark.asyncio
async def test_session_is_reused_and_closed():
    async def handler(request):
        return web.json_response({"message": {"content": "ok"}})

    async with fake_server(handler) as endpoint:
        client = make_client("ollama", endpoint=endpoint)
        first = await client.query_async("one")
        session = client._session
        second = await client.query_async("two")
        assert client._session is session
        await client.close()

    assert first.success and second.success
    assert session.closed and client._session is None


@pytest.mark.asyncio
async def test_rate_limit_retries_are_bounded():
    requests = 0

    async def handler(request):
        nonlocal requests
        requests += 1
        if requests < 3:
            return web.json_response({"error": "slow down"}, status=429)
        return web.json_response({"choices": [{"message": {"content": "ready"}}]})

    async with fake_server(handler) as endpoint:
        client = make_client("openai", endpoint=endpoint, max_retries=2)
        result = await client.query_async("hello")
        await client.close()

    assert result.success and result.text == "ready"
    assert result.attempts == 3 and requests == 3


@pytest.mark.asyncio
async def test_rate_limit_exhaustion_remains_typed():
    requests = 0

    async def handler(request):
        nonlocal requests
        requests += 1
        return web.Response(text="slow down", status=429)

    async with fake_server(handler) as endpoint:
        client = make_client("openai", endpoint=endpoint, max_retries=1)
        result = await client.query_async("hello")
        await client.close()

    assert not result.success and not result.text
    assert result.error_type == "rate_limit" and result.status == 429
    assert result.attempts == 2 and requests == 2


@pytest.mark.asyncio
async def test_timeout_is_typed_and_bounded():
    async def handler(request):
        await asyncio.sleep(1)
        return web.json_response({"message": {"content": "late"}})

    async with fake_server(handler) as endpoint:
        client = make_client(
            "ollama",
            endpoint=endpoint,
            connect_timeout=0.1,
            read_timeout=0.1,
            total_timeout=0.03,
            max_retries=0,
        )
        result = await client.query_async("hello")
        await client.close()

    assert not result.success and result.error_type == "timeout"


@pytest.mark.asyncio
async def test_cancellation_returns_typed_result_and_can_close():
    entered = asyncio.Event()
    release = asyncio.Event()

    async def handler(request):
        entered.set()
        await release.wait()
        return web.json_response({"message": {"content": "late"}})

    async with fake_server(handler) as endpoint:
        client = make_client("ollama", endpoint=endpoint)
        task = asyncio.create_task(client.query_async("hello"))
        await entered.wait()
        task.cancel()
        result = await task
        session = client._session
        await client.close()
        release.set()

    assert not result.success and result.cancelled
    assert result.error_type == "cancelled" and session.closed


@pytest.mark.asyncio
@pytest.mark.parametrize(
    ("response", "error_type"),
    [
        ({"unexpected": True}, "malformed_response"),
        ({"message": {"content": ""}}, "empty_response"),
    ],
)
async def test_invalid_responses_are_not_successful_text(response, error_type):
    async def handler(request):
        return web.json_response(response)

    async with fake_server(handler) as endpoint:
        client = make_client("ollama", endpoint=endpoint)
        result = await client.query_async("hello")
        await client.close()

    assert not result.success and not result.text
    assert result.error_type == error_type


@pytest.mark.asyncio
async def test_malformed_anthropic_block_is_typed():
    async def handler(request):
        return web.json_response({"content": ["not-a-block"]})

    async with fake_server(handler) as endpoint:
        client = make_client("anthropic", endpoint=endpoint)
        result = await client.query_async("hello")
        await client.close()

    assert not result.success and not result.text
    assert result.error_type == "malformed_response"


@pytest.mark.asyncio
async def test_non_json_response_is_malformed():
    async def handler(request):
        return web.Response(text="not-json", content_type="application/json")

    async with fake_server(handler) as endpoint:
        client = make_client("ollama", endpoint=endpoint)
        result = await client.query_async("hello")
        await client.close()

    assert not result.success and result.error_type == "malformed_response"


def test_history_is_bounded_in_storage_and_serialization():
    history = ChatHistory(max_length=3)
    for number in range(8):
        history.add_user_message(str(number))
    assert [message["content"] for message in history.get_messages()] == ["5", "6", "7"]

    client = make_client("ollama", history_limit=3)
    _, payload = client.serialize_request("now", chat_history=history.get_messages())
    non_system = payload["messages"][1:]
    assert [message["content"] for message in non_system] == ["6", "7", "now"]
