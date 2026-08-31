"""ZARA-027 streaming acceptance tests (issue #28).

Covers the acceptance matrix from rage/28-streaming-llm-design.org:

- A1: first text delta observable before the model stream completes.
- A3: tool-call chunks never become text_delta/sentence_ready; typed
  tool lifecycle events instead.
- A4: cancellation mid-stream closes the stream, emits exactly one
  ``cancelled`` terminal and no stale deltas.
- A5: malformed/failed streams emit exactly one typed ``failed`` terminal,
  never a fake completion, and close provider resources.
- A6: LLMs without ``astream`` use the honest buffered fallback (single
  ``completed``, no fabricated deltas).
- A7: the aggregated assistant message is the exact final model response
  and no tool/protocol fragments leak into text events.
- A8: ``zara.llm.LLMClient.stream_events_async`` against local fake SSE/NDJSON
  servers for OpenAI-compatible, OpenRouter, Anthropic, and Ollama providers.
- A9: latency traces expose genuine request/first-token/first-sentence/final
  boundaries without the buffered proxy fiction on the streaming path.
"""

from __future__ import annotations

import asyncio
import json
import random
import threading
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from types import SimpleNamespace

import pytest
from langchain_core.messages import AIMessage, AIMessageChunk, HumanMessage, ToolMessage
from langchain_core.tools import tool

from zara.agent import AgentManager
from zara.agent import stream_events
from zara.agent.conversation import ConversationManager
from zara.agent.graph import create_agent_node, create_tools_node, run_conversation_loop
from zara.agent.sentence_chunker import SentenceChunker
from zara.agent.stream_events import (
    Cancelled,
    Completed,
    Failed,
    LLMStreamEvent,
    SentenceReady,
    TextDelta,
    ToolCallStarted,
    ToolResult,
)
from zara.database import DatabaseManager
from zara.desktop.conversation import (
    ConversationService,
    ConversationStore,
    MessageRole,
    MessageStatus,
)
from zara.latency import LatencyTrace
from zara.llm import LLMClient
from zara.runtime import bridge as runtime_bridge
from zara.runtime import events


# ---------------------------------------------------------------------------
# Fakes


class RecordingPublisher:
    def __init__(self):
        self.events: list[LLMStreamEvent] = []
        self.lock = threading.Lock()

    def __call__(self, event: LLMStreamEvent) -> None:
        with self.lock:
            self.events.append(event)

    def of(self, event_type):
        return [event for event in self.events if isinstance(event, event_type)]

    def texts(self, event_type):
        return [event.text for event in self.of(event_type)]


class FakeRegistry:
    def to_langchain_tools(self):
        return []


@tool
def calc(a: int) -> int:
    """Calculate a value."""
    return a * 2


class FakeToolRegistry:
    def to_langchain_tools(self):
        return [calc]

    def requires_approval(self, name):
        return False

    def get_tool(self, name):
        return calc if name == calc.name else None


class FakeStreamingLLM:
    """Streams fixed AIMessageChunks; counts calls; records stream close."""

    def __init__(self, chunks):
        self.chunks = chunks
        self.calls = 0
        self.closed = False

    def bind_tools(self, tools):
        return self

    async def astream(self, messages):
        self.calls += 1
        try:
            for chunk in self.chunks:
                yield chunk
        finally:
            self.closed = True


def chunk(content="", tool_call_chunks=None):
    if tool_call_chunks is None:
        return AIMessageChunk(content=content)
    return AIMessageChunk(content=content, tool_call_chunks=tool_call_chunks)


# ---------------------------------------------------------------------------
# A1: genuine incremental text deltas


@pytest.mark.asyncio
async def test_first_text_delta_published_before_stream_completes():
    recorder = RecordingPublisher()

    class OrderingLLM(FakeStreamingLLM):
        async def astream(self, messages):
            self.calls += 1
            pieces = [chunk("Hel"), chunk("lo the"), chunk("re.")]
            for index, piece in enumerate(pieces):
                yield piece
                if index == 0:
                    for _ in range(500):
                        if recorder.of(TextDelta):
                            break
                        await asyncio.sleep(0.01)
                    assert recorder.of(TextDelta), (
                        "no text delta was published while the model stream was still open"
                    )

    llm = OrderingLLM([])
    node = create_agent_node(llm, FakeRegistry(), stream_publisher=recorder)
    result = await node(
        {
            "messages": [HumanMessage(content="hi")],
            "step_count": 0,
            "turn_id": "turn-a1",
            "conversation_id": "conv-a1",
        }
    )

    assert [event.text for event in recorder.of(TextDelta)] == ["Hel", "lo the", "re."]
    assert recorder.of(Completed) == [Completed(full_text="Hello there.")]
    assert isinstance(result["messages"][0], AIMessage)
    assert result["messages"][0].content == "Hello there."
    assert result["step_count"] == 1


@pytest.mark.asyncio
async def test_streaming_deltas_carry_turn_correlation_on_runtime_bus():
    recorder = RecordingPublisher()
    llm = FakeStreamingLLM([chunk("One. "), chunk("Two.")])
    node = create_agent_node(llm, FakeRegistry(), stream_publisher=recorder)
    subscription = runtime_bridge.subscribe()
    try:
        await node(
            {
                "messages": [HumanMessage(content="hi")],
                "step_count": 0,
                "turn_id": "turn-77",
                "conversation_id": "conv-7",
            }
        )
        emitted = [envelope.event for envelope in subscription.drain()]
    finally:
        subscription.close()

    deltas = [event for event in emitted if isinstance(event, events.AssistantDelta)]
    assert [event.text for event in deltas] == ["One. ", "Two."]
    assert all(event.turn_id == "turn-77" for event in deltas)
    assert all(event.conversation_id == "conv-7" for event in deltas)
    completed = [event for event in emitted if isinstance(event, events.AssistantComplete)]
    assert [event.text for event in completed] == ["One. Two."]


# ---------------------------------------------------------------------------
# A3: tool-call chunks never leak into text


@pytest.mark.asyncio
async def test_tool_call_chunks_never_become_text_and_emit_lifecycle_events():
    recorder = RecordingPublisher()

    class ToolStreamingLLM:
        def __init__(self):
            self.calls = 0
            self.closed = False

        def bind_tools(self, tools):
            return self

        async def astream(self, messages):
            self.calls += 1
            try:
                if isinstance(messages[-1], ToolMessage):
                    yield chunk("The answer ")
                    yield chunk("is 24.")
                    return
                yield chunk("Let me check. ")
                yield chunk(
                    tool_call_chunks=[
                        {"name": "calc", "args": '{"a": ', "id": "call-1", "index": 0}
                    ]
                )
                yield chunk(
                    tool_call_chunks=[
                        {"name": "", "args": "12}", "id": "", "index": 0}
                    ]
                )
            finally:
                self.closed = True

    llm = ToolStreamingLLM()
    state = {
        "messages": [HumanMessage(content="compute 12 please")],
        "step_count": 0,
        "max_steps": 4,
        "turn_id": "turn-a3",
        "conversation_id": "conv-a3",
    }
    result = await run_conversation_loop(
        llm,
        FakeToolRegistry(),
        state,
        stream_publisher=recorder,
    )

    delta_texts = recorder.texts(TextDelta)
    sentence_texts = recorder.texts(SentenceReady)
    for text in delta_texts + sentence_texts:
        assert '{"a"' not in text
        assert "12}" not in text
        assert "call-1" not in text

    assert recorder.of(ToolCallStarted) == [ToolCallStarted(name="calc", id="call-1")]
    assert recorder.of(ToolResult) == [ToolResult(name="calc", id="call-1")]

    completions = recorder.of(Completed)
    assert [event.full_text for event in completions] == [
        "Let me check. ",
        "The answer is 24.",
    ]

    assert result["response"] == "The answer is 24."
    final_messages = [
        message for message in result["messages"] if isinstance(message, AIMessage)
    ]
    assert final_messages[-1].content == "The answer is 24."
    assert all("{" not in message.content for message in final_messages)
    assert result["tool_results"][0]["result"] == "24"


# ---------------------------------------------------------------------------
# A4: cancellation mid-stream


@pytest.mark.asyncio
async def test_cancellation_mid_stream_emits_single_cancelled_and_closes_stream():
    recorder = RecordingPublisher()

    class HangingLLM(FakeStreamingLLM):
        def __init__(self):
            super().__init__([])
            self.first_yield_seen = asyncio.Event()

        async def astream(self, messages):
            self.calls += 1
            try:
                yield chunk("Partial ")
                self.first_yield_seen.set()
                await asyncio.Event().wait()
                yield chunk("never published")
            finally:
                self.closed = True

    llm = HangingLLM()
    node = create_agent_node(llm, FakeRegistry(), stream_publisher=recorder)
    subscription = runtime_bridge.subscribe()
    task = asyncio.ensure_future(
        node(
            {
                "messages": [HumanMessage(content="hi")],
                "step_count": 0,
                "turn_id": "turn-a4",
                "conversation_id": "conv-a4",
            }
        )
    )
    try:
        await asyncio.wait_for(llm.first_yield_seen.wait(), timeout=5)
        for _ in range(200):
            if recorder.of(TextDelta):
                break
            await asyncio.sleep(0.01)
        assert recorder.of(TextDelta), "first delta was not published before cancellation"
        task.cancel()
        with pytest.raises(asyncio.CancelledError):
            await task
    finally:
        subscription.close()

    assert [type(event) for event in recorder.events] == [TextDelta, Cancelled]
    assert llm.closed is True

    runtime_events = [envelope.event for envelope in subscription.drain()]
    assert not any(
        isinstance(event, events.AssistantDelta) and event.text == "never published"
        for event in runtime_events
    )
    assert not any(isinstance(event, events.AssistantComplete) for event in runtime_events)
    deltas = [event for event in runtime_events if isinstance(event, events.AssistantDelta)]
    assert [event.text for event in deltas] == ["Partial "]


# ---------------------------------------------------------------------------
# A5: error mid-stream


@pytest.mark.asyncio
async def test_stream_error_emits_single_failed_and_propagates():
    recorder = RecordingPublisher()
    subscription = runtime_bridge.subscribe()

    class ExplodingLLM(FakeStreamingLLM):
        async def astream(self, messages):
            self.calls += 1
            try:
                yield chunk("Partial answer")
                raise RuntimeError("provider exploded")
            finally:
                self.closed = True

    llm = ExplodingLLM([])
    node = create_agent_node(llm, FakeRegistry(), stream_publisher=recorder)
    try:
        with pytest.raises(RuntimeError, match="provider exploded"):
            await node(
                {
                    "messages": [HumanMessage(content="hi")],
                    "step_count": 0,
                    "turn_id": "turn-a5",
                    "conversation_id": "conv-a5",
                }
            )
    finally:
        subscription.close()

    assert [type(event) for event in recorder.events] == [TextDelta, Failed]
    assert recorder.of(Failed)[0].error_type == "RuntimeError"
    assert llm.closed is True
    runtime_events = [envelope.event for envelope in subscription.drain()]
    assert any(isinstance(event, events.AssistantFailed) for event in runtime_events)
    assert not any(isinstance(event, events.AssistantComplete) for event in runtime_events)


@pytest.mark.asyncio
async def test_stream_error_before_first_delta_still_fails_without_completion():
    recorder = RecordingPublisher()

    class ImmediateFailureLLM(FakeStreamingLLM):
        async def astream(self, messages):
            self.calls += 1
            try:
                raise ValueError("bad request")
                yield chunk("unreachable")
            finally:
                self.closed = True

    llm = ImmediateFailureLLM([])
    node = create_agent_node(llm, FakeRegistry(), stream_publisher=recorder)

    with pytest.raises(ValueError, match="bad request"):
        await node({"messages": [HumanMessage(content="hi")], "step_count": 0})

    assert [type(event) for event in recorder.events] == [Failed]
    assert recorder.of(Failed)[0].error_type == "ValueError"
    assert llm.closed is True


# ---------------------------------------------------------------------------
# A6: honest buffered fallback


@pytest.mark.asyncio
async def test_llm_without_astream_uses_buffered_fallback_without_fake_deltas():
    recorder = RecordingPublisher()

    class BufferedOnlyLLM:
        def bind_tools(self, tools):
            return self

        async def ainvoke(self, messages):
            return AIMessage(content="Full answer.")

    node = create_agent_node(BufferedOnlyLLM(), FakeRegistry(), stream_publisher=recorder)
    result = await node(
        {
            "messages": [HumanMessage(content="hi")],
            "step_count": 0,
            "turn_id": "turn-a6",
        }
    )

    assert recorder.events == [Completed(full_text="Full answer.")]
    assert result["messages"][0].content == "Full answer."


@pytest.mark.asyncio
async def test_empty_stream_completes_with_empty_text():
    recorder = RecordingPublisher()
    llm = FakeStreamingLLM([])
    node = create_agent_node(llm, FakeRegistry(), stream_publisher=recorder)

    result = await node({"messages": [HumanMessage(content="hi")], "step_count": 0})

    assert recorder.events == [Completed(full_text="")]
    assert result["messages"][0].content == ""


# ---------------------------------------------------------------------------
# A7: final history is the exact model response


@pytest.mark.asyncio
async def test_final_message_equals_concatenation_of_provider_text_chunks():
    recorder = RecordingPublisher()
    rng = random.Random(2727)
    pieces = ["Alpha ", "beta ", "gamma. ", "Delta!", "epsilon? ", "… ", "，，", "第"]
    chunks = [chunk(rng.choice(pieces)) for _ in range(60)]
    expected = "".join(piece.content for piece in chunks)

    llm = FakeStreamingLLM(chunks)
    node = create_agent_node(llm, FakeRegistry(), stream_publisher=recorder)
    result = await node({"messages": [HumanMessage(content="hi")], "step_count": 0})

    assert result["messages"][0].content == expected
    assert "".join(recorder.texts(TextDelta)) == expected
    assert "".join(recorder.texts(SentenceReady)) != ""
    assert "".join(recorder.texts(TextDelta)) == "".join(recorder.texts(SentenceReady))


@pytest.mark.asyncio
async def test_mixed_text_and_tool_chunks_aggregate_exactly():
    recorder = RecordingPublisher()
    chunks = [
        chunk("Intro. "),
        chunk(tool_call_chunks=[{"name": "calc", "args": '{"a": ', "id": "c1", "index": 0}]),
        chunk(tool_call_chunks=[{"name": "", "args": "2}", "id": "", "index": 0}]),
        chunk(" Outro."),
    ]
    llm = FakeStreamingLLM(chunks)
    node = create_agent_node(llm, FakeRegistry(), stream_publisher=recorder)
    result = await node({"messages": [HumanMessage(content="hi")], "step_count": 0})

    message = result["messages"][0]
    assert message.content == "Intro.  Outro."
    assert message.tool_calls[0]["name"] == "calc"
    assert message.tool_calls[0]["args"] == {"a": 2}

    deltas = "".join(recorder.texts(TextDelta))
    assert deltas == "Intro.  Outro."
    assert '"a"' not in deltas and "c1" not in deltas


# ---------------------------------------------------------------------------
# A9: latency trace boundaries


@pytest.mark.asyncio
async def test_streaming_node_records_genuine_latency_boundaries():
    recorder = RecordingPublisher()

    class FakeClock:
        def __init__(self):
            self.value = 0

        def __call__(self):
            return self.value

        def advance_ms(self, milliseconds):
            self.value += int(milliseconds * 1_000_000)

    clock = FakeClock()
    trace = LatencyTrace(trace_id="trace-stream", clock_ns=clock)

    class TimedLLM(FakeStreamingLLM):
        async def astream(self, messages):
            pieces = [chunk("Hel"), chunk("lo there. "), chunk("How"), chunk(" are you?")]
            for index, piece in enumerate(pieces):
                if index == 0:
                    clock.advance_ms(120)
                if index == 2:
                    clock.advance_ms(80)
                yield piece
                await asyncio.sleep(0)

    node = create_agent_node(TimedLLM([]), FakeRegistry(), stream_publisher=recorder)
    await node(
        {
            "messages": [HumanMessage(content="hi")],
            "step_count": 0,
            "latency_trace": trace,
        }
    )

    assert [event.event for event in trace.events] == [
        "llm_request",
        "llm_first_token",
        "llm_first_sentence",
        "llm_final_token",
    ]
    first_token = trace.events[1]
    assert first_token.metadata["buffered_proxy"] is False
    assert trace.duration_ms("llm_request", "llm_first_token") == 120.0
    assert trace.duration_ms("llm_first_token", "llm_final_token") == 80.0


# ---------------------------------------------------------------------------
# A8: LLMClient.stream_events_async against local fake servers


class FakeProviderServer:
    """Local HTTP server emitting a canned streaming response."""

    def __init__(self, *, status=200, content_type="text/event-stream", frames=()):
        self.status = status
        self.content_type = content_type
        self.frames = list(frames)
        self.requests = []
        self._server = None
        self._thread = None
        self._release = None

    def start(self):
        server = self

        class Handler(BaseHTTPRequestHandler):
            def do_POST(self):
                length = int(self.headers.get("Content-Length", 0))
                body = json.loads(self.rfile.read(length) or b"{}")
                server.requests.append(
                    {"path": self.path, "headers": dict(self.headers), "body": body}
                )
                self.send_response(server.status)
                self.send_header("Content-Type", server.content_type)
                self.end_headers()
                for frame in server.frames:
                    self.wfile.write(frame)
                    self.wfile.flush()
                if server._release is not None:
                    server._release.wait()

            def log_message(self, *args):
                pass

        self._server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
        self._thread = threading.Thread(target=self._server.serve_forever, daemon=True)
        self._thread.start()
        return f"http://127.0.0.1:{self._server.server_address[1]}/v1/stream"

    def hang_after_frames(self):
        self._release = threading.Event()

    def stop(self):
        if self._release is not None:
            self._release.set()
        if self._server is not None:
            self._server.shutdown()
            self._server.server_close()
        if self._thread is not None:
            self._thread.join(timeout=5)


def sse(payload):
    return f"data: {json.dumps(payload)}\n\n".encode()


def openai_frames(*texts):
    frames = [sse({"choices": [{"delta": {"content": text}}]}) for text in texts]
    frames.append(b"data: [DONE]\n\n")
    return frames


def anthropic_frames(*texts):
    frames = [
        (
            b'event: content_block_delta\ndata: '
            + json.dumps(
                {"type": "content_block_delta", "delta": {"type": "text_delta", "text": text}}
            ).encode()
            + b"\n\n"
        )
        for text in texts
    ]
    frames.append(
        b'event: message_stop\ndata: {"type": "message_stop"}\n\n'
    )
    return frames


def ollama_frames(*texts):
    frames = [
        json.dumps({"message": {"role": "assistant", "content": text}}).encode() + b"\n"
        for text in texts
    ]
    frames.append(json.dumps({"done": True}).encode() + b"\n")
    return frames


def drain_events(client, prompt="Hi"):
    recorder = RecordingPublisher()

    async def run():
        try:
            async for event in client.stream_events_async(prompt):
                recorder.events.append(event)
        finally:
            await client.close()

    asyncio.run(run())
    return recorder.events


@pytest.mark.parametrize(
    "provider,frames_factory",
    [
        ("openai", openai_frames),
        ("openrouter", openai_frames),
        ("anthropic", anthropic_frames),
        ("ollama", ollama_frames),
    ],
)
def test_llm_client_streams_all_provider_families(provider, frames_factory):
    server = FakeProviderServer(frames=frames_factory("Hel", "lo there. ", "Bye."))
    endpoint = server.start()
    try:
        client = LLMClient(
            provider=provider,
            endpoint=endpoint,
            api_key="test-key",
            connect_timeout=2,
            read_timeout=5,
            total_timeout=10,
        )
        events = drain_events(client)
    finally:
        server.stop()

    assert [event.text for event in events if isinstance(event, TextDelta)] == [
        "Hel",
        "lo there. ",
        "Bye.",
    ]
    deltas = [event.text for event in events if isinstance(event, SentenceReady)]
    assert "Hello there. " in deltas
    assert events[-1] == Completed(full_text="Hello there. Bye.")

    request = server.requests[0]
    assert request["body"]["stream"] is True
    if provider == "openrouter":
        assert request["headers"]["Authorization"] == "Bearer test-key"
    if provider == "anthropic":
        assert request["headers"]["x-api-key"] == "test-key"


def test_llm_client_stream_reports_http_failure_without_completion():
    server = FakeProviderServer(status=500, content_type="text/plain", frames=[b"boom"])
    endpoint = server.start()
    try:
        client = LLMClient(provider="openai", endpoint=endpoint, api_key="k")
        events = drain_events(client)
    finally:
        server.stop()

    assert len(events) == 1
    assert isinstance(events[0], Failed)
    assert events[0].error_type == "http"


def test_llm_client_stream_reports_malformed_sse():
    server = FakeProviderServer(
        frames=[sse({"choices": [{"delta": {"content": "Hi"}}]}), b"data: not-json\n\n"]
    )
    endpoint = server.start()
    try:
        client = LLMClient(provider="openai", endpoint=endpoint, api_key="k")
        events = drain_events(client)
    finally:
        server.stop()

    assert isinstance(events[-1], Failed)
    assert events[-1].error_type == "malformed_response"
    assert not any(isinstance(event, Completed) for event in events)


def test_llm_client_stream_reports_empty_stream_as_failure():
    server = FakeProviderServer(frames=[b"data: [DONE]\n\n"])
    endpoint = server.start()
    try:
        client = LLMClient(provider="openai", endpoint=endpoint, api_key="k")
        events = drain_events(client)
    finally:
        server.stop()

    assert [type(event) for event in events] == [Failed]
    assert events[0].error_type == "empty_response"


def test_llm_client_stream_reports_connection_failure():
    client = LLMClient(
        provider="openai",
        endpoint="http://127.0.0.1:1/v1/stream",
        api_key="k",
        connect_timeout=1,
    )
    events = drain_events(client)

    assert [type(event) for event in events] == [Failed]
    assert events[0].error_type == "connection"


def test_llm_client_stream_cancellation_emits_cancelled_and_stops():
    server = FakeProviderServer(frames=[sse({"choices": [{"delta": {"content": "First"}}]})])
    server.hang_after_frames()
    endpoint = server.start()
    recorder = RecordingPublisher()
    try:
        client = LLMClient(
            provider="openai",
            endpoint=endpoint,
            api_key="k",
            connect_timeout=2,
            read_timeout=5,
        )

        async def scenario():
            async def consume():
                async for event in client.stream_events_async("Hi"):
                    recorder.events.append(event)

            task = asyncio.ensure_future(consume())
            for _ in range(200):
                if recorder.of(TextDelta):
                    break
                await asyncio.sleep(0.01)
            assert recorder.of(TextDelta), "no delta arrived from fake server"
            await asyncio.sleep(0.05)
            task.cancel()
            with pytest.raises(asyncio.CancelledError):
                await task
            await client.close()

        asyncio.run(scenario())
    finally:
        server.stop()

    assert recorder.events[-1] == Cancelled()
    assert not any(isinstance(event, Completed) for event in recorder.events)


def test_llm_client_query_async_remains_buffered_and_intact():
    server = FakeProviderServer(
        content_type="application/json",
        frames=[json.dumps({"choices": [{"message": {"content": "Buffered answer"}}]}).encode()],
    )
    endpoint = server.start()
    try:
        client = LLMClient(provider="openai", endpoint=endpoint, api_key="k")
        result = asyncio.run(client.query_async("Hi"))
    finally:
        server.stop()

    assert result.success is True
    assert result.text == "Buffered answer"
    assert server.requests[0]["body"].get("stream") is None


def test_llm_client_stream_reports_anthropic_provider_error():
    frames = [
        b'event: content_block_delta\ndata: '
        + json.dumps(
            {"type": "content_block_delta", "delta": {"type": "text_delta", "text": "Hi"}}
        ).encode()
        + b"\n\n",
        b'event: error\ndata: {"type": "error", "error": {"type": "overloaded"}}\n\n',
    ]
    server = FakeProviderServer(frames=frames)
    endpoint = server.start()
    try:
        client = LLMClient(provider="anthropic", endpoint=endpoint, api_key="k")
        events = drain_events(client)
    finally:
        server.stop()

    assert [type(event) for event in events] == [TextDelta, Failed]
    assert events[-1].error_type == "provider_error"


def test_llm_client_stream_read_timeout_fails_without_completion():
    server = FakeProviderServer(frames=[sse({"choices": [{"delta": {"content": "First"}}]})])
    server.hang_after_frames()
    endpoint = server.start()
    try:
        client = LLMClient(
            provider="openai",
            endpoint=endpoint,
            api_key="k",
            connect_timeout=2,
            read_timeout=0.2,
            total_timeout=5,
        )
        events = drain_events(client)
    finally:
        server.stop()

    assert [type(event) for event in events] == [TextDelta, Failed]
    assert events[-1].error_type == "timeout"


# ---------------------------------------------------------------------------
# Provider payload hygiene


@pytest.mark.asyncio
async def test_content_block_payloads_surface_only_text_blocks():
    recorder = RecordingPublisher()
    blocks = [
        {"type": "thinking", "thinking": "secret chain of thought"},
        {"type": "text", "text": "Visible "},
        {"type": "tool_use", "id": "t1", "name": "calc", "input": {"a": 1}},
        {"type": "text", "text": "text."},
    ]

    class BlockStreamingLLM(FakeStreamingLLM):
        async def astream(self, messages):
            self.calls += 1
            yield AIMessageChunk(content=blocks)

    node = create_agent_node(BlockStreamingLLM([]), FakeRegistry(), stream_publisher=recorder)
    result = await node({"messages": [HumanMessage(content="hi")], "step_count": 0})

    assert recorder.texts(TextDelta) == ["Visible text."]
    assert all("secret" not in text for text in recorder.texts(TextDelta))
    assert result["messages"][0].content == blocks


# ---------------------------------------------------------------------------
# Desktop surface integration


@pytest.mark.asyncio
async def test_streaming_runtime_events_populate_desktop_conversation(tmp_path):
    recorder = RecordingPublisher()
    service = ConversationService(
        ConversationStore(DatabaseManager(tmp_path / "conversation.db"))
    )
    state = service.create_conversation()
    llm = FakeStreamingLLM([chunk("Hello there. "), chunk("How are you?")])
    node = create_agent_node(llm, FakeRegistry(), stream_publisher=recorder)

    subscription = runtime_bridge.subscribe()
    try:
        await node(
            {
                "messages": [HumanMessage(content="hi")],
                "step_count": 0,
                "turn_id": "turn-e2e",
                "conversation_id": state.conversation.id,
            }
        )
        drained = [envelope.event for envelope in subscription.drain()]
    finally:
        subscription.close()

    for event in drained:
        service.apply_event(event)

    assistant = state.latest_message(role=MessageRole.ASSISTANT, turn_id="turn-e2e")
    assert assistant is not None
    assert assistant.content == "Hello there. How are you?"
    assert assistant.status is MessageStatus.COMPLETE
    deltas = [event for event in drained if isinstance(event, events.AssistantDelta)]
    assert len(deltas) == 2


# ---------------------------------------------------------------------------
# Agent max-wait flush and tool result lifecycle


@pytest.mark.asyncio
async def test_node_time_flush_emits_sentence_during_slow_stream():
    recorder = RecordingPublisher()

    class SlowGapLLM(FakeStreamingLLM):
        async def astream(self, messages):
            self.calls += 1
            yield chunk("A slow first sentence without end")
            await asyncio.sleep(0.7)
            yield chunk(" then more.")

    node = create_agent_node(SlowGapLLM([]), FakeRegistry(), stream_publisher=recorder)
    await node({"messages": [HumanMessage(content="hi")], "step_count": 0})

    sentences = recorder.of(SentenceReady)
    assert sentences, "no sentence events were emitted"
    assert any(
        event.text == "A slow first sentence without end" and event.is_final is False
        for event in sentences
    ), sentences


@pytest.mark.asyncio
async def test_tools_node_publishes_tool_result_for_rejected_tool():
    recorder = RecordingPublisher()

    class ApprovingRegistry(FakeToolRegistry):
        def requires_approval(self, name):
            return True

    node = create_tools_node(
        ApprovingRegistry(), publisher=lambda event: None, stream_publisher=recorder
    )
    await node(
        {
            "messages": [
                AIMessage(
                    content="",
                    tool_calls=[
                        {"name": "calc", "args": {"a": 1}, "id": "call-1", "type": "tool_call"}
                    ],
                )
            ],
            "tool_decisions": {"call-1": {"decision": "reject"}},
            "turn_id": "turn-t",
            "conversation_id": "conv-t",
        },
        {},
    )

    assert recorder.of(ToolResult) == [ToolResult(name="calc", id="call-1")]


@pytest.mark.asyncio
async def test_tools_node_publishes_tool_result_when_tool_unavailable():
    recorder = RecordingPublisher()

    class UnavailableRegistry(FakeToolRegistry):
        def get_tool(self, name):
            return object()

    node = create_tools_node(
        UnavailableRegistry(), publisher=lambda event: None, stream_publisher=recorder
    )
    await node(
        {
            "messages": [
                AIMessage(
                    content="",
                    tool_calls=[
                        {"name": "calc", "args": {"a": 1}, "id": "call-1", "type": "tool_call"}
                    ],
                )
            ],
            "tool_decisions": {},
            "turn_id": "turn-t",
            "conversation_id": "conv-t",
        },
        {},
    )

    assert recorder.of(ToolResult) == [ToolResult(name="calc", id="call-1")]


# ---------------------------------------------------------------------------
# AgentManager wiring


@pytest.mark.asyncio
async def test_agent_manager_forwards_stream_publisher_to_loop(monkeypatch):
    from zara import agent as agent_module

    class FakeAgentConfig:
        def get_section(self, name):
            return {"max_steps": 4}

        def get_agent_system_prompt(self):
            return "system"

    manager = AgentManager.__new__(AgentManager)
    manager.config = FakeAgentConfig()
    manager.llm_client = object()
    manager.tool_registry = object()
    manager.memory_manager = None
    manager.memory_context_limit = 1200
    manager.memory_top_k = 5
    manager.conversation_manager = ConversationManager()
    manager.principal = None
    manager.approval_controller = SimpleNamespace(publisher=None)

    seen = {}

    async def fake_loop(llm_client, tool_registry, state, **kwargs):
        seen["stream_publisher"] = kwargs.get("stream_publisher")
        return {
            "messages": [*state["messages"], AIMessage(content="ok")],
            "response": "ok",
            "tool_results": [],
        }

    monkeypatch.setattr(agent_module, "run_conversation_loop", fake_loop)
    marker = object()
    await manager.process_async("hi", stream_publisher=marker)

    assert seen["stream_publisher"] is marker
