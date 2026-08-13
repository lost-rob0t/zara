from __future__ import annotations

import asyncio
import sys
from pathlib import Path

import pytest

from zara.runtime.prolog_rlm import PrologRLMBackend, PrologRLMError


REVISION = "4cdc9854a510a2d07b559e9ae34491d43d81301a"


class FakeConfig:
    def __init__(self, **overrides):
        self.sections = {
            "agent": {"backend": "prolog_rlm"},
            "prolog_rlm": {
                "request_timeout": 1.0,
                "cancel_grace": 0.05,
                **overrides,
            },
        }

    def get_section(self, section):
        return dict(self.sections.get(section, {}))

    def get(self, section, key, default=None):
        return self.sections.get(section, {}).get(key, default)


def make_sidecar(tmp_path: Path, run_body: str) -> Path:
    script = tmp_path / "edge_sidecar.py"
    script.write_text(
        f'''import json
import sys
import time

REVISION = {REVISION!r}


def send(payload):
    sys.stdout.write(json.dumps(payload, separators=(",", ":")) + "\\n")
    sys.stdout.flush()


mode = sys.argv[1]
if mode == "probe":
    send({{"type": "ready", "revision": REVISION}})
    raise SystemExit(0)

request = json.loads(sys.stdin.readline())
request_id = request["request_id"]
send({{"type": "started", "request_id": request_id}})
{run_body}
''',
        encoding="utf-8",
    )
    return script


def backend_for(script: Path, **overrides) -> PrologRLMBackend:
    return PrologRLMBackend(
        FakeConfig(**overrides),
        command_builder=lambda probe: (
            sys.executable,
            str(script),
            "probe" if probe else "run",
        ),
    )


@pytest.mark.asyncio
async def test_unexpected_sidecar_crash_is_structured(tmp_path):
    script = make_sidecar(tmp_path, "raise SystemExit(17)")
    backend = backend_for(script)
    await backend.start()
    try:
        with pytest.raises(PrologRLMError) as caught:
            await backend.submit_turn("crash", turn_id="turn-crash")
    finally:
        await backend.stop()

    assert caught.value.kind == "sidecar_crash"
    assert caught.value.details["exit_status"] == 17


@pytest.mark.asyncio
async def test_request_timeout_cleans_up_child(tmp_path):
    script = make_sidecar(tmp_path, "time.sleep(10)")
    backend = backend_for(script, request_timeout=0.08, cancel_grace=0.03)
    await backend.start()
    try:
        with pytest.raises(PrologRLMError) as caught:
            await backend.submit_turn("timeout", turn_id="turn-timeout")
    finally:
        await backend.stop()

    assert caught.value.kind == "timeout"
    assert backend._active == {}


@pytest.mark.asyncio
async def test_response_request_id_mismatch_is_rejected(tmp_path):
    script = make_sidecar(
        tmp_path,
        '''send({
    "type": "completed",
    "request_id": "wrong-request-id",
    "result": {"text": "must not escape"},
})''',
    )
    backend = backend_for(script)
    await backend.start()
    try:
        with pytest.raises(PrologRLMError) as caught:
            await backend.submit_turn("correlate", turn_id="turn-correlation")
    finally:
        await backend.stop()

    assert caught.value.kind == "request_correlation_error"


@pytest.mark.asyncio
async def test_unexposed_tool_is_denied_and_bounded_to_protocol(tmp_path):
    script = make_sidecar(
        tmp_path,
        '''send({
    "type": "tool_call",
    "request_id": request_id,
    "tool_call_id": "tool-denied",
    "tool": "shell",
    "args": {"command": "echo nope"},
})
reply = json.loads(sys.stdin.readline())
send({
    "type": "completed",
    "request_id": request_id,
    "result": {"text": reply["error"]["kind"]},
})''',
    )
    backend = backend_for(script)
    await backend.start()
    try:
        result = await backend.submit_turn("deny tool", turn_id="turn-denied")
    finally:
        await backend.stop()

    assert result.response == "capability_denied"
    assert result.tool_results[0]["success"] is False
    assert result.tool_results[0]["tool_name"] == "shell"


@pytest.mark.asyncio
async def test_calculator_malformed_arguments_are_rejected(tmp_path):
    script = make_sidecar(
        tmp_path,
        '''send({
    "type": "tool_call",
    "request_id": request_id,
    "tool_call_id": "tool-malformed",
    "tool": "calculator",
    "args": {"expression": ["2+2"]},
})
reply = json.loads(sys.stdin.readline())
send({
    "type": "completed",
    "request_id": request_id,
    "result": {"text": reply["error"]["kind"]},
})''',
    )
    backend = backend_for(script)
    await backend.start()
    try:
        result = await backend.submit_turn("bad args", turn_id="turn-malformed-tool")
    finally:
        await backend.stop()

    assert result.response == "invalid_arguments"
    assert result.tool_results[0]["success"] is False


@pytest.mark.asyncio
async def test_calculator_output_limit_is_enforced(tmp_path):
    script = make_sidecar(
        tmp_path,
        '''send({
    "type": "tool_call",
    "request_id": request_id,
    "tool_call_id": "tool-large",
    "tool": "calculator",
    "args": {"expression": "9**999"},
})
reply = json.loads(sys.stdin.readline())
send({
    "type": "completed",
    "request_id": request_id,
    "result": {"text": reply["error"]["message"]},
})''',
    )
    backend = backend_for(script)
    await backend.start()
    try:
        result = await backend.submit_turn("large result", turn_id="turn-large-tool")
    finally:
        await backend.stop()

    assert "output is too large" in result.response
    assert result.tool_results[0]["success"] is False


@pytest.mark.asyncio
async def test_two_concurrent_requests_keep_responses_correlated(tmp_path):
    script = make_sidecar(
        tmp_path,
        '''delay = 0.08 if request["query"] == "slow" else 0.01
time.sleep(delay)
send({
    "type": "completed",
    "request_id": request_id,
    "result": {"text": "reply:" + request["query"]},
})''',
    )
    backend = backend_for(script)
    await backend.start()
    try:
        slow, fast = await asyncio.gather(
            backend.submit_turn("slow", turn_id="turn-slow"),
            backend.submit_turn("fast", turn_id="turn-fast"),
        )
    finally:
        await backend.stop()

    assert slow.response == "reply:slow"
    assert fast.response == "reply:fast"
    assert backend._active == {}
