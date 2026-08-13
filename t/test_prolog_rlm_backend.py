from __future__ import annotations

import asyncio
import json
import os
import subprocess
import sys
from pathlib import Path

import pytest

from zara.runtime.backend import (
    LangGraphRuntimeBackend,
    create_runtime_backend,
)
from zara.runtime.prolog_rlm import (
    PROLOG_RLM_REVISION,
    PrologRLMBackend,
    PrologRLMError,
)


FAKE_SIDECAR = r"""
import json
import sys

REVISION = "4cdc9854a510a2d07b559e9ae34491d43d81301a"

def send(payload):
    sys.stdout.write(json.dumps(payload, separators=(",", ":")) + "\n")
    sys.stdout.flush()

mode = sys.argv[1]
if mode == "probe":
    send({"type": "ready", "revision": REVISION})
    raise SystemExit(0)

request = json.loads(sys.stdin.readline())
request_id = request["request_id"]
query = request["query"]
send({"type": "started", "request_id": request_id})

if query == "malformed":
    sys.stdout.write("this is not json\n")
    sys.stdout.flush()
    raise SystemExit(0)

if query == "fail":
    send({
        "type": "failed",
        "request_id": request_id,
        "error": {
            "kind": "provider_http_failure",
            "phase": "provider",
            "message": "provider failed",
        },
    })
    raise SystemExit(0)

if query == "hang":
    while True:
        message = json.loads(sys.stdin.readline())
        if message.get("type") == "cancel":
            send({
                "type": "cancelled",
                "request_id": request_id,
                "error": {
                    "kind": "cancelled",
                    "phase": "runtime",
                    "message": "completion cancelled",
                },
            })
            raise SystemExit(0)

tool_value = None
if query == "tool":
    send({
        "type": "tool_call",
        "request_id": request_id,
        "tool_call_id": "tool-1",
        "tool": "calculator",
        "args": {"expression": "2+2"},
    })
    tool_reply = json.loads(sys.stdin.readline())
    if tool_reply["status"] == "ok":
        tool_value = tool_reply["value"]
    else:
        tool_value = tool_reply["error"]["message"]

send({
    "type": "completed",
    "request_id": request_id,
    "result": {
        "text": tool_value or "echo:" + query,
        "usage": {"model_calls": 1, "tokens_known": False},
        "recursion": {"recursive_calls": 0, "max_depth": 0},
        "model_events": [],
        "transition_count": 0,
    },
})
"""


class FakeConfig:
    def __init__(self, *, backend="langgraph", prolog_rlm=None):
        self.sections = {
            "agent": {"backend": backend},
            "prolog_rlm": prolog_rlm or {},
        }

    def get_section(self, section):
        return dict(self.sections.get(section, {}))

    def get(self, section, key, default=None):
        return self.sections.get(section, {}).get(key, default)


@pytest.fixture
def fake_sidecar(tmp_path: Path) -> Path:
    path = tmp_path / "fake_sidecar.py"
    path.write_text(FAKE_SIDECAR, encoding="utf-8")
    return path


def build_backend(fake_sidecar: Path, **settings) -> PrologRLMBackend:
    config = FakeConfig(
        backend="prolog_rlm",
        prolog_rlm={
            "request_timeout": 2.0,
            "cancel_grace": 0.2,
            **settings,
        },
    )
    return PrologRLMBackend(
        config,
        command_builder=lambda probe: (
            sys.executable,
            str(fake_sidecar),
            "probe" if probe else "run",
        ),
    )


def test_backend_selection_defaults_to_langgraph():
    backend = create_runtime_backend(FakeConfig())
    assert isinstance(backend, LangGraphRuntimeBackend)


def test_backend_selection_allows_prolog_rlm():
    backend = create_runtime_backend(FakeConfig(backend="prolog_rlm"))
    assert isinstance(backend, PrologRLMBackend)


def test_backend_selection_rejects_unknown_backend():
    with pytest.raises(ValueError, match="Unsupported agent backend"):
        create_runtime_backend(FakeConfig(backend="mystery"))


def test_prolog_rlm_revision_is_exact_integration_contract():
    assert PROLOG_RLM_REVISION == "4cdc9854a510a2d07b559e9ae34491d43d81301a"


@pytest.mark.asyncio
async def test_sidecar_start_and_normal_completion(fake_sidecar):
    backend = build_backend(fake_sidecar)
    await backend.start()
    try:
        result = await backend.submit_turn(
            "hello",
            turn_id="turn-1",
            conversation_id="conversation-1",
        )
    finally:
        await backend.stop()

    assert result.response == "echo:hello"
    assert result.metadata["usage"]["model_calls"] == 1
    assert result.metadata["recursion"]["max_depth"] == 0


@pytest.mark.asyncio
async def test_structured_failure_preserves_error_kind(fake_sidecar):
    backend = build_backend(fake_sidecar)
    await backend.start()
    try:
        with pytest.raises(PrologRLMError) as caught:
            await backend.submit_turn("fail", turn_id="turn-fail")
    finally:
        await backend.stop()

    assert caught.value.kind == "provider_http_failure"
    assert caught.value.details["phase"] == "provider"


@pytest.mark.asyncio
async def test_malformed_sidecar_output_fails_structurally(fake_sidecar):
    backend = build_backend(fake_sidecar)
    await backend.start()
    try:
        with pytest.raises(PrologRLMError) as caught:
            await backend.submit_turn("malformed", turn_id="turn-malformed")
    finally:
        await backend.stop()

    assert caught.value.kind == "sidecar_protocol_error"


@pytest.mark.asyncio
async def test_cancellation_stops_only_target_request(fake_sidecar):
    backend = build_backend(fake_sidecar)
    await backend.start()
    slow = asyncio.create_task(
        backend.submit_turn("hang", turn_id="turn-slow")
    )
    fast = asyncio.create_task(
        backend.submit_turn("fast", turn_id="turn-fast")
    )

    await asyncio.sleep(0.05)
    await backend.cancel_turn("turn-slow")

    fast_result = await fast
    with pytest.raises(asyncio.CancelledError):
        await slow
    await backend.stop()

    assert fast_result.response == "echo:fast"
    assert backend._active == {}


@pytest.mark.asyncio
async def test_calculator_bridge_is_capability_bounded(fake_sidecar):
    backend = build_backend(fake_sidecar)
    published = []
    backend.bind_event_publisher(published.append)
    await backend.start()
    try:
        result = await backend.submit_turn("tool", turn_id="turn-tool")
    finally:
        await backend.stop()

    assert result.response == "Result: 4"
    assert result.tool_results == (
        {
            "tool_run_id": "tool-1",
            "tool_name": "calculator",
            "success": True,
            "result": "Result: 4",
        },
    )
    assert [type(event).__name__ for event in published] == [
        "ToolStarted",
        "ToolCompleted",
    ]


def test_calculator_rejects_unbounded_exponent(fake_sidecar):
    backend = build_backend(fake_sidecar)
    with pytest.raises(ValueError, match="exponent"):
        backend._invoke_calculator({"expression": "2**1001"})


@pytest.mark.asyncio
async def test_sidecar_probe_failure_is_startup_failure(tmp_path):
    script = tmp_path / "exit.py"
    script.write_text("raise SystemExit(3)\n", encoding="utf-8")
    backend = PrologRLMBackend(
        FakeConfig(backend="prolog_rlm"),
        command_builder=lambda _probe: (sys.executable, str(script)),
    )

    with pytest.raises(PrologRLMError) as caught:
        await backend.start()

    assert caught.value.kind == "sidecar_startup_failed"


def test_nix_sidecar_probe_loads_pinned_runtime():
    rlm_root = os.getenv("ZARA_PROLOG_RLM_ROOT")
    if not rlm_root:
        pytest.skip("ZARA_PROLOG_RLM_ROOT is injected by the Zara Nix environment")
    sidecar = Path(__file__).parents[1] / "modules" / "rlm_sidecar.pl"
    completed = subprocess.run(
        [
            "swipl",
            "-q",
            "-f",
            "none",
            "-s",
            str(sidecar),
            "--",
            "--probe",
            rlm_root,
        ],
        check=False,
        capture_output=True,
        text=True,
        timeout=20,
    )
    assert completed.returncode == 0, completed.stderr
    payload = json.loads(completed.stdout)
    assert payload == {"type": "ready", "revision": PROLOG_RLM_REVISION}
