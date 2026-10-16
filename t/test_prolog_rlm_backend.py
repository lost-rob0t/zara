from __future__ import annotations

import asyncio
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import json
from threading import Thread

import pytest

from zara.runtime.prolog_rlm import PrologRLMRuntimeBackend


class Handler(BaseHTTPRequestHandler):
    requests = []

    def log_message(self, *_args):
        pass

    def do_GET(self):
        body = {"runtimes": [{"id": "prolog-rlm", "protocol": "ZARA-RUNTIME/1", "available": True, "installed": True, "health": "ready", "supports_cancel": True}]}
        self.reply(body)

    def do_POST(self):
        payload = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
        self.requests.append((self.path, payload))
        if self.path.endswith("cancel"):
            self.reply({"protocol": "ZARA-RUNTIME/1", "runtime_id": "prolog-rlm", "request_id": payload["request_id"], "status": "cancel_requested"})
        else:
            self.reply({"protocol": "ZARA-RUNTIME/1", "runtime_id": "prolog-rlm", "request_id": payload["request_id"], "status": "completed", "text": "from prolog"})

    def reply(self, value):
        body = json.dumps(value).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)


@pytest.fixture
def sidecar():
    Handler.requests = []
    server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    thread = Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        yield f"http://127.0.0.1:{server.server_port}/zara-runtime/v1"
    finally:
        server.shutdown()
        thread.join()
        server.server_close()


def test_real_sidecar_turn_and_cancel(sidecar):
    async def run():
        backend = PrologRLMRuntimeBackend(sidecar, principal_id="user:alice")
        await backend.start()
        result = await backend.submit_turn("Solve this", turn_id="turn-123", system_context="Use code")
        assert result.response == "from prolog"
        await backend.cancel_turn("turn-123")
        assert backend.principal_id == "user:alice"
    asyncio.run(run())
    path, request = Handler.requests[0]
    assert path.endswith("generate")
    assert request["prompt"] == "Solve this"
    assert request["inline_context"] == "Use code"
    assert request["mode"] == "rlm"
    assert Handler.requests[1][0].endswith("cancel")


def test_sidecar_requires_loopback():
    with pytest.raises(ValueError, match="loopback"):
        PrologRLMRuntimeBackend("http://example.com:8000/zara-runtime/v1", principal_id="user:alice")


def test_sidecar_rejects_protocol_mismatch(sidecar):
    original = Handler.do_GET
    def wrong(self):
        self.reply({"runtimes": [{"id": "prolog-rlm", "protocol": "ZARA-RUNTIME/2", "available": True, "installed": True, "health": "ready"}]})
    Handler.do_GET = wrong
    try:
        with pytest.raises(RuntimeError, match="protocol"):
            asyncio.run(PrologRLMRuntimeBackend(sidecar, principal_id="user:alice").start())
    finally:
        Handler.do_GET = original


def test_sidecar_rejects_mismatched_request_identity(sidecar):
    original = Handler.do_POST
    def wrong(self):
        payload = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
        self.reply({"protocol": "ZARA-RUNTIME/1", "runtime_id": "prolog-rlm", "request_id": payload["request_id"] + "-other", "status": "completed", "text": "unsafe"})
    Handler.do_POST = wrong
    try:
        async def run():
            backend = PrologRLMRuntimeBackend(sidecar, principal_id="user:alice")
            await backend.start()
            with pytest.raises(RuntimeError, match="identity mismatch"):
                await backend.submit_turn("hello", turn_id="turn-1")
        asyncio.run(run())
    finally:
        Handler.do_POST = original


def test_sidecar_reports_runtime_failure(sidecar):
    original = Handler.do_POST
    def failed(self):
        payload = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
        self.reply({"protocol": "ZARA-RUNTIME/1", "runtime_id": "prolog-rlm", "request_id": payload["request_id"], "status": "failed", "error": {"message": "model unavailable"}})
    Handler.do_POST = failed
    try:
        async def run():
            backend = PrologRLMRuntimeBackend(sidecar, principal_id="user:alice")
            await backend.start()
            with pytest.raises(RuntimeError, match="model unavailable"):
                await backend.submit_turn("hello", turn_id="turn-1")
        asyncio.run(run())
    finally:
        Handler.do_POST = original


def test_runtime_host_starts_with_rlm_but_exposes_no_unbridged_host_tools(sidecar, tmp_path):
    from zara.config import ZaraConfig
    from zara.runtime.host import RuntimeHost, RuntimeHostState

    config_path = tmp_path / "config.toml"
    config_path.write_text(
        '[agent]\nbackend = "prolog_rlm"\n'
        f'prolog_rlm_endpoint = "{sidecar}"\n'
        '[tasks]\nenabled = true\n'
    )
    config = ZaraConfig(str(config_path))
    backend = PrologRLMRuntimeBackend(sidecar, principal_id="user:alice")
    published = []
    host = RuntimeHost(
        lambda: backend,
        config=config,
        publisher=published.append,
    )
    try:
        host.start().result(timeout=10)
        assert host.state is RuntimeHostState.RUNNING
        assert host._task_runner is None
        assert any("task runner startup failed" in event.reason for event in published if hasattr(event, "reason"))
    finally:
        host.shutdown("test cleanup").result(timeout=10)
        host.join(timeout=10)
