from __future__ import annotations

import asyncio
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import json
import subprocess
from threading import Thread

import pytest

from zara.runtime.prolog_rlm import PrologRLMRuntimeBackend
from zara.runtime.turn_context import TurnCapabilityLease, bind_turn_capability_lease
from zara.agent.tools.file_tools import FileTooling, FileToolingConfig


class Handler(BaseHTTPRequestHandler):
    requests = []
    authorizations = []
    configured = False

    def log_message(self, *_args):
        pass

    def do_GET(self):
        body = {"runtimes": [{"id": "prolog-rlm", "protocol": "ZARA-RUNTIME/1", "available": True, "installed": True, "health": "ready", "supports_cancel": True, "supports_host_tools": self.configured}]}
        self.reply(body)

    def do_POST(self):
        payload = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
        self.requests.append((self.path, payload))
        self.authorizations.append(self.headers.get("Authorization"))
        if self.path.endswith("host-config"):
            type(self).configured = True
            self.reply({"protocol": "ZARA-RUNTIME/1", "status": "configured", "generation": payload["generation"]})
        elif self.path.endswith("cancel"):
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
    Handler.authorizations = []
    Handler.configured = False
    server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    thread = Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        yield f"http://127.0.0.1:{server.server_port}/zara-runtime/v1"
    finally:
        server.shutdown()
        thread.join()
        server.server_close()


def test_loopback_protocol_turn_and_cancel(sidecar):
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


def test_read_tool_bootstrap_and_turn_fence(sidecar, tmp_path):
    (tmp_path / "note.txt").write_text("hello")
    tooling = FileTooling(FileToolingConfig(base_dir=tmp_path))

    async def run():
        backend = PrologRLMRuntimeBackend(
            sidecar, principal_id="user:alice", read_tooling=tooling, host_token="test-secret"
        )
        await backend.start()
        lease = TurnCapabilityLease("turn-read")
        with bind_turn_capability_lease(lease):
            await backend.submit_turn("read note", turn_id="turn-read")
        await backend.stop()

    asyncio.run(run())
    config_path, config = Handler.requests[0]
    assert config_path.endswith("host-config")
    assert config["endpoint"].startswith("http://127.0.0.1:")
    assert config["tools"][0]["name"] == "file_read"
    assert Handler.requests[1][1]["host_tools"] == ["file_read"]
    assert Handler.authorizations == ["Bearer test-secret", "Bearer test-secret"]


def test_server_resolves_rlm_file_read_from_prolog_repository(sidecar, tmp_path, monkeypatch):
    from zara.config import ZaraConfig
    from zara.principals import PrincipalContext
    from zara.runtime.bridge import RuntimeEventBus
    from zara.server_core import RuntimeSupervisor

    repository = tmp_path / "repository"
    repository.mkdir()
    subprocess.run(["git", "init", "-q", str(repository)], check=True)
    config_file = tmp_path / "config.toml"
    config_file.write_text(
        '[agent]\nbackend = "prolog_rlm"\n'
        f'prolog_rlm_endpoint = "{sidecar}"\n'
        '[tools]\nfile_tools = true\n'
        '[file_tools]\nrepository = "work"\n'
    )

    class Prolog:
        def get_wake_words(self):
            return []

        def query_all(self, query):
            if query == "kb_config:file_repository(Alias, Path)":
                return [{"Alias": "work", "Path": str(repository)}]
            if query == "file_skill_resolver:resolved_tool(Skill, Tool)":
                return [{"Skill": "read", "Tool": "read_file"}]
            raise AssertionError(query)

    monkeypatch.setenv("ZARA_RUNTIME_HOST_TOKEN", "test-secret")
    supervisor = RuntimeSupervisor(config=ZaraConfig(str(config_file)), prolog_factory=lambda _: Prolog())
    host = supervisor._build_default_host(PrincipalContext("user:alice"), RuntimeEventBus())
    backend = host._backend_factory()
    assert backend._callback._tooling.base_dir == repository
    assert backend._host_token == "test-secret"


def test_sidecar_requires_loopback():
    for endpoint in (
        "http://example.com:8000/zara-runtime/v1",
        "http://localhost:8000/zara-runtime/v1",
    ):
        with pytest.raises(ValueError, match="127.0.0.1"):
            PrologRLMRuntimeBackend(endpoint, principal_id="user:alice")


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


def test_rlm_config_rejects_non_loopback_endpoint(tmp_path):
    from zara.config import ConfigError, ZaraConfig

    path = tmp_path / "config.toml"
    path.write_text(
        '[agent]\nbackend = "prolog_rlm"\n'
        'prolog_rlm_endpoint = "http://example.com:18765/zara-runtime/v1"\n'
    )
    with pytest.raises(ConfigError, match="prolog_rlm_endpoint"):
        ZaraConfig(str(path))


def test_history_is_committed_only_after_host_fence(sidecar):
    async def run():
        backend = PrologRLMRuntimeBackend(sidecar, principal_id="user:alice")
        await backend.start()
        first = await backend.submit_turn("first", turn_id="turn-1", conversation_id="chat-a")
        await backend.submit_turn("stale", turn_id="turn-2", conversation_id="chat-a")
        assert Handler.requests[-1][1]["inline_context"] == ""
        backend.commit_turn_result(first, turn_id="turn-1", conversation_id="chat-a")
        await backend.submit_turn("next", turn_id="turn-3", conversation_id="chat-a")
        context = Handler.requests[-1][1]["inline_context"]
        assert "user: first" in context
        assert "assistant: from prolog" in context
        assert "stale" not in context
        await backend.submit_turn("other", turn_id="turn-4", conversation_id="chat-b")
        assert Handler.requests[-1][1]["inline_context"] == ""
        await backend.submit_turn("task", turn_id="turn-5", conversation_id="chat-a", conversation_history=[])
        assert Handler.requests[-1][1]["inline_context"] == ""
    asyncio.run(run())


def test_conversation_history_has_global_bound(sidecar):
    async def run():
        backend = PrologRLMRuntimeBackend(sidecar, principal_id="user:alice")
        await backend.start()
        for index in range(33):
            conversation_id = f"chat-{index}"
            result = await backend.submit_turn(
                f"message-{index}", turn_id=f"turn-{index}", conversation_id=conversation_id
            )
            backend.commit_turn_result(
                result, turn_id=f"turn-{index}", conversation_id=conversation_id
            )
        await backend.submit_turn("again", turn_id="turn-new", conversation_id="chat-0")
        assert Handler.requests[-1][1]["inline_context"] == ""
    asyncio.run(run())
