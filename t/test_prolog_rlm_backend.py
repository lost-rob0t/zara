from __future__ import annotations

import asyncio
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import json
import subprocess
from threading import Thread

import pytest
from langchain_core.tools import StructuredTool

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


def test_operator_selected_sidecar_provider_is_sent_without_credentials(sidecar):
    provider = {"kind": "openai_compatible", "endpoint": "http://127.0.0.1:19876/v1",
                "model": "fixture", "no_credential": True}

    async def run():
        backend = PrologRLMRuntimeBackend(sidecar, principal_id="user:alice", provider=provider)
        await backend.start()
        await backend.submit_turn("test", turn_id="turn-provider")
        await backend.stop()

    asyncio.run(run())
    assert Handler.requests[0][1]["provider"] == provider
    with pytest.raises(ValueError, match="provider"):
        PrologRLMRuntimeBackend(sidecar, principal_id="user:alice",
                                provider={**provider, "api_key": "literal-secret"})


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


def test_rlm_backend_stages_plugin_tools_without_publishing_early(sidecar):
    backend = PrologRLMRuntimeBackend(sidecar, principal_id="user:alice")
    tool = StructuredTool.from_function(
        lambda: "ready", name="code_task_status", description="Read task status"
    )
    backend.register_tools((tool,))
    assert backend._registered_tools == {"code_task_status": tool}
    with pytest.raises(ValueError, match="already registered"):
        backend.register_tools((tool,))
    backend.unregister_tools(("code_task_status",))
    assert backend._registered_tools == {}
    backend.register_tools((tool,))
    asyncio.run(backend.stop())
    assert backend._registered_tools == {}


def test_rlm_backend_finalizes_registered_effect_tools(sidecar, tmp_path):
    from zara.tasks.effect_journal import RepositoryEffectJournal
    from zara.plugins.builtin.code_environment import CodeTasksArgs, CodeTaskLookupArgs

    subprocess.run(["git", "init", "-q", str(tmp_path)], check=True)
    tool = StructuredTool.from_function(
        lambda tasks: "task-123", name="code_tasks", description="Start coding tasks",
        metadata={"zara_requires_approval": True}, args_schema=CodeTasksArgs,
    )
    status_tool = StructuredTool.from_function(
        lambda project, task_id: "ready", name="code_task_status",
        description="Read coding task status", args_schema=CodeTaskLookupArgs,
        metadata={"zara_read_only": True},
    )

    async def run():
        backend = PrologRLMRuntimeBackend(
            sidecar, principal_id="user:alice", host_token="abcdefghijklmnop",
            effect_journal=RepositoryEffectJournal(tmp_path),
        )
        await backend.start()
        backend.register_tools((tool, status_tool))
        await backend.finalize_tools()
        await backend.stop()

    asyncio.run(run())
    config = [body for path, body in Handler.requests if path.endswith("host-config")]
    assert len(config) == 1
    assert [entry["name"] for entry in config[0]["tools"]] == ["code_task_status", "code_tasks"]
    assert config[0]["tools"][1]["effect"] == "process"
    assert config[0]["effect_endpoint"].endswith("/zara-host-tool/v1/effect")


def test_native_approval_resumes_the_original_rlm_turn(sidecar, tmp_path):
    from zara.runtime.events import ToolWaitingForUser
    from zara.tasks.effect_journal import RepositoryEffectJournal

    subprocess.run(["git", "init", "-q", str(tmp_path)], check=True)

    original = Handler.do_POST
    approval = {
        "id": "approval-1", "name": "code_tasks", "effect": "process",
        "args": {"tasks": [{"task": "fix bug", "project": "work"}]},
        "fingerprint": "authority-fingerprint", "attempt_id": "attempt-1",
        "effect_fingerprint": "effect-fingerprint", "idempotency_key": "key-1",
    }

    def approval_response(self):
        payload = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
        Handler.requests.append((self.path, payload))
        if self.path.endswith("host-config"):
            Handler.configured = True
            self.reply({"protocol": "ZARA-RUNTIME/1", "status": "configured", "generation": payload["generation"]})
        elif self.path.endswith("generate"):
            self.reply({"protocol": "ZARA-RUNTIME/1", "runtime_id": "prolog-rlm",
                        "request_id": payload["request_id"], "generation": Handler.effect_generation,
                        "status": "approval_required", "approvals": [approval]})
        elif self.path.endswith("host-pending"):
            self.reply({"protocol": "ZARA-RUNTIME/1", "request_id": payload["request_id"],
                        "generation": Handler.effect_generation, "status": "approval_required",
                        "approvals": [approval]})
        elif self.path.endswith("host-approval"):
            self.reply({"protocol": "ZARA-RUNTIME/1", "request_id": payload["request_id"],
                        "generation": Handler.effect_generation, "approval_id": "approval-1", "status": "accepted"})
        elif self.path.endswith("host-approval-status"):
            self.reply({"protocol": "ZARA-RUNTIME/1", "request_id": payload["request_id"],
                        "generation": Handler.effect_generation, "approval_id": "approval-1", "status": "completed",
                        "value": {"result": "task-123"}})
        elif self.path.endswith("resume"):
            self.reply({"protocol": "ZARA-RUNTIME/1", "runtime_id": "prolog-rlm",
                        "request_id": payload["request_id"], "status": "completed",
                        "text": "I started task-123"})
        else:
            original(self)

    Handler.do_POST = approval_response
    try:
        async def run():
            backend = PrologRLMRuntimeBackend(
                sidecar, principal_id="user:alice",
                read_tooling=FileTooling(FileToolingConfig(base_dir=tmp_path)),
                host_token="abcdefghijklmnop",
                effect_journal=RepositoryEffectJournal(tmp_path),
            )
            backend.register_tools((StructuredTool.from_function(
                lambda tasks: "task-123", name="code_tasks", description="Start coding tasks"
            ),))
            Handler.effect_generation = backend._generation
            seen = []
            waiting = asyncio.Event()

            def publish(event):
                seen.append(event)
                if isinstance(event, ToolWaitingForUser):
                    waiting.set()

            backend.bind_event_publisher(publish)
            await backend.start()
            lease = TurnCapabilityLease("turn-approve")
            with bind_turn_capability_lease(lease):
                task = asyncio.create_task(backend.submit_turn("needs approval", turn_id="turn-approve"))
                await asyncio.wait_for(waiting.wait(), timeout=5)
                assert not task.done()
                await backend.approve_tool("approval-1")
                result = await asyncio.wait_for(task, timeout=5)
            assert result.response == "I started task-123"
            assert seen[0].tool_run_id == "approval-1"
            await backend.stop()

        asyncio.run(run())
        assert [path.rsplit("/", 1)[-1] for path, _ in Handler.requests[1:]] == [
            "generate", "host-pending", "host-approval", "host-approval-status", "resume"
        ]
    finally:
        Handler.do_POST = original


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
