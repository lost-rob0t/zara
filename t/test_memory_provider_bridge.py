from __future__ import annotations

from types import SimpleNamespace
import asyncio
import threading

import pytest

import zara.agent as agent_module
from zara.agent import AgentManager
from zara.agent.tools.registry import ToolRegistry
from zara.memory import memory_provider_plugin
from zara.principals import PrincipalContext
from zara.runtime.backend import LangGraphRuntimeBackend
from zara.runtime.backend import RuntimeBackend
from zara.runtime.commands import SubmitTurn
from zara.runtime.commands import RestartRuntime
from zara.runtime.host import RuntimeHost, RuntimeNotReady


@pytest.mark.parametrize("settings, expected", [({}, None), ({"provider": "builtin"}, None), ({"provider": "plugin:notes"}, "notes"), ({"provider": "plugin:notes", "enabled": False}, None)])
def test_memory_provider_selector(settings, expected):
    assert memory_provider_plugin(settings) == expected


@pytest.mark.parametrize("value", [None, True, 1, "", "chroma", "plugin:", "plugin:../notes", "plugin:Notes", "plugin:notes "])
def test_invalid_memory_provider_selection_fails_closed(value):
    with pytest.raises(ValueError, match="memory.provider"):
        memory_provider_plugin({"provider": value})


@pytest.mark.parametrize("value", ["false", 0, None])
def test_memory_enabled_requires_an_explicit_boolean(value):
    with pytest.raises(ValueError, match="memory.enabled"):
        memory_provider_plugin({"provider": "plugin:notes", "enabled": value})


class Memory:
    def __init__(self, principal):
        self.principal_id = principal.principal_id
        self.principal = principal
        self.bound = []
        self.facts = []
        self.messages = []

    def bind_principal(self, principal):
        self.bound.append(principal)
        self.principal = principal
        self.principal_id = principal.principal_id

    def remember_fact(self, text, **kwargs):
        self.facts.append(text)
        return "fact-1"

    def retrieve(self, query, k=5):
        return [{"text": text, "metadata": {}} for text in self.facts[:k]]

    def list_memories(self, **kwargs):
        return []

    def forget(self, **kwargs):
        return 0

    def start_session(self):
        return "session-1"

    def add_message(self, session, role, text):
        self.messages.append((session, role, text))

    def summarise_session(self, session):
        return None


def bare_manager():
    manager = AgentManager.__new__(AgentManager)
    manager.principal = PrincipalContext("user:alice", "authenticated")
    manager.memory_manager = None
    manager.tool_registry = ToolRegistry()
    manager.config = SimpleNamespace(get_tool_config=lambda: {})
    manager.memory_top_k = 5
    manager.memory_context_limit = 1200
    return manager


def test_selected_plugin_does_not_construct_builtin_memory(monkeypatch, tmp_path):
    from zara.config import ZaraConfig
    path = tmp_path / "config.toml"
    path.write_text('[memory]\nprovider = "plugin:notes"\n[modules]\nsearch_paths = []\n')
    config = ZaraConfig(str(path))
    monkeypatch.setattr(AgentManager, "_create_llm_client", lambda self, settings: object())
    monkeypatch.setattr(agent_module, "build_memory_manager", lambda *args, **kwargs: pytest.fail("Builtin memory must not initialize"))
    manager = AgentManager(config=config, principal=PrincipalContext.local_owner())
    assert manager.memory_manager is None
    assert "remember" not in manager.tool_registry.list_tools()
    with pytest.raises(RuntimeError, match="not bound"):
        asyncio.run(manager.process_async("Private input before binding"))


def test_memory_replacement_preserves_disabled_tools_and_rebinds_old_closures():
    manager = bare_manager()
    first = Memory(manager.principal)
    second = Memory(manager.principal)
    manager.replace_memory_manager(first)
    manager.tool_registry.get_tool("remember").invoke({"text": "old"})
    manager.config = SimpleNamespace(get_tool_config=lambda: {"forget": False})
    manager.replace_memory_manager(second)
    manager.tool_registry.get_tool("remember").invoke({"text": "new"})
    assert first.facts == ["old"]
    assert second.facts == ["new"]
    assert "forget" not in manager.tool_registry.list_tools()


def test_memory_binding_cannot_overwrite_a_plugin_owned_tool():
    from langchain_core.tools import StructuredTool
    manager = bare_manager()
    existing = StructuredTool.from_function(lambda text: "external", name="remember", description="Plugin-owned")
    manager.tool_registry.register_tools([existing])
    with pytest.raises(ValueError, match="owned"):
        manager.replace_memory_manager(Memory(manager.principal))
    assert manager.memory_manager is None
    assert manager.tool_registry.get_tool("remember").description == "Plugin-owned"


def test_failed_replacement_removes_partial_new_tools_and_restores_old_provider(monkeypatch):
    manager = bare_manager()
    previous = Memory(manager.principal)
    replacement = Memory(manager.principal)
    manager.replace_memory_manager(previous)
    register = manager.tool_registry.register_tools
    attempts = []

    def partial_registration(tools):
        attempts.append(1)
        if len(attempts) == 1:
            register(tools[:1])
            raise RuntimeError("synthetic registration failure")
        register(tools)

    monkeypatch.setattr(manager.tool_registry, "register_tools", partial_registration)
    with pytest.raises(RuntimeError, match="synthetic registration failure"):
        manager.replace_memory_manager(replacement)
    assert manager.memory_manager is previous
    assert set(manager.tool_registry.list_tools()) == {"remember", "recall", "memory_list", "forget"}
    manager.tool_registry.get_tool("remember").invoke({"text": "preserved"})
    assert previous.facts == ["preserved"]
    assert replacement.facts == []


@pytest.mark.asyncio
async def test_binding_uses_principal_replaces_only_memory_tools_and_persists_through_provider():
    from langchain_core.tools import StructuredTool
    manager = bare_manager()
    other_tool = StructuredTool.from_function(lambda: "other", name="other", description="Other tool")
    manager.tool_registry.register_tools([other_tool])
    memory = Memory(manager.principal)
    calls = []

    def bind(principal):
        calls.append(principal)

    backend = LangGraphRuntimeBackend(manager_factory=lambda: manager)
    await backend.start()
    memory.bind_principal = bind
    await backend.bind_memory_provider(memory)
    assert calls == [manager.principal]
    assert manager.memory_manager is memory
    assert set(manager.tool_registry.list_tools()) == {"other", "remember", "recall", "memory_list", "forget"}
    assert manager.tool_registry.get_tool("other").description == "Other tool"
    manager.tool_registry.get_tool("remember").invoke({"text": "A durable preference"})
    assert "A durable preference" in manager._build_memory_context("preference")
    await backend._persist_turn("hello", "answer")
    assert memory.messages == [("session-1", "user", "hello"), ("session-1", "assistant", "answer")]


@pytest.mark.asyncio
async def test_mismatched_principal_never_replaces_memory_or_tools():
    manager = bare_manager()
    memory = Memory(PrincipalContext("user:bob", "authenticated"))
    backend = LangGraphRuntimeBackend(manager_factory=lambda: manager)
    await backend.start()
    with pytest.raises(ValueError, match="principal"):
        memory.bind_principal = lambda principal: None
        await backend.bind_memory_provider(memory)
    assert manager.memory_manager is None
    assert manager.tool_registry.list_tools() == []


@pytest.mark.asyncio
async def test_provider_cannot_change_principal_kind_while_preserving_the_id():
    manager = bare_manager()
    memory = Memory(PrincipalContext(manager.principal.principal_id, "guest"))
    backend = LangGraphRuntimeBackend(manager_factory=lambda: manager)
    await backend.start()
    with pytest.raises(ValueError, match="principal"):
        memory.bind_principal = lambda principal: None
        await backend.bind_memory_provider(memory)
    assert manager.memory_manager is None


@pytest.mark.asyncio
async def test_shared_provider_cannot_be_rebound_to_another_runtime_principal():
    alice = bare_manager()
    bob = bare_manager()
    bob.principal = PrincipalContext("user:bob", "authenticated")
    provider = Memory(alice.principal)
    alice_backend = LangGraphRuntimeBackend(manager_factory=lambda: alice)
    bob_backend = LangGraphRuntimeBackend(manager_factory=lambda: bob)
    await alice_backend.start()
    await bob_backend.start()
    await alice_backend.bind_memory_provider(provider)
    with pytest.raises(ValueError, match="principal"):
        await bob_backend.bind_memory_provider(provider)
    assert provider.principal == alice.principal
    assert provider.bound == [alice.principal]
    assert bob.memory_manager is None


@pytest.mark.asyncio
async def test_concurrent_binding_of_one_unbound_provider_cannot_cross_principals():
    alice = bare_manager()
    bob = bare_manager()
    bob.principal = PrincipalContext("user:bob", "authenticated")
    entered = threading.Event()
    release = threading.Event()

    class SharedMemory(Memory):
        def __init__(self):
            super().__init__(alice.principal)
            self.principal = None
            self.principal_id = None

        def bind_principal(self, principal):
            if principal == alice.principal:
                entered.set()
                assert release.wait(timeout=2)
            super().bind_principal(principal)

    provider = SharedMemory()
    alice_backend = LangGraphRuntimeBackend(manager_factory=lambda: alice)
    bob_backend = LangGraphRuntimeBackend(manager_factory=lambda: bob)
    await alice_backend.start()
    await bob_backend.start()
    first = asyncio.create_task(alice_backend.bind_memory_provider(provider))
    assert await asyncio.to_thread(entered.wait, 1)
    second = asyncio.create_task(bob_backend.bind_memory_provider(provider))
    try:
        with pytest.raises(asyncio.TimeoutError):
            await asyncio.wait_for(asyncio.shield(second), timeout=0.1)
    finally:
        release.set()
    await first
    with pytest.raises(ValueError, match="principal"):
        await second
    assert provider.principal == alice.principal
    assert bob.memory_manager is None


class ProviderConfig:
    def __init__(self, provider="plugin:memory-service", *, enabled=True, memory_enabled=True):
        self.provider = provider
        self.enabled = enabled
        self.memory_enabled = memory_enabled

    def get_section(self, name):
        return {"provider": self.provider, "enabled": self.memory_enabled} if name == "memory" else {}

    def get_api_service_config(self):
        return {"enabled": False}

    def get_plugin_runtime_config(self):
        return {"lifecycle_timeout": 1, "event_queue_size": 4, "max_managed_workers": 2}

    def get_plugin_config(self, name):
        return {"enabled": self.enabled and name == "memory-service"}


class BindingBackend(RuntimeBackend):
    def __init__(self):
        self.bound = []

    async def bind_memory_provider(self, provider):
        self.bound.append(provider)


def test_runtime_binds_selected_plugin_symbol_after_startup(tmp_path):
    provider_plugin(tmp_path / "memory_plugin.py")
    backend = BindingBackend()
    host = RuntimeHost(lambda: backend, config=ProviderConfig(), plugin_paths=(tmp_path,))
    try:
        host.start().result(timeout=5)
        assert backend.bound == ["selected-provider"]
    finally:
        host.shutdown().result(timeout=5)
        host.join(timeout=5)


@pytest.mark.parametrize("provider", ["plugin:missing", "not-a-provider"])
def test_missing_or_invalid_selected_provider_fails_startup_and_rejects_turns(tmp_path, provider):
    backend = BindingBackend()
    host = RuntimeHost(lambda: backend, config=ProviderConfig(provider), plugin_paths=(tmp_path,))
    try:
        with pytest.raises((RuntimeError, ValueError)):
            host.start().result(timeout=5)
        with pytest.raises(RuntimeNotReady):
            host.submit(SubmitTurn(text="A private question")).result(timeout=2)
        assert backend.bound == []
    finally:
        host.shutdown().result(timeout=5)
        host.join(timeout=5)


def provider_plugin(path):
    path.write_text('''from zara.plugins import PluginMetadata, ServicePlugin
class Plugin(ServicePlugin):
    metadata = PluginMetadata(name="memory-service")
    def start(self, runtime):
        runtime.register_symbol("memory.provider", "memory-provider", "selected-provider")
    def stop(self):
        pass
def create_plugin():
    return Plugin()
''')


def test_turns_wait_for_binding_and_removed_provider_cannot_be_reused(tmp_path):
    provider_plugin(tmp_path / "memory_plugin.py")
    entered = threading.Event()
    release = threading.Event()

    class SlowBackend(BindingBackend):
        async def bind_memory_provider(self, provider):
            entered.set()
            await asyncio.to_thread(release.wait, 2)
            await super().bind_memory_provider(provider)

    backend = SlowBackend()
    host = RuntimeHost(lambda: backend, config=ProviderConfig(), plugin_paths=(tmp_path,))
    try:
        started = host.start()
        assert entered.wait(timeout=2)
        with pytest.raises(RuntimeNotReady):
            host.submit(SubmitTurn(text="private while binding")).result(timeout=2)
        release.set()
        started.result(timeout=5)
        host._symbol_registry.clear_owner("plugin:memory-service")
        with pytest.raises(RuntimeNotReady):
            host.submit(SubmitTurn(text="private after removal")).result(timeout=2)
    finally:
        release.set()
        host.shutdown().result(timeout=5)
        host.join(timeout=5)


def test_disabled_provider_fails_closed(tmp_path):
    provider_plugin(tmp_path / "memory_plugin.py")
    backend = BindingBackend()
    host = RuntimeHost(lambda: backend, config=ProviderConfig(enabled=False), plugin_paths=(tmp_path,))
    try:
        with pytest.raises(RuntimeError, match="unavailable"):
            host.start().result(timeout=5)
        assert backend.bound == []
    finally:
        host.shutdown().result(timeout=5)
        host.join(timeout=5)


@pytest.mark.parametrize("mode", ["missing_symbol", "wrong_kind", "wrong_owner", "unavailable"])
def test_provider_contract_and_availability_are_checked_before_binding(tmp_path, mode):
    path = tmp_path / "memory_plugin.py"
    provider_plugin(path)
    source = path.read_text()
    if mode == "missing_symbol":
        source = source.replace('runtime.register_symbol("memory.provider", "memory-provider", "selected-provider")', "pass")
    elif mode == "wrong_kind":
        source = source.replace('"memory-provider", "selected-provider"', '"variable", "selected-provider"')
    elif mode == "unavailable":
        source = source.replace("PluginMetadata, ServicePlugin", "PluginMetadata, ServicePlugin, StartupUnavailable")
        source = source.replace('"selected-provider")', '"selected-provider")\n        return StartupUnavailable("missing_credentials")')
    path.write_text(source)
    backend = BindingBackend()
    host = RuntimeHost(lambda: backend, config=ProviderConfig(), plugin_paths=(tmp_path,))
    if mode == "wrong_owner":
        host._symbol_registry.register(symbol="memory.provider", kind="memory-provider", owner="plugin:other", value="wrong", layer="user")
    try:
        with pytest.raises(RuntimeError):
            host.start().result(timeout=5)
        assert backend.bound == []
    finally:
        host.shutdown().result(timeout=5)
        host.join(timeout=5)


def test_binding_timeout_is_visible_and_never_accepts_turns(tmp_path):
    provider_plugin(tmp_path / "memory_plugin.py")
    cancelled = threading.Event()

    class TimeoutBackend(BindingBackend):
        async def bind_memory_provider(self, provider):
            try:
                await asyncio.sleep(10)
            finally:
                cancelled.set()

    config = ProviderConfig()
    config.get_plugin_runtime_config = lambda: {"lifecycle_timeout": 0.2, "event_queue_size": 4, "max_managed_workers": 2}
    backend = TimeoutBackend()
    host = RuntimeHost(lambda: backend, config=config, plugin_paths=(tmp_path,))
    try:
        with pytest.raises(RuntimeError, match="binding failed"):
            host.start().result(timeout=5)
        assert cancelled.is_set()
        assert backend.bound == []
        with pytest.raises(RuntimeNotReady):
            host.submit(SubmitTurn(text="private input")).result(timeout=2)
    finally:
        host.shutdown().result(timeout=5)
        host.join(timeout=5)


def test_restart_rebinds_new_plugin_generation_and_plugin_failure_blocks_turns(tmp_path):
    provider_plugin(tmp_path / "memory_plugin.py")
    backend = BindingBackend()
    host = RuntimeHost(lambda: backend, config=ProviderConfig(), plugin_paths=(tmp_path,))
    try:
        host.start().result(timeout=5)
        previous = host._bound_memory_registration
        host.submit(RestartRuntime()).result(timeout=5)
        assert host._bound_memory_registration != previous
        assert backend.bound == ["selected-provider", "selected-provider"]
        record = next(item for item in host._plugin_manager._records if item.metadata.name == "memory-service")
        host._plugin_manager._runtime_failed(record, "synthetic provider worker failure")
        with pytest.raises(RuntimeNotReady):
            host.submit(SubmitTurn(text="private after provider failure")).result(timeout=2)
    finally:
        host.shutdown().result(timeout=5)
        host.join(timeout=5)


def test_explicit_configuration_repair_can_restart_a_degraded_host(tmp_path):
    config = ProviderConfig("plugin:missing")
    host = RuntimeHost(lambda: BindingBackend(), config=config, plugin_paths=(tmp_path,))
    try:
        with pytest.raises(RuntimeError, match="unavailable"):
            host.start().result(timeout=5)
        config.provider = "builtin"
        host.submit(RestartRuntime()).result(timeout=5)
        assert host.state.value == "running"
        assert host._memory_provider_ready
        assert host._selected_memory_plugin is None
    finally:
        host.shutdown().result(timeout=5)
        host.join(timeout=5)


def test_disabled_memory_does_not_bind_a_selected_plugin(tmp_path):
    backend = BindingBackend()
    config = ProviderConfig("plugin:missing", memory_enabled=False)
    host = RuntimeHost(lambda: backend, config=config, plugin_paths=(tmp_path,))
    try:
        host.start().result(timeout=5)
        assert backend.bound == []
        assert host._selected_memory_plugin is None
    finally:
        host.shutdown().result(timeout=5)
        host.join(timeout=5)
