from __future__ import annotations

from unittest.mock import MagicMock

from zara.server import PrincipalContext, RuntimeSupervisor


class MinimalRuntimeConfig:
    def get(self, section, key, default=None):
        if section == "agent" and key == "backend":
            return "langgraph"
        return default

    def get_llm_config(self):
        return {
            "provider": "ollama",
            "model": "test-model",
            "endpoint": "http://127.0.0.1:11434/api/chat",
        }

    def get_section(self, name):
        if name == "memory":
            return {
                "enabled": False,
                "top_k": 5,
                "max_chars": 1200,
            }
        if name == "agent":
            return {"conversation_timeout": 60}
        return {}

    def get_module_search_paths(self):
        return []

    def get_plugin_runtime_config(self):
        return {
            "lifecycle_timeout": 0.2,
            "event_queue_size": 4,
            "max_managed_workers": 1,
        }

    def get_plugin_config(self, _name):
        return {}


class DummyToolRegistry:
    def register_tools(self, _tools):
        return None

    def unregister_tools(self, _names):
        return None


class PrincipalCapturingManager:
    instances = []

    def __init__(self, *, config, principal, prolog_engine=None):
        self.config = config
        self.principal = principal
        self.prolog_engine = prolog_engine
        self.tool_registry = DummyToolRegistry()
        type(self).instances.append(self)

    async def shutdown_async(self):
        return None


def principal(name: str) -> PrincipalContext:
    return PrincipalContext(principal_id=f"user:{name}", kind="authenticated")


def test_default_supervisor_threads_each_principal_into_agent_manager(monkeypatch):
    from zara import agent, config as config_module

    config = MinimalRuntimeConfig()
    PrincipalCapturingManager.instances = []
    monkeypatch.setattr(agent, "AgentManager", PrincipalCapturingManager)
    monkeypatch.setattr(config_module, "get_config", lambda: config)

    supervisor = RuntimeSupervisor(
        config=config,
        max_active_principals=2,
        shutdown_timeout=0.5,
        prolog_factory=lambda _principal: MagicMock(name="prolog"),
    )
    alice = principal("alice")
    bob = principal("bob")

    try:
        alice_runtime = supervisor.start(alice)
        assert alice_runtime.startup_error is None
        bob_runtime = supervisor.open_principal(bob)
        assert bob_runtime.startup_error is None
        assert [manager.principal for manager in PrincipalCapturingManager.instances] == [
            alice,
            bob,
        ]
    finally:
        supervisor.shutdown()


def test_agent_manager_binds_memory_manager_to_explicit_principal(monkeypatch):
    from zara.agent import AgentManager
    from zara.agent.tools.registry import ToolRegistry

    config = MinimalRuntimeConfig()
    alice = principal("alice")

    monkeypatch.setattr(
        AgentManager,
        "_create_llm_client",
        lambda self, _llm_config: object(),
    )
    monkeypatch.setattr(ToolRegistry, "load_builtin_tools", lambda self, memory_manager=None: None)
    monkeypatch.setattr(ToolRegistry, "load_user_tools", lambda self, plugin_dir: None)

    manager = AgentManager(config=config, principal=alice)

    assert manager.memory_manager.principal is alice
    assert manager.memory_manager.principal_id == alice.principal_id
    assert manager.memory_manager.principal_kind == alice.kind


def test_agent_manager_binds_live_conversation_context_to_explicit_principal(monkeypatch):
    from zara.agent import AgentManager
    from zara.agent.tools.registry import ToolRegistry

    config = MinimalRuntimeConfig()
    alice = principal("alice")
    bob = principal("bob")

    monkeypatch.setattr(
        AgentManager,
        "_create_llm_client",
        lambda self, _llm_config: object(),
    )
    monkeypatch.setattr(ToolRegistry, "load_builtin_tools", lambda self, memory_manager=None: None)
    monkeypatch.setattr(ToolRegistry, "load_user_tools", lambda self, plugin_dir: None)

    alice_manager = AgentManager(config=config, principal=alice)
    bob_manager = AgentManager(config=config, principal=bob)

    assert alice_manager.conversation_manager.principal is alice
    assert bob_manager.conversation_manager.principal is bob
    assert alice_manager.conversation_manager is not bob_manager.conversation_manager


class OpenRouterRuntimeConfig(MinimalRuntimeConfig):
    def __init__(self, llm_config):
        self._llm_config = llm_config

    def get_llm_config(self):
        return dict(self._llm_config)


def _openrouter_manager(monkeypatch, llm_config):
    from zara.agent import AgentManager
    from zara.agent.tools.registry import ToolRegistry

    monkeypatch.setattr(ToolRegistry, "load_builtin_tools", lambda self, memory_manager=None: None)
    monkeypatch.setattr(ToolRegistry, "load_user_tools", lambda self, plugin_dir: None)

    return AgentManager(config=OpenRouterRuntimeConfig(llm_config))


def test_agent_factory_builds_openrouter_chat_model_with_defaults(monkeypatch):
    from langchain_openai import ChatOpenAI

    manager = _openrouter_manager(
        monkeypatch,
        {"provider": "openrouter", "openrouter_api_key": "config-key"},
    )

    assert isinstance(manager.llm_client, ChatOpenAI)
    assert manager.llm_client.model_name == "openrouter/free"
    assert manager.llm_client.openai_api_base == "https://openrouter.ai/api/v1"
    assert manager.llm_client.openai_api_key.get_secret_value() == "config-key"


def test_agent_factory_openrouter_resolves_key_from_environment(monkeypatch):
    monkeypatch.setenv("OPENROUTER_API_KEY", "env-key")

    manager = _openrouter_manager(monkeypatch, {"provider": "openrouter"})

    assert manager.llm_client.openai_api_key.get_secret_value() == "env-key"


def test_agent_factory_openrouter_honors_model_and_endpoint_overrides(monkeypatch):
    manager = _openrouter_manager(
        monkeypatch,
        {
            "provider": "openrouter",
            "openrouter_api_key": "config-key",
            "model": "z-ai/glm-4.5-air",
            "endpoint": "http://127.0.0.1:8787/openrouter/api/v1",
        },
    )

    assert manager.llm_client.model_name == "z-ai/glm-4.5-air"
    assert manager.llm_client.openai_api_base == "http://127.0.0.1:8787/openrouter/api/v1"
