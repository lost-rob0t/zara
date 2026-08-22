from __future__ import annotations

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

    def __init__(self, *, config, principal):
        self.config = config
        self.principal = principal
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
