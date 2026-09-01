from __future__ import annotations

import pytest

from zara.agent import AgentManager
from zara.agent.tools.registry import ToolRegistry
from zara.runtime.backend import LangGraphRuntimeBackend


class ToolConfig:
    def get_section(self, _name):
        return {}

    def get_tool_config(self):
        return {"query_prolog": True}


class FakeProlog:
    def __init__(self):
        self.queries = []

    def query_once(self, query):
        self.queries.append(query)
        return {"ok": True}


def test_tool_registry_can_attach_prolog_after_manager_construction():
    registry = ToolRegistry(None, ToolConfig())
    engine = FakeProlog()

    registry.attach_prolog_engine(engine)

    tool = registry.get_tool("query_prolog")
    assert registry.prolog_engine is engine
    assert tool is not None
    assert "Success" in tool.invoke({"query": "true"})
    assert engine.queries == ["true"]


def test_agent_manager_ensures_default_prolog_engine(monkeypatch, tmp_path):
    main_file = tmp_path / "main.pl"
    main_file.write_text("true.\n")
    engine = object()
    attached = []

    class Registry:
        def attach_prolog_engine(self, candidate):
            attached.append(candidate)

    manager = object.__new__(AgentManager)
    manager.config = ToolConfig()
    manager.prolog_engine = None
    manager.tool_registry = Registry()

    import zara.console as console_module
    import zara.prolog_engine as prolog_module

    monkeypatch.setattr(console_module, "find_main_pl", lambda: main_file)
    monkeypatch.setattr(
        prolog_module,
        "PrologEngine",
        lambda path: engine if path == main_file else None,
    )

    assert manager.ensure_prolog_engine() is engine
    assert manager.prolog_engine is engine
    assert attached == [engine]


def test_agent_manager_skips_default_prolog_when_tool_disabled(monkeypatch):
    class DisabledConfig(ToolConfig):
        def get_tool_config(self):
            return {"query_prolog": False}

    manager = object.__new__(AgentManager)
    manager.config = DisabledConfig()
    manager.prolog_engine = None
    manager.tool_registry = object()

    import zara.console as console_module

    monkeypatch.setattr(
        console_module,
        "find_main_pl",
        lambda: pytest.fail("disabled Prolog must not probe main.pl"),
    )

    assert manager.ensure_prolog_engine() is None
    assert manager.prolog_engine is None


def test_inprocess_client_default_backend_receives_same_config_and_plugins(monkeypatch):
    import zara.client as client_module
    import zara.runtime.backend as backend_module

    config = object()
    plugin_paths = ("/plugins/one", "/plugins/two")
    seen = {}

    class Config:
        def get_module_search_paths(self):
            return plugin_paths

    config = Config()

    class Backend:
        pass

    def agent_backend(*, config=None):
        seen["backend_config"] = config
        return Backend()

    class Host:
        def __init__(self, **kwargs):
            seen["host_kwargs"] = kwargs

    monkeypatch.setattr(backend_module, "AgentRuntimeBackend", agent_backend)
    monkeypatch.setattr(client_module, "RuntimeHost", Host)

    client_module.InProcessZaraClient(config=config)

    host_kwargs = seen["host_kwargs"]
    assert host_kwargs["config"] is config
    assert host_kwargs["plugin_paths"] == plugin_paths
    assert isinstance(host_kwargs["backend_factory"](), Backend)
    assert seen["backend_config"] is config


@pytest.mark.asyncio
async def test_langgraph_backend_ensures_runtime_services_before_use():
    calls = []

    class Manager:
        def ensure_prolog_engine(self):
            calls.append("ensure-prolog")

        def bind_event_publisher(self, _publisher):
            calls.append("bind-publisher")

        async def shutdown_async(self):
            calls.append("shutdown")

    manager = Manager()
    backend = LangGraphRuntimeBackend(lambda: manager)
    backend.bind_event_publisher(lambda _event: None)

    await backend.start()
    await backend.stop()

    assert calls == ["ensure-prolog", "bind-publisher", "shutdown"]
