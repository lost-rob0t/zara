import pytest

from zara.runtime.backend import LangGraphRuntimeBackend, RuntimeBackend, UnsupportedRuntimeCommand
from zara.runtime.host import RuntimeHost


class FakeProlog:
    def get_commerce_policy(self):
        return {
            "provider": "doordash",
            "confirmation": "always",
            "preference_learning": False,
            "preference_min_observations": 4,
            "preference_max_patterns": 7,
            "preference_min_confidence": 0.75,
        }


class FakeManager:
    def __init__(self):
        self.prolog_engine = FakeProlog()
        self.principal = type("Principal", (), {"principal_id": "owner"})()
        self.tool_registry = type(
            "Registry",
            (),
            {
                "register_tools": lambda self, tools: None,
                "unregister_tools": lambda self, names: None,
                "requires_approval": lambda self, name: False,
                "invoke_composed_tool": lambda self, name, request: {},
            },
        )()


@pytest.mark.asyncio
async def test_langgraph_backend_projects_prolog_commerce_policy_into_doordash_plugin():
    backend = LangGraphRuntimeBackend(FakeManager)
    await backend.start()

    projected = backend.plugin_configuration(
        "zara-doordash",
        {
            "learning_enabled": True,
            "preference_min_observations": 1,
            "preference_limit": 99,
            "max_url_chars": 4096,
        },
    )

    assert projected == {
        "learning_enabled": False,
        "preference_min_observations": 4,
        "preference_limit": 7,
        "preference_min_confidence": 0.75,
        "commerce_provider": "doordash",
        "commerce_confirmation": "always",
        "policy_source": "prolog",
        "max_url_chars": 4096,
    }


@pytest.mark.asyncio
async def test_non_doordash_plugin_config_is_not_rewritten():
    backend = LangGraphRuntimeBackend(FakeManager)
    await backend.start()

    assert backend.plugin_configuration("zara-browser", {"x": 1}) == {"x": 1}


def test_base_backend_does_not_invent_plugin_policy():
    with pytest.raises(UnsupportedRuntimeCommand):
        RuntimeBackend().plugin_configuration("zara-doordash", {})


@pytest.mark.asyncio
async def test_host_uses_backend_projected_plugin_configuration(monkeypatch):
    captured = {}

    class Config:
        def get_module_search_paths(self):
            return ()

        def get_plugin_runtime_config(self):
            return {
                "lifecycle_timeout": 1.0,
                "event_queue_size": 8,
                "max_managed_workers": 1,
            }

        def get_plugin_config(self, name):
            return {"from_toml": name}

    class Backend(RuntimeBackend):
        def plugin_configuration(self, name, base):
            assert name == "zara-doordash"
            return {**base, "policy_source": "prolog"}

        def register_tools(self, _tools):
            pass

        @property
        def principal_id(self):
            raise UnsupportedRuntimeCommand()

    class Manager:
        def __init__(self, _paths, **kwargs):
            captured["provider"] = kwargs["configuration_provider"]

        async def start(self):
            captured["value"] = captured["provider"]("zara-doordash")

        async def stop(self):
            pass

        def diagnostics(self):
            return ()

    monkeypatch.setattr("zara.runtime.host.PluginManager", Manager)
    host = RuntimeHost(lambda: Backend(), config=Config())
    host._backend = Backend()

    await host._start_plugins()

    assert captured["value"] == {
        "from_toml": "zara-doordash",
        "policy_source": "prolog",
    }
