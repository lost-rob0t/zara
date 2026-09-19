from __future__ import annotations

import concurrent.futures
import textwrap

import pytest

from zara.plugins.api import PluginRuntime, RuntimeStatus
from zara.runtime.backend import RuntimeBackend
from zara.runtime.host import RuntimeHost, RuntimeHostState
from zara.runtime.symbols import ProgrammableSymbolRegistry, SymbolLookupError


def _completed_dispatch(_command):
    future = concurrent.futures.Future()
    future.set_result(None)
    return future


def _status():
    return RuntimeStatus(state="running", alive=True, thread_id=1)


def _subscriber(*, maxsize):
    raise AssertionError(f"unexpected subscription with maxsize={maxsize}")


def test_plugin_runtime_registers_owner_scoped_symbol_and_unloads_it():
    registry = ProgrammableSymbolRegistry()
    runtime = PluginRuntime(
        plugin_name="sample",
        configuration={},
        status_provider=_status,
        dispatcher=_completed_dispatch,
        subscriber=_subscriber,
        failure_callback=lambda _message: None,
        symbol_registrar=registry.register,
        symbol_unregistrar=registry.unregister,
    )

    registration_id = runtime.register_symbol(
        "zara:home/open",
        "command",
        "package-home",
        docs="Open the package-defined home.",
        capabilities=("ui.open",),
        source="sample/package.pl",
    )

    active = registry.resolve("zara:home/open")
    assert active.registration_id == registration_id
    assert active.owner == "plugin:sample"
    assert active.layer == "package"
    assert registry.describe("zara:home/open")[0].source == "sample/package.pl"

    runtime._shutdown()
    runtime._shutdown()

    with pytest.raises(SymbolLookupError):
        registry.resolve("zara:home/open")
    with pytest.raises(RuntimeError, match="closed"):
        runtime.register_symbol("zara:other", "command", "nope")


def test_plugin_runtime_rejects_symbols_when_host_has_no_symbol_registry():
    runtime = PluginRuntime(
        plugin_name="sample",
        configuration={},
        status_provider=_status,
        dispatcher=_completed_dispatch,
        subscriber=_subscriber,
        failure_callback=lambda _message: None,
    )

    with pytest.raises(RuntimeError, match="not available"):
        runtime.register_symbol("zara:home/open", "command", "package-home")


class _PluginConfig:
    def get_api_service_config(self):
        return {"enabled": False}

    def get_plugin_runtime_config(self):
        return {
            "lifecycle_timeout": 1.0,
            "event_queue_size": 4,
            "max_managed_workers": 2,
        }

    def get_plugin_config(self, _name):
        return {}


class _Backend(RuntimeBackend):
    pass


def _stop_host(host):
    if host.state not in {RuntimeHostState.NEW, RuntimeHostState.STOPPED}:
        host.shutdown("test cleanup").result(timeout=5)
    host.join(timeout=5)


def test_runtime_host_exposes_plugin_symbol_and_restores_namespace_on_shutdown(tmp_path):
    plugin_path = tmp_path / "symbol_plugin.py"
    plugin_path.write_text(
        textwrap.dedent(
            """
            from zara.plugins import PluginMetadata, ServicePlugin

            class TestPlugin(ServicePlugin):
                metadata = PluginMetadata(name="symbol-test")

                def start(self, runtime):
                    runtime.register_symbol(
                        "zara:home/open",
                        "command",
                        "org-home",
                        docs="Open the Org package home.",
                        capabilities=("ui.open",),
                        source="symbol_plugin.py",
                    )

                def stop(self):
                    pass

            def create_plugin():
                return TestPlugin()
            """
        )
    )

    host = RuntimeHost(
        lambda: _Backend(),
        plugin_paths=(tmp_path,),
        config=_PluginConfig(),
    )

    try:
        host.start().result(timeout=5)
        active = host.resolve_symbol("zara:home/open")
        assert active.value == "org-home"
        assert active.owner == "plugin:symbol-test"

        diagnostics = host.symbol_diagnostics("zara:home/open")
        assert len(diagnostics) == 1
        assert diagnostics[0].active is True
        assert diagnostics[0].owner == "plugin:symbol-test"
        assert diagnostics[0].capabilities == ("ui.open",)
        assert host.programmable_symbols(kind="command") == ("zara:home/open",)
    finally:
        _stop_host(host)

    with pytest.raises(SymbolLookupError):
        host.resolve_symbol("zara:home/open")


def test_plugin_unload_restores_lower_definition_without_package_cooperation():
    registry = ProgrammableSymbolRegistry()
    registry.register(
        symbol="org:daily/open",
        kind="command",
        owner="core:daily",
        layer="core",
        value="core-daily",
    )

    runtime = PluginRuntime(
        plugin_name="logseq-daily",
        configuration={},
        status_provider=_status,
        dispatcher=_completed_dispatch,
        subscriber=_subscriber,
        failure_callback=lambda _message: None,
        symbol_registrar=registry.register,
        symbol_unregistrar=registry.unregister,
    )
    runtime.register_symbol("org:daily/open", "command", "continuous-daily")

    assert registry.get("org:daily/open") == "continuous-daily"
    runtime._shutdown()
    assert registry.get("org:daily/open") == "core-daily"
