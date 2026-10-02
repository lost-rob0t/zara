from __future__ import annotations

import asyncio
import importlib
import importlib.util
import json
import socket
import tomllib

import pytest

from zara.config import DEFAULT_CONFIG_TOML
from zara.plugins import PluginRuntime, RuntimeStatus


def _module():
    return importlib.import_module("zara.plugins.builtin.beta_console")


def _runtime(configuration: dict, failures: list[str]) -> PluginRuntime:
    return PluginRuntime(
        plugin_name="beta-console",
        configuration=configuration,
        status_provider=lambda: RuntimeStatus("running", True, None),
        dispatcher=lambda _command: None,
        subscriber=lambda **_kwargs: None,
        failure_callback=failures.append,
        max_workers=2,
        worker_join_timeout=2.0,
    )


def _unused_port() -> int:
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as listener:
        listener.bind(("127.0.0.1", 0))
        return int(listener.getsockname()[1])


def test_beta_console_module_is_first_party():
    assert importlib.util.find_spec("zara.plugins.builtin.beta_console") is not None


def test_beta_console_is_enabled_only_by_new_default_config():
    module = _module()
    plugin = module.BetaConsolePlugin()
    config = tomllib.loads(DEFAULT_CONFIG_TOML)

    assert plugin.enabled_by_default is False
    assert plugin.metadata.name == "beta-console"
    assert plugin.metadata.plugin_type == "service"
    assert config["plugins"]["beta-console"] == {
        "enabled": True,
        "host": "127.0.0.1",
        "port": 8787,
        "theme": "inherit",
    }


def test_beta_console_refuses_remote_bind_without_explicit_opt_in():
    module = _module()

    assert module.require_safe_bind("127.0.0.1", allow_remote=False) == "127.0.0.1"
    assert module.require_safe_bind("::1", allow_remote=False) == "::1"
    with pytest.raises(ValueError, match="allow_remote"):
        module.require_safe_bind("0.0.0.0", allow_remote=False)
    assert module.require_safe_bind("0.0.0.0", allow_remote=True) == "0.0.0.0"


def test_beta_console_public_theme_and_html_contract_use_shared_semantic_registry():
    module = _module()

    payload = module.build_theme_payload("dotfiles-outrun")
    html = module.render_beta_console(
        theme_key="dracula",
        runtime_state="running",
        runtime_alive=True,
        version="0.3.x",
    )

    assert payload["selected"] == "dotfiles-outrun"
    assert payload["theme"]["colors"]["ground"] == "#170C32"
    assert payload["theme"]["colors"]["primary"] == "#2DE2E6"
    assert payload["theme"]["css_variables"]["--zara-primary"] == "#2DE2E6"
    assert "signal-cabin" in payload["available"]
    assert "dotfiles-outrun" in payload["available"]
    assert "--zara-ground:#282A36" in html
    assert "--zara-primary:#50FA7B" in html
    for label in ("System overview", "Test missions", "Diagnostics", "Beta builds", "Issues"):
        assert label in html


def test_beta_console_http_responses_are_read_only_no_store_and_typed():
    module = _module()
    plugin = module.BetaConsolePlugin()
    failures: list[str] = []
    runtime = _runtime({"theme": "dotfiles-outrun"}, failures)
    plugin._runtime = runtime
    plugin._theme_key = "dotfiles-outrun"

    status = asyncio.run(plugin._http_status(None))
    theme = asyncio.run(plugin._http_theme(None))

    assert status.status == 200
    assert status.content_type == "application/json"
    assert status.headers["Cache-Control"] == "no-store"
    assert status.headers["X-Content-Type-Options"] == "nosniff"
    status_payload = json.loads(status.text)
    assert status_payload["plugin"] == "beta-console"
    assert status_payload["runtime"] == {"state": "running", "alive": True}
    assert status_payload["theme"] == "dotfiles-outrun"

    assert theme.status == 200
    assert theme.content_type == "application/json"
    assert theme.headers["Cache-Control"] == "no-store"
    assert json.loads(theme.text)["selected"] == "dotfiles-outrun"
    runtime._shutdown()


def test_beta_console_bind_conflict_is_classified_and_cleanup_releases_port():
    module = _module()
    port = _unused_port()
    failures_a: list[str] = []
    failures_b: list[str] = []
    runtime_a = _runtime({"host": "127.0.0.1", "port": port, "theme": "inherit"}, failures_a)
    runtime_b = _runtime({"host": "127.0.0.1", "port": port, "theme": "inherit"}, failures_b)
    plugin_a = module.BetaConsolePlugin()
    plugin_b = module.BetaConsolePlugin()

    try:
        plugin_a.start(runtime_a)
        with pytest.raises(RuntimeError, match="beta console failed to start"):
            plugin_b.start(runtime_b)
    finally:
        plugin_b.stop()
        runtime_b._shutdown()
        plugin_a.stop()
        runtime_a._shutdown()

    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as listener:
        listener.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        listener.bind(("127.0.0.1", port))
