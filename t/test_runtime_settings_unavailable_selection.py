from __future__ import annotations

import os

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtWidgets import QApplication

import zara.desktop.windows.settings as settings_window_module
from zara.config import DEFAULT_CONFIG_TOML, ZaraConfig
from zara.desktop.windows import SettingsWindow
from zara.runtime.discovery import builtin_runtime_descriptor
from zara.runtime.registry import (
    ControlOwner,
    RuntimeDescriptor,
    RuntimeHealth,
    RuntimeLocality,
    RuntimeTransport,
    ZARA_RUNTIME_PROTOCOL,
)


def app() -> QApplication:
    instance = QApplication.instance()
    assert instance is None or isinstance(instance, QApplication)
    return instance or QApplication([])


def unavailable_prolog_runtime() -> RuntimeDescriptor:
    return RuntimeDescriptor(
        id="prolog-rlm",
        display_name="Prolog-RLM",
        protocol=ZARA_RUNTIME_PROTOCOL,
        runtime_version="0.1.0-dev",
        implementation_version="0.1.0-dev",
        installed=True,
        available=False,
        health=RuntimeHealth.FAILED,
        locality=RuntimeLocality.LOCAL_SIDECAR,
        transport=RuntimeTransport.LOOPBACK_HTTP,
        capabilities=("direct", "rlm", "cancel"),
        profiles=(),
        provider_control=ControlOwner.RUNTIME,
        model_control=ControlOwner.RUNTIME,
        supports_streaming=False,
        supports_cancel=True,
        supports_context_handles=True,
        supports_host_tools=False,
        provenance="prolog-rlm:loopback",
    )


def test_unavailable_configured_runtime_is_preserved_until_explicit_reselection(
    tmp_path,
    monkeypatch,
):
    app()
    config_path = tmp_path / "xdg" / "config.toml"
    config_path.parent.mkdir()
    config_path.write_text(
        DEFAULT_CONFIG_TOML.replace(
            'backend = "zara-python"',
            'backend = "prolog-rlm"',
            1,
        ),
        encoding="utf-8",
    )
    config_path.with_name("config.pl").write_text(
        "% runtime settings test\n",
        encoding="utf-8",
    )
    root = tmp_path / "repo"
    (root / "kb").mkdir(parents=True)
    (root / "modules").mkdir()
    (root / "main.pl").write_text("main :- true.\n", encoding="utf-8")
    (root / "kb" / "intents.pl").write_text("intent(ok).\n", encoding="utf-8")
    (root / "modules" / "logic.pl").write_text("logic(ok).\n", encoding="utf-8")

    monkeypatch.setattr(
        settings_window_module,
        "discover_installed_runtimes",
        lambda _config: (
            builtin_runtime_descriptor(),
            unavailable_prolog_runtime(),
        ),
    )

    window = SettingsWindow(ZaraConfig(str(config_path)), repo_root=root)
    try:
        runtime = window.setting_widgets["runtime.backend"]
        configured_index = runtime.findData("prolog-rlm")
        assert configured_index >= 0
        assert runtime.currentData() == "prolog-rlm"
        assert runtime.model().item(configured_index).isEnabled() is False

        window.setting_widgets["llm.model"].setText("qwen3:14b")
        window.save_settings()

        persisted = config_path.read_text(encoding="utf-8")
        assert 'backend = "prolog-rlm"' in persisted
        assert 'model = "qwen3:14b"' in persisted
    finally:
        window.prepare_for_quit()
        window.close()
        window.deleteLater()
        app().processEvents()
