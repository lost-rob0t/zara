from __future__ import annotations

import pytest

from zara.desktop import app as desktop_app
from zara.desktop.conversation.symbolic_runtime import PureSymbolicProjectionAdapter
from zara.runtime.pure_symbolic_backend import PureSymbolicRuntimeBackend


class PureSymbolicConfig:
    def get(self, section: str, key: str, default=None):
        if section == "conversation" and key == "execution_policy":
            return "pure_symbolic"
        if section == "agent" and key == "backend":
            return "langgraph"
        return default


def test_default_desktop_client_selects_in_process_pure_symbolic_backend(monkeypatch):
    config = PureSymbolicConfig()
    seen = {}
    expected_client = object()

    def fake_in_process_client(*, backend_factory=None, config=None):
        seen["backend_factory"] = backend_factory
        seen["config"] = config
        return expected_client

    monkeypatch.setattr(desktop_app, "InProcessZaraClient", fake_in_process_client)
    monkeypatch.setattr(
        desktop_app,
        "create_daemon_client",
        lambda *_args, **_kwargs: pytest.fail(
            "pure symbolic Desktop must not initialize the daemon/provider path"
        ),
    )

    expected_store = object()
    client = desktop_app._default_desktop_client(
        config,
        conversation_store=expected_store,
    )

    assert client is expected_client
    assert seen["config"] is config
    backend = seen["backend_factory"]()
    assert isinstance(backend, PureSymbolicRuntimeBackend)
    assert isinstance(backend._projection_adapter, PureSymbolicProjectionAdapter)
    assert backend._projection_adapter.store is expected_store
    assert config.get("agent", "backend", "missing") == "langgraph"


def test_create_application_shares_one_canonical_store_with_symbolic_runtime_and_ui(monkeypatch):
    """Pure-symbolic runtime and UI must use the exact same history/projection owner."""

    class FakeApplication:
        _instance = None

        @classmethod
        def instance(cls):
            return cls._instance

        def __init__(self, _argv):
            type(self)._instance = self

        def setApplicationName(self, _name):
            pass

        def setOrganizationName(self, _name):
            pass

        def setQuitOnLastWindowClosed(self, _enabled):
            pass

    class FakeConversationService:
        def __init__(self, store):
            self.store = store

    expected_store = object()
    expected_client = object()
    seen = {"store_factory_calls": 0}

    def store_factory():
        seen["store_factory_calls"] += 1
        return expected_store

    def fake_default_client(config=None, *, conversation_store=None):
        seen["client_config"] = config
        seen["client_store"] = conversation_store
        return expected_client

    def fake_bridge(service, parent=None):
        seen["bridge_service"] = service
        seen["bridge_parent"] = parent
        return object()

    class FakeController:
        def __init__(
            self,
            app,
            service,
            bridge,
            *,
            conversation_service=None,
            **_kwargs,
        ):
            seen["controller_app"] = app
            seen["controller_service"] = service
            seen["controller_bridge"] = bridge
            seen["conversation_service"] = conversation_service

    monkeypatch.setattr(desktop_app, "QApplication", FakeApplication)
    monkeypatch.setattr(desktop_app, "ConversationStore", store_factory)
    monkeypatch.setattr(
        desktop_app,
        "ConversationService",
        FakeConversationService,
        raising=False,
    )
    monkeypatch.setattr(desktop_app, "_default_desktop_client", fake_default_client)
    monkeypatch.setattr(desktop_app, "QtRuntimeBridge", fake_bridge)
    monkeypatch.setattr(desktop_app, "DesktopController", FakeController)
    monkeypatch.setattr(desktop_app, "apply_desktop_theme", lambda *_args, **_kwargs: None)

    app, _controller = desktop_app.create_application(
        ["zara-desktop"],
        config=PureSymbolicConfig(),
    )

    assert seen["store_factory_calls"] == 1
    assert seen["client_store"] is expected_store
    assert seen["conversation_service"].store is expected_store
    assert seen["bridge_service"] is expected_client
    assert seen["bridge_parent"] is app
