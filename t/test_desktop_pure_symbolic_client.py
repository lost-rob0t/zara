from __future__ import annotations

import pytest

from zara.desktop import app as desktop_app
from zara.runtime.pure_symbolic_backend import PureSymbolicRuntimeBackend


class PureSymbolicConfig:
    def get(self, section: str, key: str, default=None):
        if section == "agent" and key == "backend":
            return "pure_symbolic"
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

    client = desktop_app._default_desktop_client(config)

    assert client is expected_client
    assert seen == {
        "backend_factory": PureSymbolicRuntimeBackend,
        "config": config,
    }
