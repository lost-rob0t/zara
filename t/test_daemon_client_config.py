from __future__ import annotations

from types import SimpleNamespace

import pytest

from zara import daemon_client


class FakeConfig:
    def __init__(self, daemon: dict | None = None) -> None:
        self.daemon = daemon or {}

    def get_section(self, name: str):
        if name == "daemon":
            return dict(self.daemon)
        return {}


def clear_daemon_env(monkeypatch) -> None:
    for name in (
        daemon_client.DAEMON_ENDPOINT_ENV,
        daemon_client.CURVE_PUBLIC_KEY_ENV,
        daemon_client.CURVE_SECRET_KEY_ENV,
        daemon_client.CURVE_SERVER_PUBLIC_KEY_ENV,
    ):
        monkeypatch.delenv(name, raising=False)


def test_configured_daemon_endpoint_is_default(monkeypatch):
    clear_daemon_env(monkeypatch)
    config = FakeConfig({"endpoint": " tcp://127.0.0.1:7731 "})

    assert daemon_client.resolve_daemon_endpoint(config) == "tcp://127.0.0.1:7731"


def test_environment_endpoint_overrides_mutable_config(monkeypatch):
    clear_daemon_env(monkeypatch)
    monkeypatch.setenv(daemon_client.DAEMON_ENDPOINT_ENV, "tcp://10.0.0.8:7731")
    config = FakeConfig({"endpoint": "tcp://127.0.0.1:7731"})

    assert daemon_client.resolve_daemon_endpoint(config) == "tcp://10.0.0.8:7731"


def test_explicit_endpoint_wins_over_environment(monkeypatch):
    clear_daemon_env(monkeypatch)
    monkeypatch.setenv(daemon_client.DAEMON_ENDPOINT_ENV, "tcp://10.0.0.8:7731")

    assert (
        daemon_client.resolve_daemon_endpoint(
            FakeConfig({"endpoint": "tcp://127.0.0.1:7731"}),
            explicit="ipc:///tmp/zara-explicit.sock",
        )
        == "ipc:///tmp/zara-explicit.sock"
    )


def test_empty_configuration_retains_private_ipc_fallback(monkeypatch):
    clear_daemon_env(monkeypatch)
    runtime_dir = object()
    monkeypatch.setattr(
        daemon_client,
        "ServerLease",
        lambda: SimpleNamespace(_runtime_dir=lambda: runtime_dir),
    )
    monkeypatch.setattr(
        daemon_client,
        "default_zmq_endpoint",
        lambda value: "ipc:///private/zara.sock" if value is runtime_dir else "wrong",
    )

    assert daemon_client.resolve_daemon_endpoint(FakeConfig()) == "ipc:///private/zara.sock"


def test_partial_curve_configuration_fails_closed(monkeypatch):
    clear_daemon_env(monkeypatch)
    config = FakeConfig({"curve_secret_key": "secret-only"})

    with pytest.raises(ValueError, match="requires public, secret, and server public keys"):
        daemon_client.curve_client_config(config)


def test_create_daemon_client_uses_resolved_endpoint_and_curve(monkeypatch):
    clear_daemon_env(monkeypatch)
    expected_curve = object()
    seen = {}

    monkeypatch.setattr(
        daemon_client,
        "resolve_daemon_endpoint",
        lambda config=None, *, explicit=None: explicit or "tcp://127.0.0.1:7731",
    )
    monkeypatch.setattr(
        daemon_client,
        "curve_client_config",
        lambda config=None: expected_curve,
    )

    class FakeClient:
        def __init__(self, endpoint, **kwargs):
            seen["endpoint"] = endpoint
            seen.update(kwargs)

    import zara.zmq_transport as transport_module

    monkeypatch.setattr(transport_module, "ZmqZaraClient", FakeClient)

    client = daemon_client.create_daemon_client(
        "tcp://127.0.0.1:7731",
        config=FakeConfig(),
        voice_output="speaker",
    )

    assert isinstance(client, FakeClient)
    assert seen == {
        "endpoint": "tcp://127.0.0.1:7731",
        "curve_client": expected_curve,
        "voice_output": "speaker",
    }


def test_create_daemon_client_omits_curve_kwarg_when_unconfigured(monkeypatch):
    clear_daemon_env(monkeypatch)
    seen = []

    class LegacyStyleClient:
        def __init__(self, endpoint):
            seen.append(endpoint)

    import zara.zmq_transport as transport_module

    monkeypatch.setattr(transport_module, "ZmqZaraClient", LegacyStyleClient)

    client = daemon_client.create_daemon_client(
        "ipc:///tmp/zara.sock",
        config=FakeConfig(),
    )

    assert isinstance(client, LegacyStyleClient)
    assert seen == ["ipc:///tmp/zara.sock"]
