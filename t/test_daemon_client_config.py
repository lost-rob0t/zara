from __future__ import annotations

from types import SimpleNamespace

import pytest
import zmq

from zara import daemon_client


class FakeConfig:
    def __init__(self, daemon: dict | None = None) -> None:
        self.daemon = daemon or {}

    def get_section(self, name: str):
        if name == "daemon":
            return dict(self.daemon)
        return {}


class RawConfig:
    def __init__(self, daemon) -> None:
        self.daemon = daemon

    def get_section(self, name: str):
        return self.daemon if name == "daemon" else {}


def clear_daemon_env(monkeypatch) -> None:
    for name in (
        daemon_client.DAEMON_ENDPOINT_ENV,
        daemon_client.CURVE_PUBLIC_KEY_ENV,
        daemon_client.CURVE_SECRET_KEY_ENV,
        daemon_client.CURVE_SERVER_PUBLIC_KEY_ENV,
    ):
        monkeypatch.delenv(name, raising=False)


def curve_keypair() -> tuple[str, str]:
    public_key, secret_key = zmq.curve_keypair()
    return public_key.decode("ascii"), secret_key.decode("ascii")


def test_configured_daemon_endpoint_is_default(monkeypatch):
    clear_daemon_env(monkeypatch)
    config = FakeConfig({"endpoint": " tcp://127.0.0.1:7731 "})

    assert daemon_client.resolve_daemon_endpoint(config) == "tcp://127.0.0.1:7731"


def test_global_config_is_used_when_config_is_not_supplied(monkeypatch):
    clear_daemon_env(monkeypatch)
    config = FakeConfig({"endpoint": "tcp://127.0.0.1:8842"})
    monkeypatch.setattr(daemon_client, "get_config", lambda: config)

    assert daemon_client.resolve_daemon_endpoint() == "tcp://127.0.0.1:8842"


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


@pytest.mark.parametrize("explicit", ["", " ", "\t\n"])
def test_explicit_endpoint_rejects_blank_values(monkeypatch, explicit):
    clear_daemon_env(monkeypatch)

    with pytest.raises(ValueError, match="explicit daemon endpoint must not be empty"):
        daemon_client.resolve_daemon_endpoint(FakeConfig(), explicit=explicit)


def test_daemon_configuration_rejects_non_table(monkeypatch):
    clear_daemon_env(monkeypatch)

    with pytest.raises(ValueError, match="daemon configuration must be a table"):
        daemon_client.resolve_daemon_endpoint(RawConfig(["not", "a", "table"]))


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


def test_unconfigured_curve_auth_returns_none(monkeypatch):
    clear_daemon_env(monkeypatch)

    assert daemon_client.curve_client_config(FakeConfig()) is None


def test_complete_curve_configuration_is_trimmed(monkeypatch):
    clear_daemon_env(monkeypatch)
    client_public, client_secret = curve_keypair()
    server_public, _ = curve_keypair()
    config = FakeConfig(
        {
            "curve_public_key": f" {client_public} ",
            "curve_secret_key": f" {client_secret} ",
            "curve_server_public_key": f" {server_public} ",
        }
    )

    curve = daemon_client.curve_client_config(config)

    assert curve is not None
    assert curve.public_key == client_public
    assert curve.secret_key == client_secret
    assert curve.server_public_key == server_public


@pytest.mark.parametrize("field", ["public", "secret", "server"])
def test_curve_environment_values_override_config_per_field(monkeypatch, field):
    clear_daemon_env(monkeypatch)
    client_public, client_secret = curve_keypair()
    alternate_public, alternate_secret = curve_keypair()
    server_public, _ = curve_keypair()
    alternate_server_public, _ = curve_keypair()

    config_values = {
        "curve_public_key": client_public,
        "curve_secret_key": client_secret,
        "curve_server_public_key": server_public,
    }
    expected = (client_public, client_secret, server_public)

    if field == "public":
        config_values["curve_public_key"] = alternate_public
        monkeypatch.setenv(daemon_client.CURVE_PUBLIC_KEY_ENV, client_public)
    elif field == "secret":
        config_values["curve_secret_key"] = alternate_secret
        monkeypatch.setenv(daemon_client.CURVE_SECRET_KEY_ENV, client_secret)
    else:
        monkeypatch.setenv(
            daemon_client.CURVE_SERVER_PUBLIC_KEY_ENV,
            alternate_server_public,
        )
        expected = (client_public, client_secret, alternate_server_public)

    curve = daemon_client.curve_client_config(FakeConfig(config_values))

    assert curve is not None
    assert (curve.public_key, curve.secret_key, curve.server_public_key) == expected


def test_partial_curve_configuration_fails_closed(monkeypatch):
    clear_daemon_env(monkeypatch)
    config = FakeConfig({"curve_secret_key": "secret-only"})

    with pytest.raises(ValueError, match="requires public, secret, and server public keys"):
        daemon_client.curve_client_config(config)


def test_create_daemon_client_rejects_caller_owned_curve_config(monkeypatch):
    clear_daemon_env(monkeypatch)

    with pytest.raises(TypeError, match="curve_client is owned"):
        daemon_client.create_daemon_client(
            "ipc:///tmp/zara.sock",
            config=FakeConfig(),
            curve_client=object(),
        )


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
