from __future__ import annotations

import zmq
from zmq.auth.thread import ThreadAuthenticator
from zmq.utils import z85

from zara.security import (
    Capability,
    CurveClientConfig,
    CurveCredentialsProvider,
    CurveServerConfig,
    KeyRecord,
    KeyRegistry,
    SecurityConfigurationError,
    apply_curve_client,
    apply_curve_server,
)
from zara.server import PrincipalContext


def _pair():
    public, secret = zmq.curve_keypair()
    return public.decode("ascii"), secret.decode("ascii")


def _registry_with(public_key: str) -> KeyRegistry:
    registry = KeyRegistry()
    registry.enroll(
        KeyRecord(
            public_key=public_key,
            principal=PrincipalContext("alice", kind="authenticated"),
            device_id="alice-laptop",
            capabilities=frozenset({Capability.CONVERSATION, Capability.STATUS}),
        )
    )
    return registry


def _curve_roundtrip(*, wrong_server_pin: bool) -> bool:
    client_public, client_secret = _pair()
    server_public, server_secret = _pair()
    pinned_public = _pair()[0] if wrong_server_pin else server_public
    registry = _registry_with(client_public)
    server_config = CurveServerConfig(
        public_key=server_public,
        secret_key=server_secret,
        registry=registry,
        zap_domain="zara-test",
    )
    client_config = CurveClientConfig(
        public_key=client_public,
        secret_key=client_secret,
        server_public_key=pinned_public,
    )

    context = zmq.Context()
    authenticator = ThreadAuthenticator(context)
    server = context.socket(zmq.ROUTER)
    client = context.socket(zmq.DEALER)
    try:
        authenticator.start()
        authenticator.configure_curve_callback(
            domain=server_config.zap_domain,
            credentials_provider=CurveCredentialsProvider(registry),
        )
        apply_curve_server(server, server_config)
        apply_curve_client(client, client_config)
        server.bind("tcp://127.0.0.1:0")
        endpoint = server.getsockopt(zmq.LAST_ENDPOINT).decode("ascii")
        client.connect(endpoint)
        client.send(b"probe")

        if not server.poll(timeout=500, flags=zmq.POLLIN):
            return False
        frames = server.recv_multipart()
        return frames[-1] == b"probe"
    finally:
        client.close(0)
        server.close(0)
        authenticator.stop()
        context.term()


def test_curve_server_config_requires_matching_keypair():
    public, secret = _pair()
    other_public, _ = _pair()
    registry = _registry_with(public)

    config = CurveServerConfig(
        public_key=public,
        secret_key=secret,
        registry=registry,
    )
    assert config.public_key == public

    try:
        CurveServerConfig(
            public_key=other_public,
            secret_key=secret,
            registry=registry,
        )
    except SecurityConfigurationError:
        pass
    else:
        raise AssertionError("mismatched server CURVE keypair was accepted")


def test_curve_client_config_requires_matching_keypair_and_server_pin():
    public, secret = _pair()
    server_public, _ = _pair()
    other_public, _ = _pair()

    config = CurveClientConfig(
        public_key=public,
        secret_key=secret,
        server_public_key=server_public,
    )
    assert config.server_public_key == server_public

    try:
        CurveClientConfig(
            public_key=other_public,
            secret_key=secret,
            server_public_key=server_public,
        )
    except SecurityConfigurationError:
        pass
    else:
        raise AssertionError("mismatched client CURVE keypair was accepted")


def test_curve_config_repr_never_contains_secret_key():
    client_public, client_secret = _pair()
    server_public, server_secret = _pair()
    registry = _registry_with(client_public)

    server = CurveServerConfig(
        public_key=server_public,
        secret_key=server_secret,
        registry=registry,
    )
    client = CurveClientConfig(
        public_key=client_public,
        secret_key=client_secret,
        server_public_key=server_public,
    )

    assert server_secret not in repr(server)
    assert client_secret not in repr(client)


def test_credentials_provider_accepts_only_enabled_enrolled_raw_key():
    public, _ = _pair()
    unknown_public, _ = _pair()
    registry = _registry_with(public)
    provider = CurveCredentialsProvider(registry)

    raw_public = z85.decode(public.encode("ascii"))
    raw_unknown = z85.decode(unknown_public.encode("ascii"))

    assert provider.callback("zara", raw_public) is True
    assert provider.callback("zara", raw_unknown) is False

    registry.revoke(public)
    assert provider.callback("zara", raw_public) is False


def test_credentials_provider_rejects_malformed_raw_key_without_throwing():
    public, _ = _pair()
    provider = CurveCredentialsProvider(_registry_with(public))

    assert provider.callback("zara", b"") is False
    assert provider.callback("zara", b"short") is False
    assert provider.callback("zara", b"x" * 31) is False
    assert provider.callback("zara", b"x" * 33) is False


def test_apply_curve_server_sets_curve_mechanism_and_zap_domain():
    client_public, _ = _pair()
    server_public, server_secret = _pair()
    config = CurveServerConfig(
        public_key=server_public,
        secret_key=server_secret,
        registry=_registry_with(client_public),
        zap_domain="zara-test",
    )
    context = zmq.Context()
    socket = context.socket(zmq.ROUTER)
    try:
        apply_curve_server(socket, config)
        assert socket.getsockopt(zmq.MECHANISM) == zmq.CURVE
        assert socket.getsockopt(zmq.CURVE_SERVER) == 1
        assert socket.getsockopt(zmq.ZAP_DOMAIN) == b"zara-test"
    finally:
        socket.close(0)
        context.term()


def test_apply_curve_client_sets_curve_mechanism():
    client_public, client_secret = _pair()
    server_public, _ = _pair()
    config = CurveClientConfig(
        public_key=client_public,
        secret_key=client_secret,
        server_public_key=server_public,
    )
    context = zmq.Context()
    socket = context.socket(zmq.DEALER)
    try:
        apply_curve_client(socket, config)
        assert socket.getsockopt(zmq.MECHANISM) == zmq.CURVE
        assert socket.getsockopt(zmq.CURVE_SERVER) == 0
    finally:
        socket.close(0)
        context.term()


def test_correct_server_pin_completes_authenticated_curve_connection():
    assert _curve_roundtrip(wrong_server_pin=False) is True


def test_wrong_server_pin_cannot_complete_authenticated_curve_connection():
    assert _curve_roundtrip(wrong_server_pin=True) is False
