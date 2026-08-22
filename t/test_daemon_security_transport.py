from __future__ import annotations

import pytest
import zmq
from zmq.utils import z85

from zara.security import Capability, KeyNotActive, SecurityRegistry
from zara.security_transport import (
    AuthenticationRequired,
    CurveClientConfig,
    CurveServerConfig,
    RegistryAuthenticator,
    RegistryCredentialsProvider,
    authenticated_user_id,
    configure_curve_client_socket,
    configure_curve_server_socket,
)
from zara.server import PrincipalContext


@pytest.fixture
def zmq_context():
    context = zmq.Context()
    try:
        yield context
    finally:
        context.term()


def keypair() -> tuple[str, str]:
    public, secret = zmq.curve_keypair()
    return public.decode("ascii"), secret.decode("ascii")


def test_registry_credentials_provider_accepts_only_live_enrollment():
    public, _ = keypair()
    registry = SecurityRegistry()
    enrolled = registry.enroll(
        public,
        principal=PrincipalContext("user:alice", kind="authenticated"),
        device_id="alice-phone",
        capabilities={Capability.SESSION_BASIC},
    )
    provider = RegistryCredentialsProvider(registry)
    raw_public = z85.decode(public.encode("ascii"))

    assert provider.callback("zara", raw_public) is True
    assert provider.user_id(raw_public) == enrolled.user_id

    registry.revoke("alice-phone")
    assert provider.callback("zara", raw_public) is False
    with pytest.raises(KeyNotActive):
        provider.user_id(raw_public)


def test_registry_credentials_provider_denies_unknown_key_without_oracle_details():
    public, _ = keypair()
    provider = RegistryCredentialsProvider(SecurityRegistry())
    raw_public = z85.decode(public.encode("ascii"))

    assert provider.callback("zara", raw_public) is False
    with pytest.raises(KeyNotActive):
        provider.user_id(raw_public)


def test_registry_authenticator_maps_curve_key_to_server_selected_user_id(zmq_context):
    public, _ = keypair()
    registry = SecurityRegistry()
    enrolled = registry.enroll(
        public,
        principal=PrincipalContext("user:alice", kind="authenticated"),
        device_id="alice-phone",
    )
    authenticator = RegistryAuthenticator(zmq_context, registry=registry, domain="zara")
    raw_public = z85.decode(public.encode("ascii"))

    assert authenticator.curve_user_id(raw_public) == enrolled.user_id


def test_curve_server_socket_configuration_is_explicit_and_pins_zap_domain(zmq_context):
    server_public, server_secret = keypair()
    config = CurveServerConfig(
        public_key=server_public,
        secret_key=server_secret,
        zap_domain="zara",
    )
    socket = zmq_context.socket(zmq.ROUTER)
    try:
        configure_curve_server_socket(socket, config)

        assert socket.getsockopt(zmq.CURVE_SERVER) == 1
        assert socket.getsockopt(zmq.CURVE_PUBLICKEY) == z85.decode(server_public.encode("ascii"))
        assert socket.getsockopt(zmq.CURVE_SECRETKEY) == z85.decode(server_secret.encode("ascii"))
        assert socket.getsockopt_string(zmq.ZAP_DOMAIN) == "zara"
    finally:
        socket.close(0)


def test_curve_client_socket_configuration_pins_server_and_client_keys(zmq_context):
    server_public, _ = keypair()
    client_public, client_secret = keypair()
    config = CurveClientConfig(
        public_key=client_public,
        secret_key=client_secret,
        server_public_key=server_public,
    )
    socket = zmq_context.socket(zmq.DEALER)
    try:
        configure_curve_client_socket(socket, config)

        assert socket.getsockopt(zmq.CURVE_PUBLICKEY) == z85.decode(client_public.encode("ascii"))
        assert socket.getsockopt(zmq.CURVE_SECRETKEY) == z85.decode(client_secret.encode("ascii"))
        assert socket.getsockopt(zmq.CURVE_SERVERKEY) == z85.decode(server_public.encode("ascii"))
    finally:
        socket.close(0)


class FakeAuthenticatedFrame:
    def __init__(self, user_id=None):
        self.user_id = user_id

    def __getitem__(self, key):
        if key != "User-Id" or self.user_id is None:
            raise KeyError(key)
        return self.user_id


def test_authenticated_user_id_reads_zap_metadata_not_payload_identity():
    frames = [FakeAuthenticatedFrame("zara:alice-device:1:opaque"), FakeAuthenticatedFrame()]
    assert authenticated_user_id(frames) == "zara:alice-device:1:opaque"


def test_authenticated_user_id_fails_closed_when_metadata_is_missing_or_empty():
    with pytest.raises(AuthenticationRequired):
        authenticated_user_id([FakeAuthenticatedFrame()])
    with pytest.raises(AuthenticationRequired):
        authenticated_user_id([FakeAuthenticatedFrame("")])
    with pytest.raises(AuthenticationRequired):
        authenticated_user_id([])


def test_curve_configs_reject_malformed_keys_and_empty_domain():
    public, secret = keypair()
    with pytest.raises(ValueError):
        CurveServerConfig(public_key="bad", secret_key=secret)
    with pytest.raises(ValueError):
        CurveServerConfig(public_key=public, secret_key="bad")
    with pytest.raises(ValueError):
        CurveServerConfig(public_key=public, secret_key=secret, zap_domain="")
    with pytest.raises(ValueError):
        CurveClientConfig(public_key=public, secret_key=secret, server_public_key="bad")
