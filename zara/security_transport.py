"""CURVE/ZAP transport primitives for Zara daemon authentication.

This module configures transport identity only. Runtime authorization and
principal-scoped persistence remain separate boundaries.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Iterable

import zmq
from zmq.auth.thread import ThreadAuthenticator
from zmq.utils import z85

from zara.security import KeyNotActive, SecurityRegistry


class AuthenticationRequired(RuntimeError):
    pass


def _curve_key(value: str | bytes, *, name: str) -> bytes:
    if isinstance(value, str):
        try:
            encoded = value.encode("ascii")
        except UnicodeEncodeError as error:
            raise ValueError(f"{name} must be ASCII Z85") from error
    elif isinstance(value, bytes):
        encoded = value
    else:
        raise TypeError(f"{name} must be str or bytes")

    try:
        raw = z85.decode(encoded)
    except ValueError as error:
        raise ValueError(f"{name} must be valid Z85") from error
    if len(encoded) != 40 or len(raw) != 32:
        raise ValueError(f"{name} must encode exactly 32 bytes")
    return encoded


@dataclass(frozen=True)
class CurveServerConfig:
    public_key: str | bytes
    secret_key: str | bytes
    zap_domain: str = "zara"

    def __post_init__(self) -> None:
        _curve_key(self.public_key, name="server public key")
        _curve_key(self.secret_key, name="server secret key")
        if not isinstance(self.zap_domain, str) or not self.zap_domain.strip():
            raise ValueError("zap_domain must be a non-empty string")
        if self.zap_domain != self.zap_domain.strip():
            raise ValueError("zap_domain must not contain surrounding whitespace")


@dataclass(frozen=True)
class CurveClientConfig:
    public_key: str | bytes
    secret_key: str | bytes
    server_public_key: str | bytes

    def __post_init__(self) -> None:
        _curve_key(self.public_key, name="client public key")
        _curve_key(self.secret_key, name="client secret key")
        _curve_key(self.server_public_key, name="server public key")


class RegistryCredentialsProvider:
    """Live CURVE credential lookup backed by SecurityRegistry."""

    def __init__(self, registry: SecurityRegistry) -> None:
        if not isinstance(registry, SecurityRegistry):
            raise TypeError("registry must be SecurityRegistry")
        self._registry = registry

    @staticmethod
    def _z85_public_key(raw_public_key: bytes) -> str:
        if not isinstance(raw_public_key, bytes) or len(raw_public_key) != 32:
            raise KeyNotActive("client key is not active")
        return z85.encode(raw_public_key).decode("ascii")

    def callback(self, _domain, raw_public_key: bytes) -> bool:
        try:
            self._registry.resolve_public_key(self._z85_public_key(raw_public_key))
        except (KeyNotActive, ValueError, TypeError):
            return False
        return True

    def user_id(self, raw_public_key: bytes) -> str:
        public_key = self._z85_public_key(raw_public_key)
        return self._registry.resolve_public_key(public_key).user_id


class RegistryAuthenticator(ThreadAuthenticator):
    """Threaded ZAP authenticator whose CURVE policy uses live registry state."""

    def __init__(
        self,
        context: zmq.Context | None = None,
        *,
        registry: SecurityRegistry,
        domain: str = "zara",
    ) -> None:
        if not isinstance(domain, str) or not domain.strip():
            raise ValueError("domain must be a non-empty string")
        if domain != domain.strip():
            raise ValueError("domain must not contain surrounding whitespace")
        super().__init__(context=context)
        self._provider = RegistryCredentialsProvider(registry)
        self.configure_curve_callback(domain=domain, credentials_provider=self._provider)

    def curve_user_id(self, client_public_key: bytes) -> str:
        return self._provider.user_id(client_public_key)


def configure_curve_server_socket(socket: zmq.Socket, config: CurveServerConfig) -> None:
    if not isinstance(config, CurveServerConfig):
        raise TypeError("config must be CurveServerConfig")
    socket.curve_publickey = _curve_key(config.public_key, name="server public key")
    socket.curve_secretkey = _curve_key(config.secret_key, name="server secret key")
    socket.curve_server = True
    socket.zap_domain = config.zap_domain.encode("utf-8")


def configure_curve_client_socket(socket: zmq.Socket, config: CurveClientConfig) -> None:
    if not isinstance(config, CurveClientConfig):
        raise TypeError("config must be CurveClientConfig")
    socket.curve_publickey = _curve_key(config.public_key, name="client public key")
    socket.curve_secretkey = _curve_key(config.secret_key, name="client secret key")
    socket.curve_serverkey = _curve_key(config.server_public_key, name="server public key")


def authenticated_user_id(frames: Iterable[object]) -> str:
    for frame in frames:
        try:
            user_id = frame["User-Id"]
        except (KeyError, TypeError):
            continue
        if isinstance(user_id, bytes):
            try:
                user_id = user_id.decode("utf-8")
            except UnicodeDecodeError as error:
                raise AuthenticationRequired("authenticated identity is missing") from error
        if isinstance(user_id, str) and user_id:
            return user_id
        raise AuthenticationRequired("authenticated identity is missing")
    raise AuthenticationRequired("authenticated identity is missing")


__all__ = [
    "AuthenticationRequired",
    "CurveClientConfig",
    "CurveServerConfig",
    "RegistryAuthenticator",
    "RegistryCredentialsProvider",
    "authenticated_user_id",
    "configure_curve_client_socket",
    "configure_curve_server_socket",
]
