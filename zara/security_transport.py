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


_MIN_SECURE_CURVE_LIBZMQ = (4, 3, 3)
_ZAP_MAX_STRING_BYTES = 255


class AuthenticationRequired(RuntimeError):
    pass


def require_secure_curve_runtime() -> tuple[int, ...]:
    """Reject libzmq releases with known remotely reachable CURVE/ZAP flaws."""
    version = tuple(int(part) for part in zmq.zmq_version_info())
    if version < _MIN_SECURE_CURVE_LIBZMQ:
        minimum = ".".join(str(part) for part in _MIN_SECURE_CURVE_LIBZMQ)
        actual = ".".join(str(part) for part in version)
        raise RuntimeError(
            f"secure CURVE/ZAP requires libzmq >= {minimum}; found {actual}"
        )
    return version


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
    except (ValueError, KeyError) as error:
        raise ValueError(f"{name} must be valid Z85") from error
    if len(encoded) != 40 or len(raw) != 32:
        raise ValueError(f"{name} must encode exactly 32 bytes")
    return encoded


def _curve_pair(
    public_key: str | bytes,
    secret_key: str | bytes,
    *,
    prefix: str,
) -> tuple[bytes, bytes]:
    public = _curve_key(public_key, name=f"{prefix} public key")
    secret = _curve_key(secret_key, name=f"{prefix} secret key")
    try:
        derived = zmq.curve_public(secret)
    except (TypeError, ValueError, KeyError) as error:
        raise ValueError(f"{prefix} secret key cannot derive a public key") from error
    if isinstance(derived, str):
        derived = derived.encode("ascii")
    if derived != public:
        raise ValueError(f"{prefix} CURVE public/secret key pair does not match")
    return public, secret


def _zap_string(name: str, value: object, *, allow_empty: bool = False) -> str:
    if not isinstance(value, str):
        raise ValueError(f"{name} must be an ASCII string")
    if not value and not allow_empty:
        raise ValueError(f"{name} must be a non-empty ASCII string")
    try:
        encoded = value.encode("ascii")
    except UnicodeEncodeError as error:
        raise ValueError(f"{name} must be ASCII") from error
    if len(encoded) > _ZAP_MAX_STRING_BYTES:
        raise ValueError(f"{name} exceeds ZAP string limit")
    if any(byte < 0x21 or byte > 0x7E for byte in encoded):
        raise ValueError(f"{name} must be printable ASCII without whitespace")
    return value


@dataclass(frozen=True)
class CurveServerConfig:
    public_key: str | bytes
    secret_key: str | bytes
    zap_domain: str = "zara"

    def __post_init__(self) -> None:
        _curve_pair(self.public_key, self.secret_key, prefix="server")
        domain = _zap_string("zap_domain", self.zap_domain)
        if domain != domain.strip():
            raise ValueError("zap_domain must not contain surrounding whitespace")


@dataclass(frozen=True)
class CurveClientConfig:
    public_key: str | bytes
    secret_key: str | bytes
    server_public_key: str | bytes

    def __post_init__(self) -> None:
        _curve_pair(self.public_key, self.secret_key, prefix="client")
        _curve_key(self.server_public_key, name="server public key")


class RegistryCredentialsProvider:
    """Live CURVE credential lookup backed by SecurityRegistry."""

    def __init__(self, registry: SecurityRegistry) -> None:
        if not isinstance(registry, SecurityRegistry):
            raise TypeError("registry must be SecurityRegistry")
        self._registry = registry

    @staticmethod
    def _z85_public_key(public_key: bytes) -> str:
        if not isinstance(public_key, bytes):
            raise KeyNotActive("client key is not active")
        if len(public_key) == 32:
            return z85.encode(public_key).decode("ascii")
        if len(public_key) == 40:
            try:
                z85.decode(public_key)
                return public_key.decode("ascii")
            except (UnicodeDecodeError, ValueError, KeyError) as error:
                raise KeyNotActive("client key is not active") from error
        raise KeyNotActive("client key is not active")

    def callback(self, _domain, public_key: bytes) -> bool:
        try:
            self._registry.resolve_public_key(self._z85_public_key(public_key))
        except (KeyNotActive, ValueError, TypeError):
            return False
        return True

    def user_id(self, raw_public_key: bytes) -> str:
        public_key = self._z85_public_key(raw_public_key)
        user_id = self._registry.resolve_public_key(public_key).user_id
        try:
            return _zap_string("authenticated user id", user_id)
        except ValueError as error:
            raise KeyNotActive("client key is not active") from error


class RegistryAuthenticator(ThreadAuthenticator):
    """Threaded ZAP authenticator whose CURVE policy uses live registry state."""

    def __init__(
        self,
        context: zmq.Context | None = None,
        *,
        registry: SecurityRegistry,
        domain: str = "zara",
    ) -> None:
        normalized_domain = _zap_string("domain", domain)
        super().__init__(context=context)
        self._provider = RegistryCredentialsProvider(registry)
        self.configure_curve_callback(domain=normalized_domain, credentials_provider=self._provider)

    def curve_user_id(self, client_public_key: bytes) -> str:
        return self._provider.user_id(client_public_key)


def configure_curve_server_socket(socket: zmq.Socket, config: CurveServerConfig) -> None:
    if not isinstance(config, CurveServerConfig):
        raise TypeError("config must be CurveServerConfig")
    require_secure_curve_runtime()
    public, secret = _curve_pair(config.public_key, config.secret_key, prefix="server")
    socket.curve_publickey = public
    socket.curve_secretkey = secret
    socket.curve_server = True
    socket.zap_domain = _zap_string("zap_domain", config.zap_domain).encode("ascii")


def configure_curve_client_socket(socket: zmq.Socket, config: CurveClientConfig) -> None:
    if not isinstance(config, CurveClientConfig):
        raise TypeError("config must be CurveClientConfig")
    public, secret = _curve_pair(config.public_key, config.secret_key, prefix="client")
    socket.curve_publickey = public
    socket.curve_secretkey = secret
    socket.curve_serverkey = _curve_key(config.server_public_key, name="server public key")


def authenticated_user_id(frames: Iterable[object]) -> str:
    for frame in frames:
        try:
            user_id = frame["User-Id"]
        except (KeyError, TypeError):
            continue
        if isinstance(user_id, bytes):
            try:
                user_id = user_id.decode("ascii")
            except UnicodeDecodeError as error:
                raise AuthenticationRequired("authenticated identity is missing") from error
        try:
            return _zap_string("authenticated user id", user_id)
        except ValueError as error:
            raise AuthenticationRequired("authenticated identity is missing") from error
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
    "require_secure_curve_runtime",
]
