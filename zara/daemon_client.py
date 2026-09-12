"""Canonical daemon-client selection for Zara application surfaces."""

from __future__ import annotations

import os
from typing import TYPE_CHECKING, Optional

from zara.config import ZaraConfig, get_config
from zara.security_transport import CurveClientConfig
from zara.server import ServerLease, default_zmq_endpoint

if TYPE_CHECKING:
    from zara.zmq_transport import ZmqZaraClient


DAEMON_ENDPOINT_ENV = "ZARA_DAEMON_ENDPOINT"
CURVE_PUBLIC_KEY_ENV = "ZARA_DAEMON_CURVE_PUBLIC_KEY"
CURVE_SECRET_KEY_ENV = "ZARA_DAEMON_CURVE_SECRET_KEY"
CURVE_SERVER_PUBLIC_KEY_ENV = "ZARA_DAEMON_CURVE_SERVER_PUBLIC_KEY"


def _daemon_section(config: Optional[ZaraConfig] = None) -> dict:
    active = config or get_config()
    section = active.get_section("daemon") or {}
    if not isinstance(section, dict):
        raise ValueError("daemon configuration must be a table")
    return section


def resolve_daemon_endpoint(
    config: Optional[ZaraConfig] = None,
    *,
    explicit: Optional[str] = None,
) -> str:
    """Resolve one client endpoint with explicit/configured/local precedence.

    Explicit CLI/API selection wins. The environment override is next so a
    service manager or Home Manager profile can select the deployment endpoint
    without owning the user's mutable config.toml. ``[daemon].endpoint`` is
    the normal persistent user setting. Empty values retain Zara's private
    owner-local IPC default.
    """
    if explicit is not None:
        endpoint = str(explicit).strip()
        if not endpoint:
            raise ValueError("explicit daemon endpoint must not be empty")
        return endpoint

    environment_endpoint = os.getenv(DAEMON_ENDPOINT_ENV, "").strip()
    if environment_endpoint:
        return environment_endpoint

    configured_endpoint = str(_daemon_section(config).get("endpoint") or "").strip()
    if configured_endpoint:
        return configured_endpoint

    return default_zmq_endpoint(ServerLease()._runtime_dir())


def curve_client_config(config: Optional[ZaraConfig] = None) -> Optional[CurveClientConfig]:
    """Build configured CURVE credentials, rejecting partial credential sets."""
    section = _daemon_section(config)
    public_key = os.getenv(CURVE_PUBLIC_KEY_ENV) or section.get("curve_public_key")
    secret_key = os.getenv(CURVE_SECRET_KEY_ENV) or section.get("curve_secret_key")
    server_public_key = (
        os.getenv(CURVE_SERVER_PUBLIC_KEY_ENV)
        or section.get("curve_server_public_key")
    )

    values = tuple(
        str(value).strip() if value is not None else ""
        for value in (public_key, secret_key, server_public_key)
    )
    if not any(values):
        return None
    if not all(values):
        raise ValueError(
            "daemon CURVE client authentication requires public, secret, and server public keys"
        )

    return CurveClientConfig(
        public_key=values[0],
        secret_key=values[1],
        server_public_key=values[2],
    )


def create_daemon_client(
    endpoint: Optional[str] = None,
    *,
    config: Optional[ZaraConfig] = None,
    **kwargs,
) -> "ZmqZaraClient":
    """Construct the canonical configured ZARA/1 client."""
    if "curve_client" in kwargs:
        raise TypeError("curve_client is owned by Zara daemon client configuration")

    # Resolve the concrete transport at call time. Besides keeping this module
    # focused on policy, this preserves Zara's long-standing transport injection
    # seam used by tests and embedders.
    from zara.zmq_hardening import HardenedZmqZaraClient

    curve_client = curve_client_config(config)
    if curve_client is not None:
        kwargs["curve_client"] = curve_client

    return HardenedZmqZaraClient(
        resolve_daemon_endpoint(config, explicit=endpoint),
        **kwargs,
    )


__all__ = [
    "DAEMON_ENDPOINT_ENV",
    "CURVE_PUBLIC_KEY_ENV",
    "CURVE_SECRET_KEY_ENV",
    "CURVE_SERVER_PUBLIC_KEY_ENV",
    "create_daemon_client",
    "curve_client_config",
    "resolve_daemon_endpoint",
]
