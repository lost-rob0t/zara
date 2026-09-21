"""Bounded per-client configuration profiles.

Client profiles are host-owned configuration. A client may be bound to a named
profile by exact id, while local processes may select a profile explicitly.
Profiles never carry credentials and currently override only LLM routing fields.
"""

from __future__ import annotations

import os
import re
from typing import Any, Mapping

CLIENT_ID_RE = re.compile(r"[A-Za-z0-9._:@+-]{1,128}")
PROFILE_NAME_RE = re.compile(r"[A-Za-z0-9._-]{1,64}")
ALLOWED_PROFILE_SECTIONS = frozenset({"llm"})
ALLOWED_LLM_KEYS = frozenset(
    {
        "provider",
        "model",
        "endpoint",
        "connect_timeout",
        "read_timeout",
        "total_timeout",
        "max_retries",
        "history_limit",
    }
)
SUPPORTED_LLM_PROVIDERS = frozenset({"anthropic", "openai", "openrouter", "ollama"})


class ClientProfileError(ValueError):
    """Raised when a client pin/profile configuration is unsafe or malformed."""


def _bounded_token(value: Any, pattern: re.Pattern[str], label: str) -> str:
    if not isinstance(value, str) or pattern.fullmatch(value) is None:
        raise ClientProfileError(f"{label} is invalid")
    return value


def validate_client_profiles(config: Mapping[str, Any]) -> None:
    pins = config.get("client_pins", {})
    if not isinstance(pins, Mapping):
        raise ClientProfileError("[client_pins] must be a TOML table")

    profiles = config.get("client_profiles", {})
    if not isinstance(profiles, Mapping):
        raise ClientProfileError("[client_profiles] must be a TOML table")

    for client_id, profile_name in pins.items():
        _bounded_token(client_id, CLIENT_ID_RE, "client pin id")
        _bounded_token(profile_name, PROFILE_NAME_RE, "client profile name")
        if profile_name not in profiles:
            raise ClientProfileError(
                f"client pin {client_id!r} references unknown profile {profile_name!r}"
            )

    for profile_name, profile in profiles.items():
        _bounded_token(profile_name, PROFILE_NAME_RE, "client profile name")
        if not isinstance(profile, Mapping):
            raise ClientProfileError(f"client_profiles.{profile_name} must be a TOML table")

        unknown_sections = sorted(set(profile) - ALLOWED_PROFILE_SECTIONS)
        if unknown_sections:
            raise ClientProfileError(
                f"client_profiles.{profile_name} has unsupported section(s): "
                + ", ".join(unknown_sections)
            )

        llm = profile.get("llm", {})
        if not isinstance(llm, Mapping):
            raise ClientProfileError(
                f"client_profiles.{profile_name}.llm must be a TOML table"
            )
        unknown_keys = sorted(set(llm) - ALLOWED_LLM_KEYS)
        if unknown_keys:
            raise ClientProfileError(
                f"client_profiles.{profile_name}.llm has unsupported/secret key(s): "
                + ", ".join(unknown_keys)
            )

        provider = llm.get("provider")
        if provider is not None and provider not in SUPPORTED_LLM_PROVIDERS:
            raise ClientProfileError(
                f"client_profiles.{profile_name}.llm.provider is unsupported"
            )

        for key in ("model", "endpoint"):
            value = llm.get(key)
            if value is not None and (not isinstance(value, str) or len(value) > 2048):
                raise ClientProfileError(
                    f"client_profiles.{profile_name}.llm.{key} must be a bounded string"
                )

        for key in ("connect_timeout", "read_timeout", "total_timeout"):
            value = llm.get(key)
            if value is not None and (
                isinstance(value, bool)
                or not isinstance(value, (int, float))
                or value <= 0
            ):
                raise ClientProfileError(
                    f"client_profiles.{profile_name}.llm.{key} must be positive"
                )

        for key, minimum in (("max_retries", 0), ("history_limit", 1)):
            value = llm.get(key)
            if value is not None and (
                isinstance(value, bool)
                or not isinstance(value, int)
                or value < minimum
            ):
                raise ClientProfileError(
                    f"client_profiles.{profile_name}.llm.{key} must be an integer "
                    f"of at least {minimum}"
                )


def resolve_profile_name(
    config: Mapping[str, Any],
    *,
    client_id: str | None = None,
    profile_name: str | None = None,
) -> str | None:
    """Resolve an explicit/local profile or an exact server-owned client pin."""

    explicit = profile_name or os.getenv("ZARA_CLIENT_PROFILE")
    if explicit:
        name = _bounded_token(explicit, PROFILE_NAME_RE, "client profile name")
        profiles = config.get("client_profiles", {})
        if name not in profiles:
            raise ClientProfileError(f"unknown client profile {name!r}")
        return name

    effective_client = client_id or os.getenv("ZARA_CLIENT_ID")
    if not effective_client:
        return None
    cid = _bounded_token(effective_client, CLIENT_ID_RE, "client pin id")
    pins = config.get("client_pins", {})
    return pins.get(cid)


def section_for_client(
    config: Mapping[str, Any],
    section: str,
    *,
    client_id: str | None = None,
    profile_name: str | None = None,
) -> dict[str, Any]:
    """Return base section plus the named client profile overlay."""

    base = config.get(section, {})
    if not isinstance(base, Mapping):
        raise ClientProfileError(f"[{section}] must be a TOML table")
    merged = dict(base)

    name = resolve_profile_name(
        config,
        client_id=client_id,
        profile_name=profile_name,
    )
    if name is None:
        return merged

    profile = config.get("client_profiles", {}).get(name, {})
    overlay = profile.get(section, {})
    if overlay:
        merged.update(dict(overlay))
    return merged
