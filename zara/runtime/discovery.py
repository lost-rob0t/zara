"""Backend-neutral runtime discovery for Zara.

Runtime discovery is observation only. It does not grant tool, filesystem,
provider, principal, or plugin authority. Descriptor semantics come from the
canonical :mod:`zara.runtime.registry` contract rather than a parallel type.
"""

from __future__ import annotations

import json
import urllib.error
import urllib.parse
import urllib.request
from typing import Any, Mapping

from .registry import (
    ControlOwner,
    RuntimeDescriptor,
    RuntimeHealth,
    RuntimeLocality,
    RuntimeTransport,
    ZARA_RUNTIME_PROTOCOL,
)

BUILTIN_RUNTIME_ID = "zara-python"
PROLOG_RLM_RUNTIME_ID = "prolog-rlm"
DEFAULT_PROLOG_RLM_ENDPOINT = "http://127.0.0.1:18765"
_MAX_DISCOVERY_BYTES = 64 * 1024


class RuntimeDiscoveryError(RuntimeError):
    """A runtime could not be safely discovered."""


def builtin_runtime_descriptor() -> RuntimeDescriptor:
    """Describe Zara's built-in Python runtime without probing providers."""

    return RuntimeDescriptor(
        id=BUILTIN_RUNTIME_ID,
        display_name="Zara Python",
        protocol=ZARA_RUNTIME_PROTOCOL,
        runtime_version="builtin",
        implementation_version="builtin",
        installed=True,
        available=True,
        health=RuntimeHealth.READY,
        locality=RuntimeLocality.EMBEDDED,
        transport=RuntimeTransport.IN_PROCESS,
        capabilities=("chat", "streaming", "host_tools", "context"),
        profiles=(),
        provider_control=ControlOwner.ZARA,
        model_control=ControlOwner.ZARA,
        supports_streaming=True,
        supports_cancel=True,
        supports_context_handles=True,
        supports_host_tools=True,
        provenance="zara:builtin",
    )


def normalize_loopback_endpoint(value: str) -> str:
    """Return a canonical loopback-only HTTP endpoint or fail closed."""

    if not isinstance(value, str) or not value.strip():
        raise RuntimeDiscoveryError("runtime endpoint must be a non-empty string")
    parsed = urllib.parse.urlsplit(value.strip())
    if parsed.scheme != "http":
        raise RuntimeDiscoveryError("runtime endpoint must use loopback HTTP")
    if parsed.username is not None or parsed.password is not None:
        raise RuntimeDiscoveryError("runtime endpoint must not contain credentials")
    if parsed.query or parsed.fragment:
        raise RuntimeDiscoveryError("runtime endpoint must not contain query or fragment")
    if parsed.hostname not in {"127.0.0.1", "localhost", "::1"}:
        raise RuntimeDiscoveryError("runtime endpoint must resolve to explicit loopback")
    try:
        port = parsed.port
    except ValueError as error:
        raise RuntimeDiscoveryError("runtime endpoint has an invalid port") from error
    if port is None or not 1 <= port <= 65535:
        raise RuntimeDiscoveryError("runtime endpoint requires a valid port")
    host = "[::1]" if parsed.hostname == "::1" else parsed.hostname
    path = parsed.path.rstrip("/")
    return f"http://{host}:{port}{path}"


def runtime_descriptor_from_wire(value: Mapping[str, Any]) -> RuntimeDescriptor:
    """Validate and project one bounded ZARA-RUNTIME descriptor observation.

    Unsupported protocol majors remain observable so Settings can explain why an
    installed runtime cannot be selected. Selection itself stays fail-closed in
    the canonical registry.
    """

    if not isinstance(value, Mapping):
        raise RuntimeDiscoveryError("runtime descriptor must be an object")

    try:
        return RuntimeDescriptor(
            id=_bounded_string(value, "id", 64),
            display_name=_bounded_string(value, "display_name", 128),
            protocol=_bounded_string(value, "protocol", 32),
            runtime_version=_bounded_string(value, "runtime_version", 64),
            implementation_version=_bounded_string(value, "implementation_version", 64),
            installed=_strict_bool(value, "installed"),
            available=_strict_bool(value, "available"),
            health=RuntimeHealth(
                _bounded_choice(
                    value,
                    "health",
                    {"starting", "ready", "busy", "degraded", "failed", "stopped"},
                )
            ),
            locality=RuntimeLocality(
                _bounded_choice(
                    value,
                    "locality",
                    {"embedded", "local_process", "local_sidecar", "remote"},
                )
            ),
            transport=RuntimeTransport(
                _bounded_choice(
                    value,
                    "transport",
                    {"in_process", "stdio", "loopback_http", "binder", "zara_remote"},
                )
            ),
            capabilities=_bounded_string_list(value.get("capabilities", ()), "capabilities", 64),
            profiles=_bounded_string_list(value.get("profiles", ()), "profiles", 32),
            provider_control=ControlOwner(
                _bounded_choice(value, "provider_control", {"runtime", "zara", "mixed"})
            ),
            model_control=ControlOwner(
                _bounded_choice(value, "model_control", {"runtime", "zara", "mixed"})
            ),
            supports_streaming=_strict_bool(value, "supports_streaming"),
            supports_cancel=_strict_bool(value, "supports_cancel"),
            supports_context_handles=_strict_bool(value, "supports_context_handles"),
            supports_host_tools=_strict_bool(value, "supports_host_tools"),
            provenance=_optional_bounded_string(value, "provenance", 512),
        )
    except (TypeError, ValueError) as error:
        raise RuntimeDiscoveryError("runtime descriptor is invalid") from error


class PrologRlmSidecarClient:
    """Small loopback JSON transport for an installed Prolog-RLM sidecar."""

    def __init__(
        self,
        endpoint: str = DEFAULT_PROLOG_RLM_ENDPOINT,
        *,
        timeout: float = 0.35,
        opener=None,
    ) -> None:
        if isinstance(timeout, bool) or not isinstance(timeout, (int, float)) or timeout <= 0:
            raise ValueError("runtime timeout must be positive")
        self.endpoint = normalize_loopback_endpoint(endpoint)
        self.timeout = float(timeout)
        self._opener = opener or urllib.request.build_opener(urllib.request.ProxyHandler({}))

    def discover(self) -> tuple[RuntimeDescriptor, ...]:
        payload = self._request_json("GET", "/zara-runtime/v1/discover")
        raw = payload.get("runtimes")
        if not isinstance(raw, list) or len(raw) > 16:
            raise RuntimeDiscoveryError("runtime discovery payload is invalid")
        descriptors = tuple(runtime_descriptor_from_wire(item) for item in raw)
        ids = [descriptor.id for descriptor in descriptors]
        if len(ids) != len(set(ids)):
            raise RuntimeDiscoveryError("runtime discovery contains duplicate ids")
        return descriptors

    def generate(self, payload: Mapping[str, Any]) -> dict[str, Any]:
        return self._request_json("POST", "/zara-runtime/v1/generate", payload)

    def cancel(self, request_id: str) -> dict[str, Any]:
        return self._request_json(
            "POST",
            "/zara-runtime/v1/cancel",
            {"request_id": request_id},
        )

    def _request_json(
        self,
        method: str,
        path: str,
        payload: Mapping[str, Any] | None = None,
    ) -> dict[str, Any]:
        body = None
        headers = {"Accept": "application/json"}
        if payload is not None:
            body = json.dumps(payload, ensure_ascii=False, separators=(",", ":")).encode("utf-8")
            if len(body) > 512 * 1024:
                raise RuntimeDiscoveryError("runtime request exceeds transport bound")
            headers["Content-Type"] = "application/json"
        request = urllib.request.Request(
            self.endpoint + path,
            data=body,
            headers=headers,
            method=method,
        )
        try:
            with self._opener.open(request, timeout=self.timeout) as response:
                raw = response.read(_MAX_DISCOVERY_BYTES + 1)
        except (OSError, urllib.error.URLError, TimeoutError) as error:
            raise RuntimeDiscoveryError("runtime sidecar is unavailable") from error
        if len(raw) > _MAX_DISCOVERY_BYTES:
            raise RuntimeDiscoveryError("runtime response exceeds transport bound")
        try:
            decoded = json.loads(raw.decode("utf-8"))
        except (UnicodeDecodeError, json.JSONDecodeError) as error:
            raise RuntimeDiscoveryError("runtime returned invalid JSON") from error
        if not isinstance(decoded, dict):
            raise RuntimeDiscoveryError("runtime response must be an object")
        return decoded


def discover_installed_runtimes(config=None) -> tuple[RuntimeDescriptor, ...]:
    """Return current installed runtime observations for Desktop Settings.

    Zara's built-in runtime is always present. Prolog-RLM is added only after an
    explicit loopback sidecar answers discovery. An installed but incompatible or
    unhealthy sidecar remains observable for diagnostics, while the canonical
    descriptor keeps it non-selectable.
    """

    runtimes = [builtin_runtime_descriptor()]
    endpoint = DEFAULT_PROLOG_RLM_ENDPOINT
    timeout = 0.35
    if config is not None:
        getter = getattr(config, "get_runtime_config", None)
        runtime_config = getter() if callable(getter) else {}
        endpoint = str(runtime_config.get("prolog_rlm_endpoint", endpoint))
        timeout = float(runtime_config.get("discovery_timeout", timeout))
    try:
        discovered = PrologRlmSidecarClient(endpoint, timeout=timeout).discover()
    except (RuntimeDiscoveryError, ValueError):
        return tuple(runtimes)

    for descriptor in discovered:
        if descriptor.id == PROLOG_RLM_RUNTIME_ID and descriptor.installed:
            runtimes.append(descriptor)
    return tuple(runtimes)


def _bounded_string(value: Mapping[str, Any], key: str, maximum: int) -> str:
    item = value.get(key)
    if not isinstance(item, str) or not item or len(item) > maximum or any(
        ord(char) < 0x20 for char in item
    ):
        raise RuntimeDiscoveryError(f"runtime descriptor field {key} is invalid")
    return item


def _optional_bounded_string(value: Mapping[str, Any], key: str, maximum: int) -> str:
    item = value.get(key, "")
    if not isinstance(item, str) or len(item) > maximum or any(ord(char) < 0x20 for char in item):
        raise RuntimeDiscoveryError(f"runtime descriptor field {key} is invalid")
    return item


def _bounded_string_list(value: Any, field: str, maximum_items: int) -> tuple[str, ...]:
    if not isinstance(value, (list, tuple)) or len(value) > maximum_items:
        raise RuntimeDiscoveryError(f"runtime descriptor field {field} is invalid")
    items: list[str] = []
    for item in value:
        if not isinstance(item, str) or not item or len(item) > 96:
            raise RuntimeDiscoveryError(f"runtime descriptor field {field} is invalid")
        items.append(item)
    if len(items) != len(set(items)):
        raise RuntimeDiscoveryError(f"runtime descriptor field {field} contains duplicates")
    return tuple(items)


def _strict_bool(value: Mapping[str, Any], key: str) -> bool:
    item = value.get(key)
    if type(item) is not bool:
        raise RuntimeDiscoveryError(f"runtime descriptor field {key} must be boolean")
    return item


def _bounded_choice(value: Mapping[str, Any], key: str, allowed: set[str]) -> str:
    item = _bounded_string(value, key, 32)
    if item not in allowed:
        raise RuntimeDiscoveryError(f"runtime descriptor field {key} is unsupported")
    return item
