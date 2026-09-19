"""Typed Zara peer-node descriptors bound to authenticated registry identity.

Node documents are discovery/session metadata only.  They never select a
principal, replace CURVE/ZAP authentication, or widen the capabilities granted
by the durable security registry.
"""

from __future__ import annotations

import enum
import re
from dataclasses import dataclass
from typing import Mapping
from urllib.parse import urlsplit

from zara.security import Capability, EnrolledKey, SecurityRegistry

_MAX_DISPLAY_NAME_BYTES = 256
_MAX_ENDPOINT_BYTES = 512
_MAX_ENDPOINTS = 32
_MAX_PROTOCOL_VERSIONS = 16
_PROTOCOL_VERSION_RE = re.compile(r"ZARA/[1-9][0-9]*\Z")
_WIRE_FIELDS = frozenset(
    {
        "node_id",
        "display_name",
        "device_class",
        "curve_public_key",
        "endpoints",
        "capabilities",
        "protocol_versions",
        "last_seen",
        "enrollment_generation",
    }
)


class NodeAuthorityError(ValueError):
    """A node document conflicts with authenticated registry authority."""


class DeviceClass(str, enum.Enum):
    ANDROID = "android"
    DESKTOP = "desktop"
    SERVER = "server"
    HOSTED = "hosted"


def _bounded_text(name: str, value: object, *, max_bytes: int) -> str:
    if not isinstance(value, str) or not value or value != value.strip():
        raise ValueError(f"{name} must be a non-empty trimmed string")
    if any(ord(character) < 0x20 or ord(character) == 0x7F for character in value):
        raise ValueError(f"{name} must not contain control characters")
    if len(value.encode("utf-8")) > max_bytes:
        raise ValueError(f"{name} exceeds byte limit")
    return value


def _node_id(value: object) -> str:
    if not isinstance(value, str):
        raise ValueError("node_id is invalid")
    try:
        return SecurityRegistry._normalize_device_id(value)
    except ValueError as error:
        raise ValueError("node_id is invalid") from error


def _endpoint(value: object) -> str:
    endpoint = _bounded_text("endpoint", value, max_bytes=_MAX_ENDPOINT_BYTES)
    try:
        parsed = urlsplit(endpoint)
        port = parsed.port
    except ValueError as error:
        raise ValueError("endpoint is invalid") from error
    if (
        parsed.scheme.lower() != "tcp"
        or not parsed.hostname
        or port is None
        or port not in range(1, 65536)
        or parsed.username is not None
        or parsed.password is not None
        or parsed.path not in ("", "/")
        or parsed.query
        or parsed.fragment
    ):
        raise ValueError("endpoint must be a bounded tcp://host:port address")
    return endpoint.rstrip("/")


def _sequence(name: str, value: object, *, limit: int) -> list[object]:
    if not isinstance(value, (list, tuple)):
        raise TypeError(f"{name} must be an array")
    if len(value) > limit:
        raise ValueError(f"{name} exceeds item limit")
    return list(value)


@dataclass(frozen=True)
class ZaraNode:
    """Validated ZARA/1 peer descriptor; authentication remains external."""

    node_id: str
    display_name: str
    device_class: DeviceClass
    curve_public_key: str
    endpoints: tuple[str, ...]
    capabilities: frozenset[Capability]
    protocol_versions: frozenset[str]
    last_seen: int
    enrollment_generation: int

    def __post_init__(self) -> None:
        object.__setattr__(self, "node_id", _node_id(self.node_id))
        object.__setattr__(
            self,
            "display_name",
            _bounded_text("display_name", self.display_name, max_bytes=_MAX_DISPLAY_NAME_BYTES),
        )
        if not isinstance(self.device_class, DeviceClass):
            raise TypeError("device_class must be DeviceClass")
        try:
            public_key = SecurityRegistry._normalize_public_key(self.curve_public_key)
        except ValueError as error:
            raise ValueError("CURVE public key is invalid") from error
        object.__setattr__(self, "curve_public_key", public_key)

        if not isinstance(self.endpoints, tuple):
            raise TypeError("endpoints must be a tuple")
        if len(self.endpoints) > _MAX_ENDPOINTS:
            raise ValueError("endpoints exceeds item limit")
        normalized_endpoints = tuple(_endpoint(value) for value in self.endpoints)
        if len(set(normalized_endpoints)) != len(normalized_endpoints):
            raise ValueError("endpoints contains a duplicate endpoint")
        object.__setattr__(self, "endpoints", normalized_endpoints)

        if not isinstance(self.capabilities, frozenset):
            raise TypeError("capabilities must be a frozenset")
        if not all(isinstance(value, Capability) for value in self.capabilities):
            raise TypeError("capabilities must contain Capability values")

        if not isinstance(self.protocol_versions, frozenset) or not self.protocol_versions:
            raise ValueError("protocol_versions must be a non-empty frozenset")
        if len(self.protocol_versions) > _MAX_PROTOCOL_VERSIONS:
            raise ValueError("protocol_versions exceeds item limit")
        for version in self.protocol_versions:
            if not isinstance(version, str) or _PROTOCOL_VERSION_RE.fullmatch(version) is None:
                raise ValueError("protocol version is invalid")

        if type(self.last_seen) is not int or self.last_seen < 0:
            raise ValueError("last_seen must be a non-negative integer")
        if type(self.enrollment_generation) is not int or self.enrollment_generation <= 0:
            raise ValueError("enrollment_generation must be a positive integer")

    @classmethod
    def from_mapping(cls, value: Mapping[str, object]) -> "ZaraNode":
        if not isinstance(value, Mapping) or set(value) != _WIRE_FIELDS:
            raise ValueError("Zara node document has invalid fields")

        try:
            device_class = DeviceClass(value["device_class"])
        except (TypeError, ValueError) as error:
            raise ValueError("device_class is invalid") from error

        endpoints_raw = _sequence("endpoints", value["endpoints"], limit=_MAX_ENDPOINTS)
        endpoints = tuple(_endpoint(endpoint) for endpoint in endpoints_raw)
        if len(set(endpoints)) != len(endpoints):
            raise ValueError("endpoints contains a duplicate endpoint")

        capability_values = _sequence(
            "capabilities",
            value["capabilities"],
            limit=len(Capability),
        )
        if len(set(capability_values)) != len(capability_values):
            raise ValueError("capabilities contains a duplicate capability")
        capabilities: set[Capability] = set()
        for raw in capability_values:
            try:
                capabilities.add(Capability(raw))
            except (TypeError, ValueError) as error:
                raise ValueError("capability is invalid") from error

        protocol_values = _sequence(
            "protocol_versions",
            value["protocol_versions"],
            limit=_MAX_PROTOCOL_VERSIONS,
        )
        if not protocol_values:
            raise ValueError("protocol_versions must not be empty")
        if len(set(protocol_values)) != len(protocol_values):
            raise ValueError("protocol_versions contains a duplicate protocol")

        return cls(
            node_id=value["node_id"],
            display_name=value["display_name"],
            device_class=device_class,
            curve_public_key=value["curve_public_key"],
            endpoints=endpoints,
            capabilities=frozenset(capabilities),
            protocol_versions=frozenset(protocol_values),
            last_seen=value["last_seen"],
            enrollment_generation=value["enrollment_generation"],
        )

    def to_mapping(self) -> dict[str, object]:
        return {
            "node_id": self.node_id,
            "display_name": self.display_name,
            "device_class": self.device_class.value,
            "curve_public_key": self.curve_public_key,
            "endpoints": list(self.endpoints),
            "capabilities": sorted(capability.value for capability in self.capabilities),
            "protocol_versions": sorted(self.protocol_versions),
            "last_seen": self.last_seen,
            "enrollment_generation": self.enrollment_generation,
        }


def verify_authenticated_node(node: ZaraNode, enrolled: EnrolledKey) -> ZaraNode:
    """Bind peer metadata to the already-authenticated durable registry record."""

    if not isinstance(node, ZaraNode):
        raise TypeError("node must be ZaraNode")
    if not isinstance(enrolled, EnrolledKey):
        raise TypeError("enrolled must be EnrolledKey")
    if not enrolled.active:
        raise NodeAuthorityError("authenticated node registry record is inactive")
    if node.curve_public_key != enrolled.public_key:
        raise NodeAuthorityError("node CURVE key does not match authenticated identity")
    if node.node_id != enrolled.device_id:
        raise NodeAuthorityError("node_id does not match authenticated identity")
    if node.enrollment_generation != enrolled.generation:
        raise NodeAuthorityError("node enrollment generation is stale or untrusted")
    if not node.capabilities.issubset(enrolled.capabilities):
        raise NodeAuthorityError("node document cannot widen authenticated capabilities")
    return node


__all__ = [
    "DeviceClass",
    "NodeAuthorityError",
    "ZaraNode",
    "verify_authenticated_node",
]
