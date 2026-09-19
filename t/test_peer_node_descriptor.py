from __future__ import annotations

from dataclasses import replace

import pytest
import zmq

from zara.principals import PrincipalContext
from zara.security import Capability, SecurityRegistry
from zara.node import DeviceClass, NodeAuthorityError, ZaraNode, verify_authenticated_node


def _keypair() -> tuple[str, str]:
    public, secret = zmq.curve_keypair()
    return public.decode("ascii"), secret.decode("ascii")


def _node(public_key: str, **overrides: object) -> ZaraNode:
    values: dict[str, object] = {
        "node_id": "phone-alice",
        "display_name": "Alice's phone",
        "device_class": DeviceClass.ANDROID,
        "curve_public_key": public_key,
        "endpoints": ("tcp://192.0.2.10:17865",),
        "capabilities": frozenset({Capability.SESSION_BASIC}),
        "protocol_versions": frozenset({"ZARA/1"}),
        "last_seen": 123456789,
        "enrollment_generation": 1,
    }
    values.update(overrides)
    return ZaraNode(**values)


def test_node_document_round_trips_canonical_wire_shape() -> None:
    public_key, _ = _keypair()
    raw = {
        "node_id": "phone-alice",
        "display_name": "Alice's phone",
        "device_class": "android",
        "curve_public_key": public_key,
        "endpoints": ["tcp://192.0.2.10:17865"],
        "capabilities": ["session.basic", "turn.submit"],
        "protocol_versions": ["ZARA/1"],
        "last_seen": 123456789,
        "enrollment_generation": 3,
    }

    node = ZaraNode.from_mapping(raw)

    assert node.device_class is DeviceClass.ANDROID
    assert node.capabilities == frozenset({Capability.SESSION_BASIC, Capability.TURN_SUBMIT})
    assert node.to_mapping() == raw
    assert "principal_id" not in node.to_mapping()


@pytest.mark.parametrize(
    "field,value,match",
    [
        ("node_id", " phone-alice", "node_id"),
        ("display_name", "", "display_name"),
        ("device_class", "browser", "device_class"),
        ("curve_public_key", "not-z85", "CURVE"),
        ("endpoints", ["http://192.0.2.10:17865"], "endpoint"),
        ("endpoints", ["tcp://192.0.2.10:17865", "tcp://192.0.2.10:17865"], "duplicate"),
        ("capabilities", ["session.basic", "ambient.shell"], "capability"),
        ("protocol_versions", [], "protocol_versions"),
        ("protocol_versions", ["HTTP/1"], "protocol"),
        ("last_seen", -1, "last_seen"),
        ("enrollment_generation", 0, "enrollment_generation"),
    ],
)
def test_node_document_rejects_malformed_or_ambient_fields(
    field: str,
    value: object,
    match: str,
) -> None:
    public_key, _ = _keypair()
    raw = _node(public_key).to_mapping()
    raw[field] = value

    with pytest.raises((TypeError, ValueError), match=match):
        ZaraNode.from_mapping(raw)


def test_node_document_requires_exact_fields() -> None:
    public_key, _ = _keypair()
    raw = _node(public_key).to_mapping()
    raw["principal_id"] = "user:mallory"

    with pytest.raises(ValueError, match="fields"):
        ZaraNode.from_mapping(raw)


def test_authenticated_node_binding_uses_registry_identity_and_only_narrows_capabilities() -> None:
    public_key, _ = _keypair()
    registry = SecurityRegistry()
    enrolled = registry.enroll(
        public_key,
        principal=PrincipalContext.local_owner(),
        device_id="phone-alice",
        capabilities={Capability.SESSION_BASIC, Capability.TURN_SUBMIT},
    )
    node = _node(public_key)

    assert verify_authenticated_node(node, enrolled) is node


@pytest.mark.parametrize("mutation", ["node_id", "curve_public_key", "generation", "capability"])
def test_authenticated_node_binding_rejects_payload_authority_escalation(mutation: str) -> None:
    public_key, _ = _keypair()
    other_public, _ = _keypair()
    registry = SecurityRegistry()
    enrolled = registry.enroll(
        public_key,
        principal=PrincipalContext.local_owner(),
        device_id="phone-alice",
        capabilities={Capability.SESSION_BASIC},
    )
    node = _node(public_key)
    if mutation == "node_id":
        node = replace(node, node_id="phone-mallory")
    elif mutation == "curve_public_key":
        node = replace(node, curve_public_key=other_public)
    elif mutation == "generation":
        node = replace(node, enrollment_generation=enrolled.generation + 1)
    else:
        node = replace(node, capabilities=frozenset({Capability.SESSION_BASIC, Capability.DAEMON_ADMIN}))

    with pytest.raises(NodeAuthorityError):
        verify_authenticated_node(node, enrolled)


def test_authenticated_node_binding_rejects_revoked_registry_record() -> None:
    public_key, _ = _keypair()
    registry = SecurityRegistry()
    enrolled = registry.enroll(
        public_key,
        principal=PrincipalContext.local_owner(),
        device_id="phone-alice",
        capabilities={Capability.SESSION_BASIC},
    )
    revoked = registry.revoke(enrolled.device_id)

    with pytest.raises(NodeAuthorityError, match="inactive"):
        verify_authenticated_node(_node(public_key), revoked)
