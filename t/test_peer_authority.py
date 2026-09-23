from __future__ import annotations

import dataclasses

import pytest
import zmq

from zara.node import DeviceClass, NodeAuthorityError, ZaraNode
from zara.peer_authority import peer_authority_from_session
from zara.principals import PrincipalContext
from zara.security import Capability, EnrolledKey


def _records(*, generation: int = 4):
    public_key, _secret_key = zmq.curve_keypair()
    key = public_key.decode("ascii")
    principal = PrincipalContext("principal-mesh", kind="client")
    enrolled = EnrolledKey(
        public_key=key,
        principal=principal,
        device_id="peer-phone",
        capabilities=frozenset(
            {Capability.SESSION_BASIC, Capability.TURN_SUBMIT, Capability.TURN_CANCEL}
        ),
        user_id="zara:4:test",
        generation=generation,
    )
    node = ZaraNode(
        node_id="peer-phone",
        display_name="Peer Phone",
        device_class=DeviceClass.ANDROID,
        curve_public_key=key,
        endpoints=("tcp://127.0.0.1:17865",),
        capabilities=frozenset({"open_uri"}),
        protocol_versions=frozenset({"ZARA/1"}),
        last_seen=0,
        enrollment_generation=generation,
    )
    return node, enrolled


def test_peer_authority_is_derived_from_verified_authenticated_node_and_session() -> None:
    node, enrolled = _records()

    authority = peer_authority_from_session(
        node=node,
        enrolled=enrolled,
        session_id="session-4",
    )

    assert authority.principal_id == "principal-mesh"
    assert authority.source_node_id == "peer-phone"
    assert authority.session_id == "session-4"
    assert authority.enrollment_generation == 4
    assert authority.capabilities == enrolled.capabilities


def test_device_advertisement_never_elevates_security_capabilities() -> None:
    node, enrolled = _records()
    authority = peer_authority_from_session(
        node=node,
        enrolled=enrolled,
        session_id="session-4",
    )

    assert "open_uri" in node.capabilities
    assert Capability.CONTEXT_READ not in authority.capabilities
    assert authority.capabilities == enrolled.capabilities


def test_stale_node_generation_is_rejected_before_peer_call_admission() -> None:
    node, enrolled = _records(generation=4)
    stale = dataclasses.replace(node, enrollment_generation=3)

    with pytest.raises(NodeAuthorityError, match="generation"):
        peer_authority_from_session(
            node=stale,
            enrolled=enrolled,
            session_id="session-4",
        )


def test_inactive_enrollment_is_rejected() -> None:
    node, enrolled = _records()
    revoked = dataclasses.replace(enrolled, active=False)

    with pytest.raises(NodeAuthorityError, match="inactive"):
        peer_authority_from_session(
            node=node,
            enrolled=revoked,
            session_id="session-4",
        )
