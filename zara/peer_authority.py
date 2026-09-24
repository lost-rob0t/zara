"""Adapter from authenticated ZARA/1 peer state to peer-call authority.

The secure gateway already owns authentication, enrollment generations, and
session routing.  This module only projects those verified records into the
bounded peer ask/delegate contract; it does not create or persist authority.
"""

from __future__ import annotations

from zara.node import ZaraNode, verify_authenticated_node
from zara.peer_protocol import PeerAuthority
from zara.security import EnrolledKey


def peer_authority_from_session(
    *,
    node: ZaraNode,
    enrolled: EnrolledKey,
    session_id: str,
) -> PeerAuthority:
    """Return peer-call authority derived exclusively from authenticated state."""

    verified = verify_authenticated_node(node, enrolled)
    return PeerAuthority(
        principal_id=enrolled.principal.principal_id,
        source_node_id=verified.node_id,
        session_id=session_id,
        enrollment_generation=enrolled.generation,
        capabilities=enrolled.capabilities,
    )


__all__ = ["peer_authority_from_session"]
