from __future__ import annotations

import pytest

from zara.security import AuthorizationDenied, Capability
from zara.security_gateway import SecureZaraZmqGateway


def test_security_gateway_capability_policy_fails_closed_for_unmapped_message_type():
    assert SecureZaraZmqGateway._capability_for("hello") is Capability.SESSION_BASIC
    assert SecureZaraZmqGateway._capability_for("turn.submit") is Capability.TURN_SUBMIT

    with pytest.raises(AuthorizationDenied, match="unknown daemon message capability"):
        SecureZaraZmqGateway._capability_for("future.admin.action")
