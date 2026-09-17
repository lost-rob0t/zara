from __future__ import annotations

import threading
from concurrent.futures import Future
from types import SimpleNamespace

import pytest

from zara import android_device_bridge


class FakeHandle(Future):
    def __init__(self, action_id: str, result):
        super().__init__()
        self.action_id = action_id
        self.set_result(result)


class FakeGateway:
    def __init__(self, *states):
        self._lock = threading.RLock()
        self._routes = {f"route-{index}".encode(): state for index, state in enumerate(states)}
        self.requests = []
        self.cancelled = []

    def request_device_action(self, **kwargs):
        self.requests.append(kwargs)
        result = SimpleNamespace(
            action_id="action-1",
            capability=kwargs["capability"],
            outcome="completed",
            backend="accessibility",
            identity="accessibility:app:10000",
            output="ok",
        )
        return FakeHandle("action-1", result)

    def cancel_device_action(self, action_id, *, reason="cancelled"):
        self.cancelled.append((action_id, reason))
        return True


def state(principal="owner", session="session-1", capabilities=("android_raw",)):
    return SimpleNamespace(
        ready=True,
        principal_id=principal,
        session_id=session,
        capabilities=frozenset(capabilities),
    )


@pytest.fixture(autouse=True)
def clear_routes():
    android_device_bridge.reset_for_tests()
    yield
    android_device_bridge.reset_for_tests()


def test_no_android_raw_route_is_unavailable():
    gateway = FakeGateway(state(capabilities=("open_uri",)))
    android_device_bridge._track_gateway(gateway)

    with pytest.raises(android_device_bridge.AndroidDeviceUnavailable):
        android_device_bridge.unique_route()


def test_multiple_eligible_sessions_are_rejected_instead_of_guessing():
    gateway = FakeGateway(
        state(principal="owner", session="phone-a"),
        state(principal="owner", session="phone-b"),
    )
    android_device_bridge._track_gateway(gateway)

    with pytest.raises(android_device_bridge.AndroidDeviceAmbiguous):
        android_device_bridge.unique_route()


def test_exactly_one_android_route_binds_principal_session_and_raw_capability():
    gateway = FakeGateway(state(principal="owner-1", session="phone-1"))
    android_device_bridge._track_gateway(gateway)

    result = android_device_bridge.execute(
        backend="accessibility",
        operation="accessibility.global_action",
        arguments={"action": "home"},
        timeout_seconds=2,
    )

    assert result.output == "ok"
    assert len(gateway.requests) == 1
    request = gateway.requests[0]
    assert request["principal_id"] == "owner-1"
    assert request["session_id"] == "phone-1"
    assert request["capability"] == "android_raw"
    assert request["args"] == {
        "backend": "accessibility",
        "operation": "accessibility.global_action",
        "arguments": {"action": "home"},
    }
    assert request["idempotency"] == "at_most_once"
    assert request["deadline_ns"] > 0
