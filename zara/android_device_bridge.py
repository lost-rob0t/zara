"""Route agent Android operations to a uniquely eligible ZARA/1 device session.

The bridge deliberately refuses ambiguity. It never chooses a "latest" phone,
never crosses sessions by string coincidence, and only considers sessions that
advertise the explicit ``android_raw`` capability.
"""

from __future__ import annotations

import concurrent.futures
import threading
import time
import weakref
from dataclasses import dataclass
from typing import Any, Mapping


class AndroidDeviceUnavailable(RuntimeError):
    pass


class AndroidDeviceAmbiguous(RuntimeError):
    pass


@dataclass(frozen=True)
class AndroidDeviceRoute:
    gateway: Any
    principal_id: str
    session_id: str


_GATEWAYS: "weakref.WeakSet[Any]" = weakref.WeakSet()
_INSTALL_LOCK = threading.RLock()
_INSTALLED = False


def _track_gateway(gateway: Any) -> None:
    _GATEWAYS.add(gateway)


def install_gateway_tracking() -> None:
    """Patch gateway construction/result decoding once, before server startup."""

    global _INSTALLED
    with _INSTALL_LOCK:
        if _INSTALLED:
            return

        from . import zmq_transport

        gateway_type = zmq_transport.ZaraZmqGateway
        original_init = gateway_type.__init__
        original_handle_device = gateway_type._handle_device_message

        def tracked_init(self, *args, **kwargs):
            from . import android_raw_protocol

            android_raw_protocol.install()
            original_init(self, *args, **kwargs)
            _track_gateway(self)

        def rich_handle_device(self, socket, route, state, message):
            if message.type != "device.action.result":
                return original_handle_device(self, socket, route, state, message)

            # Let the original implementation own every invalid/stale/not-yet-
            # accepted path. Intercept only a valid accepted terminal result.
            if message.session_id != state.session_id:
                return original_handle_device(self, socket, route, state, message)
            body = dict(message.body or {})
            action_id = body.get("action_id")
            if not isinstance(action_id, str):
                return original_handle_device(self, socket, route, state, message)

            with self._lock:
                pending = self._device_action_pending(route, state, action_id)
                if pending is None or not pending.accepted:
                    return original_handle_device(self, socket, route, state, message)
                self._device_actions.pop(action_id, None)

            if not pending.future.done():
                result = zmq_transport.DeviceActionResult(
                    action_id=action_id,
                    capability=pending.capability,
                    outcome=body["outcome"],
                )
                # Preserve the public dataclass and old equality contract while
                # attaching v1 optional result metadata for android_raw callers.
                object.__setattr__(result, "backend", body.get("backend"))
                object.__setattr__(result, "identity", body.get("identity"))
                object.__setattr__(result, "output", body.get("output"))
                pending.future.set_result(result)

        gateway_type.__init__ = tracked_init
        gateway_type._handle_device_message = rich_handle_device
        _INSTALLED = True


def eligible_routes() -> tuple[AndroidDeviceRoute, ...]:
    install_gateway_tracking()
    routes: list[AndroidDeviceRoute] = []
    seen: set[tuple[int, str, str]] = set()
    for gateway in tuple(_GATEWAYS):
        lock = getattr(gateway, "_lock", None)
        route_states = getattr(gateway, "_routes", None)
        if lock is None or route_states is None:
            continue
        with lock:
            for state in tuple(route_states.values()):
                if not getattr(state, "ready", False):
                    continue
                capabilities = frozenset(getattr(state, "capabilities", ()))
                if "android_raw" not in capabilities:
                    continue
                principal_id = getattr(state, "principal_id", None)
                session_id = getattr(state, "session_id", None)
                if not isinstance(principal_id, str) or not isinstance(session_id, str):
                    continue
                key = (id(gateway), principal_id, session_id)
                if key in seen:
                    continue
                seen.add(key)
                routes.append(AndroidDeviceRoute(gateway, principal_id, session_id))
    return tuple(routes)


def unique_route() -> AndroidDeviceRoute:
    routes = eligible_routes()
    if not routes:
        raise AndroidDeviceUnavailable(
            "No authenticated Android session currently advertises android_raw. "
            "Enable android_authority(unrestricted). on the phone and reconnect."
        )
    if len(routes) != 1:
        raise AndroidDeviceAmbiguous(
            f"Refusing to guess between {len(routes)} Android sessions."
        )
    return routes[0]


def execute(
    *,
    backend: str,
    operation: str,
    arguments: Mapping[str, str] | None = None,
    timeout_seconds: float = 30.0,
):
    route = unique_route()
    timeout = min(max(float(timeout_seconds), 1.0), 300.0)
    deadline_ns = time.time_ns() + int(timeout * 1_000_000_000)
    handle = route.gateway.request_device_action(
        principal_id=route.principal_id,
        session_id=route.session_id,
        capability="android_raw",
        args={
            "backend": backend,
            "operation": operation,
            "arguments": dict(arguments or {}),
        },
        deadline_ns=deadline_ns,
        idempotency="at_most_once",
    )
    try:
        return handle.result(timeout=timeout + 1.0)
    except concurrent.futures.TimeoutError:
        route.gateway.cancel_device_action(handle.action_id, reason="android tool timeout")
        raise AndroidDeviceUnavailable("Android operation timed out") from None


def reset_for_tests() -> None:
    _GATEWAYS.clear()
