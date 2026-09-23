from __future__ import annotations

import threading
from pathlib import Path

from zara.notification_routing import (
    NotificationActionRequest,
    NotificationDenied,
    NotificationRouter,
)

from t.test_notification_routing import (
    NOW,
    Policy,
    SlowEffectPlane,
    _event,
    _peers,
    _store,
)


def test_mirrored_sinks_share_one_source_owned_effect_identity(tmp_path: Path) -> None:
    _, store = _store(tmp_path)
    policy = Policy()
    policy.route = "mirror"
    router = NotificationRouter(local_peer_id="phone", policy=policy, store=store)
    event = _event(notification_id="n:mirror-effect")
    decision = router.route(event, _peers(), now_ms=NOW)

    assert {"desktop", "watch"}.issubset(decision.sinks)

    plane = SlowEffectPlane()
    requests = [
        NotificationActionRequest(
            request_id="transport:desktop",
            notification_id=event.notification_id,
            generation=event.generation,
            principal_id=event.principal_id,
            workspace_id=event.workspace_id,
            sink_peer="desktop",
            action="dismiss",
        ),
        NotificationActionRequest(
            request_id="transport:watch",
            notification_id=event.notification_id,
            generation=event.generation,
            principal_id=event.principal_id,
            workspace_id=event.workspace_id,
            sink_peer="watch",
            action="dismiss",
        ),
    ]
    start = threading.Barrier(3)
    successes: list[object] = []
    failures: list[BaseException] = []

    def run(request: NotificationActionRequest) -> None:
        start.wait()
        try:
            successes.append(router.perform_action(request, plane, now_ms=NOW + 1))
        except BaseException as exc:
            failures.append(exc)

    threads = [threading.Thread(target=run, args=(request,)) for request in requests]
    for thread in threads:
        thread.start()
    start.wait()
    for thread in threads:
        thread.join(timeout=5)
        assert not thread.is_alive()

    assert len(plane.executed) == 1
    assert plane.executed[0][0] in {"transport:desktop", "transport:watch"}
    assert plane.executed[0][1] == "phone"
    assert len(successes) == 1
    assert len(failures) == 1
    assert isinstance(failures[0], NotificationDenied)
