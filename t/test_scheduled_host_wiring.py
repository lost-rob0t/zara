"""Runtime-host integration contract for scheduled tasks."""

from test_task_host_wiring import build_environment, stop_host


def test_host_starts_and_stops_canonical_scheduler(monkeypatch, tmp_path):
    host, *_ = build_environment(monkeypatch, tmp_path, tasks_enabled=True)
    try:
        host.start().result(timeout=5)
        assert host.task_runner is not None
        assert host.scheduled_tasks is not None
    finally:
        stop_host(host)

    assert host.task_runner is None
    assert host.scheduled_tasks is None


def test_host_keeps_scheduler_off_with_task_subsystem_gate(monkeypatch, tmp_path):
    host, *_ = build_environment(monkeypatch, tmp_path, tasks_enabled=False)
    try:
        host.start().result(timeout=5)
        assert host.task_runner is None
        assert host.scheduled_tasks is None
    finally:
        stop_host(host)
