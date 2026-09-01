"""Wake fails closed when the Zara daemon is unavailable (#244)."""

from __future__ import annotations

import pytest

import zara.wake as wake_module
from zara.wake_daemon import WakeDaemonUnavailable


def test_wake_module_never_references_in_process_fallback():
    import inspect

    source = inspect.getsource(wake_module)
    assert "InProcessZaraClient" not in source
    assert "RuntimeHost" not in source
    assert "AgentManager" not in source


def test_connect_to_dead_endpoint_raises_unavailable_with_guidance():
    from zara.wake_daemon import WakeDaemonClient

    client = WakeDaemonClient(endpoint="ipc:///tmp/zara-does-not-exist.sock")
    with pytest.raises(WakeDaemonUnavailable) as excinfo:
        client.connect()
    assert "Zara daemon" in str(excinfo.value)


def test_reconnect_exhaustion_fails_closed(monkeypatch):
    from zara.wake_daemon import WakeDaemonClient
    from t.test_wake_daemon_client import FakeZaraClient

    class BrokenReconnectClient(FakeZaraClient):
        def __init__(self) -> None:
            super().__init__()
            self.state = "FAILED"

        def reconnect_with_backoff(self, **kwargs):
            raise ConnectionError("daemon still down")

    client = WakeDaemonClient(client=BrokenReconnectClient())
    with pytest.raises(WakeDaemonUnavailable) as excinfo:
        client.ensure_connected()
    assert "reconnect" in str(excinfo.value)


def test_wake_runner_exits_with_error_when_daemon_unavailable(monkeypatch, capsys):
    async def failing_run(self):
        raise WakeDaemonUnavailable("Could not reach the Zara daemon: boom")

    monkeypatch.setattr(wake_module.WakeWordListener, "__init__", lambda self, **kwargs: None)
    monkeypatch.setattr(wake_module.WakeWordListener, "run_async", failing_run)
    monkeypatch.setattr(wake_module.WakeWordListener, "log", lambda self, message: None)
    monkeypatch.setattr(wake_module.WakeWordListener, "executor", None, raising=False)
    monkeypatch.setattr(
        wake_module.WakeWordListener, "request_stop", lambda self: None
    )

    exit_code = wake_module.run_wake_listener(model="tiny", device="cpu")

    assert exit_code == 2
    captured = capsys.readouterr()
    assert "Zara daemon" in captured.err


async def _async_noop():
    return None
