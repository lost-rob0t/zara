from __future__ import annotations

import concurrent.futures

import pytest

from zara.plugins.api import PluginRuntime, RuntimeStatus


def _completed_dispatch(_command):
    future = concurrent.futures.Future()
    future.set_result(None)
    return future


def _status():
    return RuntimeStatus(state="running", alive=True, thread_id=1)


def _subscriber(*, maxsize):
    raise AssertionError(f"unexpected subscription with maxsize={maxsize}")


def test_plugin_runtime_binds_advice_owner_and_unregisters_on_shutdown():
    registered = []
    unregistered = []

    def register(kind, owner, priority, callback):
        registered.append((kind, owner, priority, callback))
        return len(registered)

    runtime = PluginRuntime(
        plugin_name="sample",
        configuration={},
        status_provider=_status,
        dispatcher=_completed_dispatch,
        subscriber=_subscriber,
        failure_callback=lambda _message: None,
        advice_registrar=register,
        advice_unregistrar=unregistered.append,
    )

    callback = lambda *_args, **_kwargs: None
    first = runtime.register_agent_loop_advice("before", 5, callback)
    second = runtime.register_agent_loop_advice("after", 10, callback)

    assert (first, second) == (1, 2)
    assert registered == [
        ("before", "plugin:sample", 5, callback),
        ("after", "plugin:sample", 10, callback),
    ]

    runtime._shutdown()
    runtime._shutdown()

    assert unregistered == [1, 2]
    with pytest.raises(RuntimeError, match="closed"):
        runtime.register_agent_loop_advice("before", 0, callback)


def test_plugin_runtime_rejects_advice_when_backend_does_not_support_it():
    runtime = PluginRuntime(
        plugin_name="sample",
        configuration={},
        status_provider=_status,
        dispatcher=_completed_dispatch,
        subscriber=_subscriber,
        failure_callback=lambda _message: None,
    )

    with pytest.raises(RuntimeError, match="not available"):
        runtime.register_agent_loop_advice("before", 0, lambda: None)
