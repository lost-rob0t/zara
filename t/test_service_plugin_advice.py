from __future__ import annotations

import concurrent.futures
import textwrap

import pytest

from zara.plugins.api import PluginRuntime, RuntimeStatus
from zara.runtime.backend import RuntimeBackend
from zara.runtime.host import RuntimeHost, RuntimeHostState


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


class _PluginConfig:
    def get_plugin_runtime_config(self):
        return {
            "lifecycle_timeout": 1.0,
            "event_queue_size": 4,
            "max_managed_workers": 2,
        }

    def get_plugin_config(self, _name):
        return {}


class _AdviceBackend(RuntimeBackend):
    def __init__(self):
        self.registered = []
        self.unregistered = []

    def register_agent_loop_advice(self, kind, owner, priority, callback):
        self.registered.append((kind, owner, priority, callback))
        return len(self.registered)

    def unregister_agent_loop_advice(self, registration_id):
        self.unregistered.append(registration_id)
        return True


def _stop_host(host):
    if host.state not in {RuntimeHostState.NEW, RuntimeHostState.STOPPED}:
        host.shutdown("test cleanup").result(timeout=5)
    host.join(timeout=5)


def test_runtime_host_injects_plugin_advice_capability_and_cleans_it_up(tmp_path):
    plugin_path = tmp_path / "advice_plugin.py"
    plugin_path.write_text(
        textwrap.dedent(
            """
            from zara.plugins import PluginMetadata, ServicePlugin

            class TestPlugin(ServicePlugin):
                metadata = PluginMetadata(name="advice-test")

                def start(self, runtime):
                    runtime.register_agent_loop_advice(
                        "before",
                        7,
                        lambda *_args, **_kwargs: None,
                    )

                def stop(self):
                    pass

            def create_plugin():
                return TestPlugin()
            """
        )
    )
    backend = _AdviceBackend()
    host = RuntimeHost(
        lambda: backend,
        plugin_paths=(tmp_path,),
        config=_PluginConfig(),
    )

    try:
        host.start().result(timeout=5)
        assert len(backend.registered) == 1
        kind, owner, priority, callback = backend.registered[0]
        assert (kind, owner, priority) == ("before", "plugin:advice-test", 7)
        assert callable(callback)
    finally:
        _stop_host(host)

    assert backend.unregistered == [1]
