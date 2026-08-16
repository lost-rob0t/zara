from __future__ import annotations

import textwrap
import threading
import time

import pytest

from zara.plugins import PluginState
from zara.plugins.loader import load_plugin_module, load_plugins
from zara.runtime import bridge as runtime_bridge
from zara.runtime import events
from zara.runtime.backend import RuntimeBackend
from zara.runtime.commands import StartVoice
from zara.runtime.host import RuntimeHost, RuntimeHostState


class PluginTestConfig:
    def __init__(self, plugin_values=None):
        self.plugin_values = plugin_values or {}

    def get_plugin_runtime_config(self):
        return {
            "lifecycle_timeout": 1.0,
            "event_queue_size": 4,
            "max_managed_workers": 2,
        }

    def get_plugin_config(self, name):
        return dict(self.plugin_values.get(name, {}))


class PluginBackend(RuntimeBackend):
    def __init__(self):
        self.tools = []
        self.voice_thread_id = None

    def register_tools(self, tools) -> None:
        self.tools.extend(tools)

    def unregister_tools(self, names) -> None:
        owned = set(names)
        self.tools = [tool for tool in self.tools if tool.name not in owned]

    async def start_voice(self) -> None:
        self.voice_thread_id = threading.get_ident()


class RecordingPublisher:
    def __init__(self):
        self.bus = runtime_bridge.RuntimeEventBus()
        self.events = []

    def __call__(self, event):
        self.events.append(event)
        return self.bus.publish(event)


def stop_host(host):
    if host.state not in {RuntimeHostState.NEW, RuntimeHostState.STOPPED}:
        host.shutdown("test cleanup").result(timeout=5)
    host.join(timeout=5)


def write_service_plugin(path, *, start_body="pass", stop_body="pass", api_version="1"):
    path.write_text(
        textwrap.dedent(
            f"""
            import threading
            from langchain_core.tools import StructuredTool
            from zara.plugins import PluginMetadata, ServicePlugin

            CREATE_COUNT = 0
            START_COUNT = 0
            STOP_COUNT = 0
            RUNTIME = None
            SUBSCRIPTION = None
            WORKER = None
            WORKER_THREAD_ID = None

            def echo(value: str) -> str:
                return value

            class TestPlugin(ServicePlugin):
                metadata = PluginMetadata(
                    name="test-service",
                    version="1.2.3",
                    api_version={api_version!r},
                    description="test service",
                )

                def start(self, runtime):
                    global START_COUNT, RUNTIME, SUBSCRIPTION, WORKER
                    START_COUNT += 1
                    RUNTIME = runtime
                    SUBSCRIPTION = runtime.subscribe()
                    {textwrap.indent(start_body, ' ' * 20).lstrip()}

                def stop(self):
                    global STOP_COUNT
                    STOP_COUNT += 1
                    {textwrap.indent(stop_body, ' ' * 20).lstrip()}

                def tools(self):
                    return [StructuredTool.from_function(
                        echo,
                        name="service_echo",
                        description="Echo a value for lifecycle tests.",
                    )]

            def create_plugin():
                global CREATE_COUNT
                CREATE_COUNT += 1
                return TestPlugin()
            """
        )
    )


def test_external_service_plugin_lifecycle_tools_events_config_and_diagnostics(tmp_path):
    plugin_path = tmp_path / "service_plugin.py"
    write_service_plugin(plugin_path)
    backend = PluginBackend()
    publisher = RecordingPublisher()
    config = PluginTestConfig({"test-service": {"enabled": True, "port": 4321}})
    module = load_plugin_module(plugin_path)
    assert module.CREATE_COUNT == 0
    assert module.START_COUNT == 0
    host = RuntimeHost(
        lambda: backend,
        publisher=publisher,
        subscriber=publisher.bus.subscribe,
        plugin_paths=(tmp_path,),
        config=config,
    )

    try:
        host.start().result(timeout=5)
        host.start().result(timeout=5)
        assert module.CREATE_COUNT == 1
        assert module.START_COUNT == 1
        assert module.RUNTIME.status.state == "running"
        assert dict(module.RUNTIME.configuration) == {"enabled": True, "port": 4321}
        assert [tool.name for tool in backend.tools] == ["service_echo"]
        assert isinstance(module.SUBSCRIPTION.get(timeout=1).event, events.RuntimeStarted)

        diagnostics = host.plugin_diagnostics()
        assert [(item.name, item.version, item.plugin_type, item.state) for item in diagnostics] == [
            ("test-service", "1.2.3", "service", PluginState.RUNNING)
        ]
    finally:
        stop_host(host)

    host.shutdown("already stopped").result(timeout=1)
    assert module.STOP_COUNT == 1
    assert module.SUBSCRIPTION.closed is True
    assert backend.tools == []
    assert host.plugin_diagnostics()[0].state is PluginState.STOPPED


def test_service_worker_dispatches_on_runtime_thread_and_is_cleaned_up(tmp_path):
    plugin_path = tmp_path / "dispatch_plugin.py"
    write_service_plugin(
        plugin_path,
        start_body="""
def worker(stop_event):
    global WORKER_THREAD_ID
    WORKER_THREAD_ID = threading.get_ident()
    runtime.dispatch(StartVoice()).result(timeout=2)
    stop_event.wait()
from zara.runtime.commands import StartVoice
WORKER = runtime.start_worker("dispatcher", worker)
""",
    )
    backend = PluginBackend()
    host = RuntimeHost(
        lambda: backend,
        plugin_paths=(tmp_path,),
        config=PluginTestConfig(),
    )

    host.start().result(timeout=5)
    module = load_plugin_module(plugin_path)
    deadline = time.monotonic() + 2
    while backend.voice_thread_id is None and time.monotonic() < deadline:
        time.sleep(0.01)

    assert backend.voice_thread_id == host.thread_id
    assert module.WORKER_THREAD_ID != host.thread_id
    worker = module.WORKER

    stop_host(host)

    assert worker.is_alive is False
    assert host.is_alive is False


def test_service_subscription_is_bounded_and_can_unsubscribe(tmp_path):
    plugin_path = tmp_path / "events_plugin.py"
    write_service_plugin(plugin_path)
    publisher = RecordingPublisher()
    host = RuntimeHost(
        lambda: PluginBackend(),
        publisher=publisher,
        subscriber=publisher.bus.subscribe,
        plugin_paths=(tmp_path,),
        config=PluginTestConfig(),
    )

    try:
        host.start().result(timeout=5)
        module = load_plugin_module(plugin_path)
        module.SUBSCRIPTION.drain()
        for label in ("one", "two", "three", "four", "five"):
            publisher.bus.publish(events.RuntimeIdle(label=label))

        received = module.SUBSCRIPTION.drain()
        assert [item.event.label for item in received] == ["two", "three", "four", "five"]
        assert module.SUBSCRIPTION.dropped_count == 1

        module.SUBSCRIPTION.close()
        publisher.bus.publish(events.RuntimeIdle(label="after-close"))
        assert module.SUBSCRIPTION.drain() == []
        with pytest.raises(ValueError, match="between 1 and 4096"):
            module.RUNTIME.subscribe(maxsize=0)

        subscriptions = [module.RUNTIME.subscribe() for _ in range(16)]
        with pytest.raises(RuntimeError, match="subscription limit"):
            module.RUNTIME.subscribe()
        subscriptions[0].close()
        replacement = module.RUNTIME.subscribe()
        assert replacement.closed is False
    finally:
        stop_host(host)


@pytest.mark.parametrize(
    ("start_body", "stop_body", "error_text"),
    [
        ('raise RuntimeError("startup boom")', "pass", "startup boom"),
        ("pass", 'raise RuntimeError("shutdown boom")', "shutdown boom"),
    ],
)
def test_service_lifecycle_failures_are_contained(
    tmp_path, start_body, stop_body, error_text
):
    plugin_path = tmp_path / f"failure_{error_text.split()[0]}.py"
    write_service_plugin(plugin_path, start_body=start_body, stop_body=stop_body)
    publisher = RecordingPublisher()
    host = RuntimeHost(
        lambda: PluginBackend(),
        publisher=publisher,
        subscriber=publisher.bus.subscribe,
        plugin_paths=(tmp_path,),
        config=PluginTestConfig(),
    )

    host.start().result(timeout=5)
    assert host.state is RuntimeHostState.RUNNING
    module = load_plugin_module(plugin_path)

    stop_host(host)

    assert module.START_COUNT == 1
    assert module.STOP_COUNT == 1
    diagnostic = host.plugin_diagnostics()[0]
    assert diagnostic.state is PluginState.FAILED
    assert error_text in diagnostic.error
    assert any(
        isinstance(event, events.RuntimeError)
        and event.label == "plugin:test-service"
        and error_text in event.reason
        for event in publisher.events
    )


def test_managed_worker_failure_does_not_kill_runtime(tmp_path):
    plugin_path = tmp_path / "worker_failure.py"
    write_service_plugin(
        plugin_path,
        start_body="""
def worker(_stop_event):
    raise RuntimeError("worker boom")
WORKER = runtime.start_worker("broken", worker)
""",
    )
    host = RuntimeHost(
        lambda: PluginBackend(),
        plugin_paths=(tmp_path,),
        config=PluginTestConfig(),
    )

    try:
        host.start().result(timeout=5)
        deadline = time.monotonic() + 2
        while time.monotonic() < deadline:
            diagnostics = host.plugin_diagnostics()
            if diagnostics and diagnostics[0].state is PluginState.FAILED:
                break
            time.sleep(0.01)

        assert host.state is RuntimeHostState.RUNNING
        assert host.is_alive is True
        assert host.plugin_diagnostics()[0].state is PluginState.FAILED
        assert "worker boom" in host.plugin_diagnostics()[0].error
    finally:
        stop_host(host)


def test_incompatible_service_plugin_is_not_started(tmp_path):
    plugin_path = tmp_path / "future_plugin.py"
    write_service_plugin(plugin_path, api_version="999")
    host = RuntimeHost(
        lambda: PluginBackend(),
        plugin_paths=(tmp_path,),
        config=PluginTestConfig(),
    )

    try:
        host.start().result(timeout=5)
        module = load_plugin_module(plugin_path)
        diagnostic = host.plugin_diagnostics()[0]

        assert module.CREATE_COUNT == 1
        assert module.START_COUNT == 0
        assert diagnostic.state is PluginState.INCOMPATIBLE
        assert "999" in diagnostic.error
    finally:
        stop_host(host)


@pytest.mark.parametrize("entrypoint", ["register_tools", "register_skills"])
def test_legacy_tool_and_skill_plugins_still_load(tmp_path, entrypoint):
    plugin_path = tmp_path / f"legacy_{entrypoint}.py"
    plugin_path.write_text(
        textwrap.dedent(
            f"""
            from langchain_core.tools import StructuredTool

            def echo(value: str) -> str:
                return value

            def {entrypoint}(_prolog_engine):
                return [StructuredTool.from_function(
                    echo,
                    name="legacy_echo",
                    description="Echo a value for legacy plugin tests.",
                )]
            """
        )
    )

    tools = load_plugins(str(tmp_path))

    assert [tool.name for tool in tools] == ["legacy_echo"]


def test_invalid_legacy_plugin_is_contained(tmp_path):
    (tmp_path / "broken.py").write_text(
        "def register_tools(_prolog_engine):\n    return [object()]\n"
    )

    assert load_plugins(str(tmp_path)) == []
