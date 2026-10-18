import asyncio

from zara.runtime import events
from zara.voice_agent import VoiceAgentRouter
from t.test_voice_agent_fleet import make_runner


def test_new_tasks_accumulate_while_chatter_and_speech_interrupt_leave_work_running(tmp_path):
    async def scenario():
        async def submit(text, **kwargs):
            await asyncio.Event().wait()
        runner, _, _ = make_runner(tmp_path, submit, max_concurrent=1)
        published = []
        decisions = {
            "research first": {"Route": "new_task", "Argument": "research first"},
            "also review second": {"Route": "new_task", "Argument": "also review second"},
            "nice weather": {"Route": "chat", "Argument": ""},
            "stop talking": {"Route": "speech_only", "Argument": ""},
            "stop all agents": {"Route": "cancel_all", "Argument": ""},
        }
        router = VoiceAgentRouter(runner, resolver=decisions.__getitem__, publisher=published.append)
        await runner.start()
        try:
            assert "task-" in await router.handle("research first", conversation_id="voice")
            assert await router.handle("nice weather", conversation_id="voice") is None
            assert "task-" in await router.handle("also review second", conversation_id="voice")
            assert len(runner.list_tasks()) == 2
            await router.handle("stop talking", conversation_id="voice")
            assert isinstance(published[-1], events.SpeechInterruptRequested)
            assert {task.status.value for task in runner.list_tasks()} == {"running", "pending"}
            await router.handle("stop all agents", conversation_id="voice")
            assert {task.status.value for task in runner.list_tasks()} == {"cancelled"}
        finally:
            await runner.stop()
    asyncio.run(scenario())


def test_unknown_or_failed_prolog_routes_never_start_work_or_fall_through(tmp_path):
    async def scenario():
        async def submit(text, **kwargs):
            raise AssertionError("must not execute")
        runner, _, _ = make_runner(tmp_path, submit)
        await runner.start()
        try:
            for decision in (None, {}, {"Route": "run_shell", "Argument": "bad"}):
                router = VoiceAgentRouter(runner, resolver=lambda text: decision)
                assert "unavailable" in (await router.handle("start something")).lower()
                assert runner.list_tasks() == []
            def broken(text):
                raise RuntimeError("provider secret must not be reflected")
            router = VoiceAgentRouter(runner, resolver=broken)
            response = await router.handle("start something")
            assert "unavailable" in response.lower()
            assert "secret" not in response
        finally:
            await runner.stop()
    asyncio.run(scenario())


def test_ambiguous_this_task_control_does_not_choose_an_arbitrary_agent(tmp_path):
    async def scenario():
        async def submit(text, **kwargs):
            await asyncio.Event().wait()
        runner, _, _ = make_runner(tmp_path, submit)
        await runner.start()
        try:
            await runner.enqueue_task(goal="first", conversation_id="voice")
            await runner.enqueue_task(goal="second", conversation_id="voice")
            router = VoiceAgentRouter(runner, resolver=lambda text: {"Route": "pause_task", "Argument": "current"})
            reply = await router.handle("pause this task", conversation_id="voice")
            assert "task ID" in reply
            assert all(task.status.value == "running" for task in runner.list_tasks())
        finally:
            await runner.stop()
    asyncio.run(scenario())


def test_canonical_backend_routes_user_turn_but_not_task_steps(tmp_path):
    from types import SimpleNamespace
    from zara.runtime.backend import LangGraphRuntimeBackend

    async def scenario():
        calls = []
        async def submit(text, **kwargs):
            await asyncio.Event().wait()
        async def process(text, **kwargs):
            calls.append((text, kwargs))
            return {"response": "TASK_COMPLETE: done", "tool_results": []}
        runner, _, _ = make_runner(tmp_path, submit)
        backend = LangGraphRuntimeBackend()
        backend._manager = SimpleNamespace(
            prolog_engine=SimpleNamespace(resolve_voice_agent=lambda text: {"Route": "new_task", "Argument": text}),
            memory_manager=None, process_async=process,
        )
        backend.bind_task_runner(runner)
        await runner.start()
        try:
            result = await backend.submit_turn("research X", turn_id="spoken")
            assert result.metadata["route"] == "voice_agent"
            assert len(runner.list_tasks()) == 1
            assert calls == []
            await backend.submit_turn("task step", turn_id="worker", conversation_history=[], system_context="bounded task")
            assert len(runner.list_tasks()) == 1
            assert len(calls) == 1
            backend.bind_task_runner(None)
            await backend.submit_turn("normal turn", turn_id="next")
            assert len(calls) == 2
        finally:
            await runner.stop()
    asyncio.run(scenario())
