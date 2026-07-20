import asyncio
from unittest.mock import AsyncMock, MagicMock, patch

from zara.prolog_engine import IntentResult, PrologEngine
from zara.wake import WakeWordListener


def test_prolog_engine_propagates_failed_queries():
    engine = PrologEngine.__new__(PrologEngine)
    engine.query_once = MagicMock(return_value=None)
    engine.logger = MagicMock()

    assert engine.execute_intent("unknown", []) is False
    assert engine.reload_config() is False


def build_listener(intent_result):
    listener = WakeWordListener.__new__(WakeWordListener)
    listener.prolog = MagicMock()
    listener.prolog.resolve_intent.return_value = intent_result
    listener.executor = None
    listener.log = MagicMock()
    listener.session_id = None
    listener.memory = MagicMock()
    listener.memory.start_session.return_value = "session"
    listener.agent_manager = MagicMock()
    listener.agent_manager.process_async = AsyncMock(return_value={"response": "fallback"})
    listener.agent_manager.conversation_manager.conversation_history = []
    listener.in_conversation_mode = MagicMock(return_value=False)
    return listener


def test_failed_execution_falls_back_exactly_once():
    listener = build_listener(IntentResult("prolog", "open", ["missing"]))
    listener.prolog.execute_intent.return_value = False

    used_agent, response = asyncio.run(listener.query_with_fallback_async("open missing"))

    assert (used_agent, response) == (True, "fallback")
    listener.prolog.execute_intent.assert_called_once_with("open", ["missing"])
    listener.prolog.resolve_intent.assert_called_once_with(
        "open missing", state="passive"
    )
    listener.agent_manager.process_async.assert_awaited_once_with("open missing")


def test_python_skill_result_executes_from_wake_mode():
    listener = build_listener(IntentResult("python", "search_todos", ["milk"]))

    with patch("zara.wake.python_skills.execute", return_value="found") as execute:
        used_agent, response = asyncio.run(
            listener.query_with_fallback_async("search todos milk")
        )

    assert (used_agent, response) == (False, "found")
    execute.assert_called_once_with("search_todos", ["milk"])
    listener.prolog.execute_intent.assert_not_called()
    listener.agent_manager.process_async.assert_not_awaited()


def test_pending_result_returns_clarification_without_execution():
    listener = build_listener(IntentResult("pending", "open", ["app"]))

    used_agent, response = asyncio.run(listener.query_with_fallback_async("open"))

    assert (used_agent, response) == (False, "Please provide: app.")
    listener.prolog.execute_intent.assert_not_called()
    listener.agent_manager.process_async.assert_not_awaited()


def test_non_command_utterance_skips_prolog_entirely():
    listener = build_listener(None)

    used_agent, response = asyncio.run(
        listener.query_with_fallback_async("why is the sky blue")
    )

    assert (used_agent, response) == (True, "fallback")
    listener.prolog.resolve_intent.assert_not_called()
    listener.prolog.execute_intent.assert_not_called()
    listener.agent_manager.process_async.assert_awaited_once_with(
        "why is the sky blue"
    )


def test_non_command_utterance_does_not_skip_prolog_for_command_shaped_input():
    listener = build_listener(IntentResult("prolog", "open", ["firefox"]))
    listener.prolog.execute_intent.return_value = True

    used_agent, response = asyncio.run(
        listener.query_with_fallback_async("open firefox")
    )

    assert used_agent is False
    assert "Executed" in response
    listener.prolog.resolve_intent.assert_called_once_with(
        "open firefox", state="passive"
    )


def test_conversation_stop_phrase_still_goes_through_prolog():
    listener = build_listener(IntentResult("prolog", "end_conversation", []))

    used_agent, response = asyncio.run(
        listener.query_with_fallback_async("goodbye")
    )

    assert (used_agent, response) == (False, "")
    listener.prolog.resolve_intent.assert_called_once_with(
        "goodbye", state="passive"
    )
    listener.agent_manager.process_async.assert_not_awaited()
