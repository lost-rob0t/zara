import asyncio
from unittest.mock import AsyncMock, MagicMock

from zara.prolog_engine import PrologEngine
from zara.wake import WakeWordListener


def test_prolog_engine_propagates_failed_queries():
    engine = PrologEngine.__new__(PrologEngine)
    engine.query_once = MagicMock(return_value=None)
    engine.logger = MagicMock()

    assert engine.execute_intent("unknown", []) is False
    assert engine.start_timer(5) is False
    assert engine.reload_config() is False


def test_failed_execution_falls_back_exactly_once():
    listener = WakeWordListener.__new__(WakeWordListener)
    listener.prolog = MagicMock()
    listener.prolog.resolve_intent.return_value = {"Intent": "open", "Args": ["missing"]}
    listener.prolog.execute_intent.return_value = False
    listener.executor = None
    listener.log = MagicMock()
    listener.session_id = None
    listener.memory = MagicMock()
    listener.memory.start_session.return_value = "session"
    listener.agent_manager = MagicMock()
    listener.agent_manager.process_async = AsyncMock(return_value={"response": "fallback"})
    listener.agent_manager.conversation_manager.conversation_history = []
    listener.in_conversation_mode = MagicMock(return_value=False)

    used_agent, response = asyncio.run(listener.query_with_fallback_async("open missing"))

    assert (used_agent, response) == (True, "fallback")
    listener.prolog.execute_intent.assert_called_once_with("open", ["missing"])
    listener.agent_manager.process_async.assert_awaited_once_with("open missing")
