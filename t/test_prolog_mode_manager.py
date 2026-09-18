"""First-class backend integration against the real Zara manager."""
from types import SimpleNamespace
from unittest.mock import MagicMock, patch
import pytest

pytest.importorskip("langchain_core")
from zara.agent import AgentManager
from zara.agent.prolog_mode import run_prolog_conversation_loop


def config():
    value = MagicMock()
    value.config_dir = None
    value.get_section.side_effect = lambda section: {"backend": "prolog"} if section == "agent" else {}
    value.get_hooks_config.return_value = {"enabled": False, "allow_override": False}
    value.get_module_search_paths.return_value = []
    return value


def test_native_construction_needs_neither_model_nor_embedding_client():
    with patch.object(AgentManager, "_create_llm_client", side_effect=AssertionError("model initialized")), \
         patch("zara.agent.build_memory_manager", side_effect=AssertionError("memory initialized")), \
         patch("zara.agent.ToolRegistry"):
        manager = AgentManager(config=config(), prolog_engine=MagicMock())
    registration = manager.agent_loop_registry.resolve("prolog")
    assert registration.owner == "core:prolog"
    assert registration.callback is run_prolog_conversation_loop
    assert manager.llm_client is None
    assert manager.customization_diagnostics().backend_known
    assert not manager.agent_loop_advice.allow_override


def test_lazy_registry_also_contains_native_mode():
    manager = AgentManager.__new__(AgentManager)
    assert manager._get_agent_loop_registry().resolve("prolog").callback is run_prolog_conversation_loop


@pytest.mark.asyncio
async def test_native_process_uses_same_history_and_no_override_gate():
    engine = MagicMock()
    engine.query_once.return_value = {"Payload": '{"status":"ok","bindings":[{"X":"a"}],"limit_reached":false}'}
    with patch("zara.agent.ToolRegistry") as registry, patch.object(AgentManager, "_create_llm_client", side_effect=AssertionError("model")):
        registry.return_value.prolog_engine = engine
        manager = AgentManager(config=config(), prolog_engine=engine)
    with patch.object(manager, "_build_memory_context", side_effect=AssertionError("embedding lookup")):
        result = await manager.process_async("member(X,[a]).", turn_id="native-test")
    assert result["response"] == "X = a."
    assert result["mode"] == "prolog"
    assert result["prolog"]["bindings"] == [{"X": "a"}]
    assert len(manager.conversation_manager.conversation_history) == 2
