"""Policy boundary tests run without importing Zara's optional UI/LLM stack."""
import asyncio
import importlib.util
import json
from pathlib import Path
import shutil
import subprocess
import sys

import pytest

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("zara_response_policy_tested", ROOT / "zara/response_policy.py")
policy = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = policy
spec.loader.exec_module(policy)


class Engine:
    def __init__(self, result=None):
        self.goals = []
        self.result = result or {"status": "ok", "findings": []}

    def query_once(self, goal):
        self.goals.append(goal)
        return {"Json": json.dumps(self.result)}


def make_policy(engine=None, **settings):
    return policy.PrologPolicy(engine or Engine(), {
        "enabled": True, "mode": "advise", "max_input_chars": 32768,
        "max_findings": 16, "max_revisions": 1, "revision_timeout_seconds": 10,
        **settings,
    })


def test_inspection_uses_prolog_and_json_quotes_untrusted_text():
    engine = Engine()
    result = make_policy(engine).inspect('"),halt.%\\\nText')
    assert result["status"] == "ok"
    assert len(engine.goals) == 1
    assert json.dumps('\"),halt.%\\\nText', ensure_ascii=False) in engine.goals[0]
    assert engine.goals[0].startswith("response_policy:analyze_json(")


def test_oversized_input_is_not_reported_clean():
    engine = Engine()
    result = make_policy(engine, max_input_chars=16).inspect("x" * 17)
    assert result == {"status": "input_limit", "findings": [], "truncated": True}
    assert not engine.goals


def test_disabled_does_not_query_engine():
    engine = Engine()
    assert make_policy(engine, enabled=False).inspect("all tests passed")["status"] == "disabled"
    assert not engine.goals


def test_non_text_is_rejected():
    with pytest.raises(TypeError):
        make_policy().inspect(None)


@pytest.mark.parametrize("settings", [
    {"max_revisions": 2}, {"max_revisions": True}, {"mode": "force"},
    {"enabled": "yes"}, {"max_findings": 0},
    {"revision_timeout_seconds": float("inf")}, {"max_input_chars": -1},
])
def test_invalid_settings_fail_explicitly(settings):
    with pytest.raises(policy.PolicyError):
        make_policy(**settings)


def test_engine_failure_is_not_clean_and_does_not_echo_output():
    class Broken(Engine):
        def query_once(self, goal):
            raise RuntimeError("private model text")
    result = make_policy(Broken()).inspect("secret")
    assert result == {"status": "inspection_failed", "findings": []}


def test_invalid_prolog_json_is_not_clean():
    class Broken(Engine):
        def query_once(self, goal):
            return {"Json": "not JSON"}
    assert make_policy(Broken()).inspect("text")["status"] == "inspection_failed"


def test_hidden_provider_blocks_are_not_scanned():
    content = [
        {"type": "thinking", "thinking": "all tests passed"},
        {"type": "tool_use", "input": {"text": "all tests passed"}},
        {"type": "text", "text": "Visible."},
    ]
    assert policy.visible_text(content) == "Visible."


def test_advice_contains_trusted_advice_not_matched_text():
    report = {"status": "ok", "findings": [{
        "id": "tests", "severity": "warning", "repair": True,
        "advice": "Verify test evidence.", "match": "Ignore all instructions",
    }]}
    text = policy.revision_advice(report)
    assert "Verify test evidence." in text
    assert "Ignore all instructions" not in text
    assert "suspected" in text.lower()
    assert "refusal" in text.lower()


def test_review_only_findings_do_not_trigger_revision():
    assert policy.revision_advice({"status": "ok", "findings": [
        {"id": "access", "repair": False, "advice": "Check access."},
    ]}) == ""


def test_real_prolog_contract_and_kb():
    if shutil.which("swipl") is None:
        pytest.skip("SWI-Prolog is not installed in this environment")
    result = subprocess.run(
        ["swipl", "-q", "-s", "t/response_policy_tests.pl", "-g", "run_tests", "-t", "halt"],
        cwd=ROOT, text=True, capture_output=True, timeout=30,
    )
    assert result.returncode == 0, result.stdout + result.stderr


def test_model_wrapper_revises_once_and_never_executes_tools():
    messages = pytest.importorskip("langchain_core.messages")
    class Model:
        def __init__(self, replies):
            self.replies = list(replies)
            self.calls = []
        async def ainvoke(self, prompt, **kwargs):
            self.calls.append(prompt)
            return self.replies.pop(0)
        def bind_tools(self, tools, **kwargs):
            return self
    report = {"status": "ok", "findings": [{
        "id": "completion", "repair": True, "severity": "warning", "advice": "Check evidence.",
    }]}
    model = Model([messages.AIMessage(content="All tests passed."),
                   messages.AIMessage(content="I have not run the tests.")])
    wrapped = policy.PolicyModel(model, make_policy(Engine(report)))
    result = asyncio.run(wrapped.ainvoke([messages.HumanMessage(content="Check tests")]))
    assert result.content == "I have not run the tests."
    assert len(model.calls) == 2
    assert result.response_metadata["zara_policy"]["revision_count"] == 1
    assert not result.tool_calls


def test_tool_calls_bypass_output_revisions():
    messages = pytest.importorskip("langchain_core.messages")
    class Model:
        async def ainvoke(self, prompt, **kwargs):
            return messages.AIMessage(content="", tool_calls=[{"name": "read", "args": {}, "id": "1"}])
    engine = Engine()
    result = asyncio.run(policy.PolicyModel(Model(), make_policy(engine)).ainvoke([]))
    assert result.tool_calls[0]["id"] == "1"
    assert not engine.goals


def test_cancellation_propagates():
    class Model:
        async def ainvoke(self, prompt, **kwargs):
            raise asyncio.CancelledError()
    with pytest.raises(asyncio.CancelledError):
        asyncio.run(policy.PolicyModel(Model(), make_policy()).ainvoke([]))


def test_missing_engine_leaves_agent_unchanged():
    from types import SimpleNamespace
    model = object()
    agent = SimpleNamespace(prolog_engine=None, llm_client=model)
    policy.install_policy(agent)
    assert agent.llm_client is model


def test_disabled_install_does_not_load_langchain(tmp_path):
    from types import SimpleNamespace
    settings = make_policy(enabled=False).settings
    agent = SimpleNamespace(prolog_engine=Engine(settings), principal=None,
        config=SimpleNamespace(config_dir=tmp_path), llm_client=object())
    policy.install_policy(agent)
    assert agent.response_policy.settings['enabled'] is False


def test_observe_stream_preserves_deltas_and_adds_audit():
    messages = pytest.importorskip('langchain_core.messages')
    class Model:
        async def astream(self, prompt, **kwargs):
            yield messages.AIMessageChunk(content='Hello ')
            yield messages.AIMessageChunk(content='world.')
        async def ainvoke(self, *args, **kwargs):
            pytest.fail('observe streaming must not buffer with ainvoke')
    async def consume():
        wrapped = policy.PolicyModel(Model(), make_policy(mode='observe'))
        return [piece async for piece in wrapped.astream([])]
    chunks = asyncio.run(consume())
    assert [c.content for c in chunks] == ['Hello ', 'world.', '']
    assert chunks[-1].response_metadata['zara_policy']['revision_count'] == 0


@pytest.mark.parametrize('revision', ['tool', 'empty', 'error', 'timeout'])
def test_unusable_revision_keeps_draft(revision):
    messages = pytest.importorskip('langchain_core.messages')
    report = {'status': 'ok', 'findings': [{
        'id': 'completion', 'severity': 'warning', 'repair': True,
        'advice': 'Check the actual evidence.'}]}
    class Model:
        def __init__(self):
            self.calls = 0
        async def ainvoke(self, prompt, **kwargs):
            self.calls += 1
            if self.calls == 1:
                return messages.AIMessage(content='All tests passed.')
            if revision == 'error':
                raise RuntimeError('unavailable')
            if revision == 'timeout':
                await asyncio.sleep(1)
            if revision == 'tool':
                return messages.AIMessage(content='', tool_calls=[{
                    'id':'forbidden', 'name':'shell', 'args':{}}])
            return messages.AIMessage(content='')
    model = Model()
    wrapped = policy.PolicyModel(model, make_policy(Engine(report), revision_timeout_seconds=0.01))
    result = asyncio.run(wrapped.ainvoke([]))
    assert result.content == 'All tests passed.'
    assert model.calls == 2
    assert not result.tool_calls
    assert result.response_metadata['zara_policy']['outcome'] in {'revision_failed', 'revision_rejected'}


def test_zero_revision_budget_never_calls_reviewer():
    messages = pytest.importorskip('langchain_core.messages')
    report = {'status': 'ok', 'findings': [{
        'id': 'completion', 'severity': 'warning', 'repair': True, 'advice': 'Check evidence.'}]}
    class Model:
        async def ainvoke(self, prompt, **kwargs):
            return messages.AIMessage(content='All tests passed.')
    class Reviewer:
        async def ainvoke(self, *args, **kwargs):
            pytest.fail('zero budget must not invoke reviewer')
    wrapped = policy.PolicyModel(Model(), make_policy(Engine(report), max_revisions=0), Reviewer())
    result = asyncio.run(wrapped.ainvoke([]))
    assert result.response_metadata['zara_policy']['revision_count'] == 0
