"""Dependency-free output-advice contract tests."""
import asyncio
import copy
import importlib.util
import json
from pathlib import Path
import sys
import unittest
from dataclasses import dataclass, field

MODULE_PATH = Path(__file__).parents[1] / "zara" / "agent" / "output_policy.py"
spec = importlib.util.spec_from_file_location("_output_policy_contract", MODULE_PATH)
policy = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = policy
spec.loader.exec_module(policy)


@dataclass
class Response:
    content: object
    tool_calls: list = field(default_factory=list)
    additional_kwargs: dict = field(default_factory=dict)
    response_metadata: dict = field(default_factory=lambda: {"usage": 1})


class Model:
    def __init__(self, *responses):
        self.responses = list(responses)
        self.calls = []
        self.bound = None

    async def ainvoke(self, messages, **kwargs):
        self.calls.append((copy.deepcopy(messages), kwargs))
        return self.responses.pop(0)

    def bind_tools(self, tools, **kwargs):
        self.bound = (tools, kwargs)
        return self


class Engine:
    def __init__(self, payload):
        self.payload = payload
        self.goals = []

    def query_once(self, goal):
        self.goals.append(goal)
        return {"Payload": json.dumps(self.payload)}


class OutputPolicyContractTest(unittest.IsolatedAsyncioTestCase):
    async def test_unmatched_response_is_returned_unchanged(self):
        expected = Response("done")
        model = Model(expected)
        client = policy.AdvisedModel(model, lambda text: ())
        self.assertIs(await client.ainvoke([]), expected)
        self.assertEqual(len(model.calls), 1)

    async def test_advice_revises_only_model_output_and_preserves_history(self):
        history = [{"role": "system", "content": "Original instruction."},
                   {"role": "user", "content": "Explain."}]
        original = copy.deepcopy(history)
        accepted = Response("Direct explanation.")
        model = Model(Response("As an AI model, explanation."), accepted)
        advice = policy.PolicyAdvice("no_fluff", 100, "Remove the AI preamble.")
        checks = []
        def evaluate(text):
            checks.append(text)
            return (advice,) if "AI model" in text else ()
        result = await policy.AdvisedModel(model, evaluate).ainvoke(history, config={"x": 1})
        self.assertIs(result, accepted)
        self.assertEqual(history, original)
        self.assertEqual(checks, ["As an AI model, explanation.", "Direct explanation."])
        self.assertIn("Original instruction.", model.calls[1][0][0]["content"])
        self.assertIn(advice.message, model.calls[1][0][0]["content"])
        self.assertEqual(model.calls[1][1], {"config": {"x": 1}})

    async def test_revisions_have_a_hard_limit(self):
        model = Model(*(Response("bad") for _ in range(3)))
        advice = policy.PolicyAdvice("rule", 0, "Revise.")
        client = policy.AdvisedModel(model, lambda text: (advice,), max_revisions=2)
        with self.assertRaises(policy.OutputPolicyRejected):
            await client.ainvoke([])
        self.assertEqual(len(model.calls), 3)

    async def test_zero_revisions_checks_without_retrying(self):
        model = Model(Response("bad"))
        client = policy.AdvisedModel(model, lambda text: (policy.PolicyAdvice("r", 0, "Fix."),), max_revisions=0)
        with self.assertRaises(policy.OutputPolicyRejected):
            await client.ainvoke([])
        self.assertEqual(len(model.calls), 1)

    async def test_first_pass_tool_call_is_not_rewritten_or_evaluated(self):
        expected = Response("working", tool_calls=[{"name": "read", "id": "a"}])
        def forbidden(text):
            self.fail("tool protocol must not be sent through text policy")
        model = Model(expected)
        self.assertIs(await policy.AdvisedModel(model, forbidden).ainvoke([]), expected)

    async def test_legacy_tool_call_is_preserved(self):
        expected = Response("", additional_kwargs={"function_call": {"name": "read"}})
        model = Model(expected)
        self.assertIs(await policy.AdvisedModel(model, lambda _: self.fail()).ainvoke([]), expected)

    async def test_tool_call_introduced_by_revision_is_rejected(self):
        model = Model(Response("bad"), Response("", tool_calls=[{"name": "delete"}]))
        client = policy.AdvisedModel(model, lambda _: (policy.PolicyAdvice("r", 0, "Fix."),))
        with self.assertRaises(policy.OutputPolicyRejected):
            await client.ainvoke([])
        self.assertEqual(len(model.calls), 2)

    async def test_only_public_text_blocks_are_matched(self):
        expected = Response([{"type": "thinking", "thinking": "private"},
                             {"type": "text", "text": "public"}])
        seen = []
        def evaluate(text):
            seen.append(text)
            return ()
        result = await policy.AdvisedModel(Model(expected), evaluate).ainvoke([])
        self.assertIs(result, expected)
        self.assertEqual(seen, ["public"])

    async def test_policy_enabled_client_does_not_expose_token_streaming(self):
        client = policy.AdvisedModel(Model(Response("x")), lambda _: ())
        self.assertFalse(callable(getattr(client, "astream", None)))
        bound = client.bind_tools(["tool"], tool_choice="auto")
        self.assertFalse(callable(getattr(bound, "astream", None)))
        self.assertEqual(bound.model.bound, (["tool"], {"tool_choice": "auto"}))

    async def test_policy_error_fails_without_returning_draft(self):
        model = Model(Response("secret draft"))
        def fail(text):
            raise ValueError("private policy detail")
        with self.assertRaises(policy.OutputPolicyError) as caught:
            await policy.AdvisedModel(model, fail).ainvoke([])
        self.assertNotIn("secret", str(caught.exception))
        self.assertNotIn("private policy detail", str(caught.exception))

    async def test_cancellation_is_not_converted_to_policy_failure(self):
        class Cancelled:
            async def ainvoke(self, *args, **kwargs):
                raise asyncio.CancelledError()
        with self.assertRaises(asyncio.CancelledError):
            await policy.AdvisedModel(Cancelled(), lambda _: ()).ainvoke([])

    async def test_retry_budget_is_request_local(self):
        advice = policy.PolicyAdvice("r", 0, "Fix.")
        model = Model(Response("bad"), Response("ok"), Response("bad"), Response("ok"))
        client = policy.AdvisedModel(model, lambda text: (advice,) if text == "bad" else (), max_revisions=1)
        await client.ainvoke([])
        await client.ainvoke([])
        self.assertEqual(len(model.calls), 4)

    def test_invalid_revision_settings_are_rejected(self):
        for value in (-1, 4, True, 1.5, "2"):
            with self.subTest(value=value), self.assertRaises(ValueError):
                policy.AdvisedModel(Model(), lambda _: (), max_revisions=value)

    def test_advice_validation(self):
        for args in [("", 0, "fix"), ("r", True, "fix"), ("r", 0, ""),
                     ("r", 100001, "fix"), ("r", 0, "x" * 2049)]:
            with self.subTest(args=args), self.assertRaises(ValueError):
                policy.PolicyAdvice(*args)

    def test_prolog_bridge_serializes_text_as_data_and_includes_context(self):
        engine = Engine({"status": "ok", "advice": []})
        evaluator = policy.PrologPolicy(engine, {"principal_id": "alice", "turn_id": "t1"})
        text = '\"); halt. %\\\n🎉'
        self.assertEqual(evaluator.evaluate(text), ())
        self.assertIn(json.dumps(text, ensure_ascii=False), engine.goals[0])
        self.assertTrue(engine.goals[0].startswith("output_policy:evaluate_json("))
        self.assertIn("alice", engine.goals[0])

    def test_advice_order_is_priority_then_id(self):
        engine = Engine({"status": "ok", "advice": [
            {"id": "z", "priority": 2, "message": "z"},
            {"id": "b", "priority": 1, "message": "b"},
            {"id": "a", "priority": 1, "message": "a"}]})
        result = policy.PrologPolicy(engine, {}).evaluate("x")
        self.assertEqual([item.rule_id for item in result], ["a", "b", "z"])

    def test_bad_or_missing_protocol_never_means_allow(self):
        for payload in ({}, {"status": "error"}, {"status": "ok"},
                        {"status": "ok", "advice": "oops"},
                        {"status": "ok", "advice": [{"id": "a"}]},
                        {"status": "ok", "advice": [{"id":"x", "priority":0,"message":"x"}] * 2}):
            with self.subTest(payload=payload), self.assertRaises(policy.OutputPolicyError):
                policy.PrologPolicy(Engine(payload), {}).evaluate("x")

    def test_oversized_text_is_rejected_before_prolog(self):
        engine = Engine({"status": "ok", "advice": []})
        with self.assertRaises(policy.OutputPolicyError):
            policy.PrologPolicy(engine, {}).evaluate("x" * (policy.MAX_TEXT_CHARS + 1))
        self.assertEqual(engine.goals, [])


if __name__ == "__main__":
    unittest.main()
