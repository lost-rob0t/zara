"""Native-mode protocol and actual LangChain integration contracts."""
import importlib.util
import json
from pathlib import Path
import sys
import unittest
from types import SimpleNamespace

PATH = Path(__file__).parents[1] / "zara" / "agent" / "prolog_mode.py"
spec = importlib.util.spec_from_file_location("_prolog_mode_contract", PATH)
mode = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = mode
spec.loader.exec_module(mode)


class Engine:
    def __init__(self, payload):
        self.payload = payload
        self.goals = []
    def query_once(self, goal):
        self.goals.append(goal)
        return {"Payload": json.dumps(self.payload)}


class PrologModeProtocolTest(unittest.TestCase):
    def test_goal_prefixes_are_explicit(self):
        for text in ("member(X,[a,b]).", "?- member(X,[a,b]).", "/prolog member(X,[a,b])."):
            self.assertEqual(mode.normalize_goal(text), "member(X,[a,b]).")

    def test_invalid_goal_input_is_rejected(self):
        for text in (None, "", "  ", "/prolog", "?- ", "x" * 16385):
            with self.subTest(text=str(text)[:20]), self.assertRaises(mode.PrologModeError):
                mode.normalize_goal(text)

    def test_input_and_principal_are_quoted_data(self):
        engine = Engine({"status": "ok", "bindings": [{"X": "a"}], "limit_reached": False})
        result = mode.evaluate_goal(engine, 'X = "quoted".', "alice")
        self.assertEqual(result["bindings"], [{"X": "a"}])
        self.assertIn(json.dumps('X = "quoted".'), engine.goals[0])
        self.assertIn('"alice"', engine.goals[0])

    def test_false_is_not_an_engine_error(self):
        engine = Engine({"status": "false", "bindings": [], "limit_reached": False})
        self.assertEqual(mode.render_result(mode.evaluate_goal(engine, "fail", "local")), "false.")

    def test_true_and_variable_bindings_are_rendered(self):
        self.assertEqual(mode.render_result({"status": "ok", "bindings": [{}], "limit_reached": False}), "true.")
        value = {"status": "ok", "bindings": [{"X": "a"}, {"X": "b"}], "limit_reached": True}
        text = mode.render_result(value)
        self.assertIn("X = a", text)
        self.assertIn("X = b", text)
        self.assertIn("Solution limit reached", text)

    def test_error_codes_do_not_echo_source(self):
        engine = Engine({"status": "error", "error": "permission_denied"})
        result = mode.evaluate_goal(engine, "private(secret)", "alice")
        text = mode.render_result(result)
        self.assertIn("permission", text.lower())
        self.assertNotIn("secret", text)

    def test_malformed_results_fail_closed(self):
        for value in ({}, {"status":"ok"}, {"status":"ok", "bindings":"x", "limit_reached":False},
                      {"status":"ok", "bindings":[{"X":3}], "limit_reached":False},
                      {"status":"ok", "bindings":[], "limit_reached":"false"}):
            with self.subTest(value=value), self.assertRaises(mode.PrologModeError):
                mode.evaluate_goal(Engine(value), "true", "local")

    def test_no_engine_is_actionable(self):
        with self.assertRaisesRegex(mode.PrologModeError, "canonical Prolog engine"):
            mode.evaluate_goal(None, "true", "local")


try:
    import langchain_core.messages
except ImportError:
    HAVE_LANGCHAIN = False
else:
    HAVE_LANGCHAIN = True


@unittest.skipUnless(HAVE_LANGCHAIN, "requires the real LangChain runtime")
class NativeLoopTest(unittest.IsolatedAsyncioTestCase):
    async def test_native_loop_never_calls_the_model(self):
        from langchain_core.messages import HumanMessage, AIMessage
        class ForbiddenModel:
            def __getattr__(self, name):
                raise AssertionError("native mode must not touch a model")
        engine = Engine({"status":"ok", "bindings":[{"X":"a"}], "limit_reached":False})
        state = {"user_input":"member(X,[a]).", "messages":[HumanMessage(content="member(X,[a]).")], "turn_id":"t1"}
        result = await mode.run_prolog_conversation_loop(ForbiddenModel(), SimpleNamespace(prolog_engine=engine), state, principal_id="local")
        self.assertIsInstance(result["messages"][-1], AIMessage)
        self.assertEqual(result["response"], "X = a.")
        self.assertEqual(len(state["messages"]), 1)
        self.assertEqual(result["tool_results"], [])

    async def test_engine_unavailable_never_falls_back_to_model(self):
        from langchain_core.messages import HumanMessage
        class ForbiddenModel:
            def __getattr__(self, name):
                raise AssertionError("strict local mode must not fall back to a model")
        state = {"user_input":"member(X,[a]).", "messages":[HumanMessage(content="member(X,[a]).")], "turn_id":"t2"}
        result = await mode.run_prolog_conversation_loop(
            ForbiddenModel(),
            SimpleNamespace(prolog_engine=None),
            state,
            principal_id="local",
        )
        self.assertEqual(result["prolog"], {"status": "error", "error": "engine_unavailable"})
        self.assertIn("canonical Prolog engine", result["response"])
        self.assertEqual(result["tool_results"], [])


if __name__ == "__main__":
    unittest.main()
