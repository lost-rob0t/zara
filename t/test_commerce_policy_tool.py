import json

from zara.agent.tools.builtin_tools import build_commerce_policy_tool


class FakeProlog:
    VALUES = {
        "kb_config:commerce_provider(Value)": {"Value": "doordash"},
        "kb_config:commerce_confirmation(Value)": {"Value": "always"},
        "kb_config:preference_learning(Value)": {"Value": "enabled"},
        "kb_config:preference_min_observations(Value)": {"Value": 2},
        "kb_config:preference_max_patterns(Value)": {"Value": 10},
        "kb_config:preference_min_confidence(Value)": {"Value": 0.5},
    }

    def query_once(self, goal):
        return self.VALUES.get(goal)


def test_commerce_policy_tool_projects_prolog_configuration():
    tool = build_commerce_policy_tool(FakeProlog())
    result = json.loads(tool.invoke({}))

    assert result == {
        "provider": "doordash",
        "confirmation": "always",
        "preference_learning": True,
        "preference_min_observations": 2,
        "preference_max_patterns": 10,
        "preference_min_confidence": 0.5,
    }


def test_commerce_policy_tool_fails_closed_when_required_fact_is_missing():
    class Missing(FakeProlog):
        VALUES = dict(FakeProlog.VALUES)
        VALUES.pop("kb_config:commerce_confirmation(Value)")

    tool = build_commerce_policy_tool(Missing())
    result = json.loads(tool.invoke({}))

    assert result["status"] == "unavailable"
    assert result["reason"] == "missing-commerce-policy-fact"
