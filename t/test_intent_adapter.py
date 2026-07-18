from dataclasses import dataclass
from typing import Any, List
from unittest.mock import MagicMock

import pytest

from zara.prolog_engine import IntentResult, PrologEngine, adapt_intent_result
from zara.python_skills import python_skills


@dataclass
class MockCompound:
    name: str
    args: List[Any]


@pytest.mark.parametrize("skill", python_skills.list_skills())
def test_actual_pyswip_string_routes_every_python_skill(skill):
    result = adapt_intent_result({"Intent": f"python({skill})", "Args": ["value"]})

    assert result == IntentResult("python", skill, ["value"])


def test_mock_compound_routes_python_skill():
    value = MockCompound("python", ["search_todos"])

    result = adapt_intent_result({"Intent": value, "Args": ["milk"]})

    assert result == IntentResult("python", "search_todos", ["milk"])


def test_pending_compound_is_typed():
    result = adapt_intent_result({"Intent": "pending(open)", "Args": ["app"]})

    assert result == IntentResult("pending", "open", ["app"])


def test_resolver_passes_state_to_prolog():
    engine = PrologEngine.__new__(PrologEngine)
    engine.logger = MagicMock()
    engine.query_once = lambda goal: {"Intent": "end_conversation", "Args": []}

    result = engine.resolve_intent("stop", state="conversation")

    assert result == IntentResult("prolog", "end_conversation", [])
