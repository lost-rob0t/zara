"""Narrow, read-only tool surface for the philosophy expert system."""

from __future__ import annotations

import json
from typing import Literal, Optional

from langchain_core.tools import StructuredTool
from pydantic import BaseModel, Field


class PhilosophyQueryArgs(BaseModel):
    mode: Literal[
        "concept",
        "philosopher",
        "position",
        "compare",
        "argument",
        "objection",
        "fallacy",
        "framework",
    ]
    subject: str = Field(..., description="Primary concept, philosopher, argument, fallacy, or goal.")
    topic: Optional[str] = Field(default=None, description="Topic used by position and compare.")
    other: Optional[str] = Field(default=None, description="Second philosopher used by compare.")


def build_philosophy_tool(prolog_engine) -> StructuredTool:
    def philosophy_query(
        mode: str,
        subject: str,
        topic: Optional[str] = None,
        other: Optional[str] = None,
    ) -> str:
        try:
            goal = _goal(mode, subject, topic, other)
            row = prolog_engine.query_once(goal)
        except Exception as error:
            return f"Philosophy query error: {error}"
        if not row:
            return "No matching philosophy KB result."
        return "\n".join(f"{key}: {value}" for key, value in row.items())

    return StructuredTool.from_function(
        func=philosophy_query,
        name="philosophy_query",
        description=(
            "Query Zara's read-only Prolog philosophy expert system. "
            "Use concept for definitions, philosopher for identity/tradition, "
            "position for one philosopher on a topic, compare for two positions, "
            "argument/objection/fallacy for argument analysis, and framework for "
            "a symbolic suggestion of a relevant philosophical framework."
        ),
        args_schema=PhilosophyQueryArgs,
    )


def _goal(mode: str, subject: str, topic: Optional[str], other: Optional[str]) -> str:
    subject_term = _string(subject)
    if mode == "concept":
        return f"philosophy_expert:concept_summary({subject_term}, Canonical, Summary)"
    if mode == "philosopher":
        return (
            "philosophy_expert:philosopher_summary("
            f"{subject_term}, Canonical, Display, School, Era)"
        )
    if mode == "position":
        if not topic:
            raise ValueError("position mode requires topic")
        return (
            "philosophy_expert:position_summary("
            f"{subject_term}, {_string(topic)}, Philosopher, Topic, Position)"
        )
    if mode == "compare":
        if not topic or not other:
            raise ValueError("compare mode requires topic and other")
        return (
            "philosophy_expert:compare_positions("
            f"{_string(topic)}, {subject_term}, {_string(other)}, LeftPosition, RightPosition)"
        )
    if mode == "argument":
        return (
            "philosophy_expert:argument_summary("
            f"{subject_term}, Name, Tradition, Premises, Conclusion)"
        )
    if mode == "objection":
        return f"philosophy_expert:objection_summary({subject_term}, Label, Summary)"
    if mode == "fallacy":
        return f"philosophy_expert:fallacy_summary({subject_term}, Name, Summary)"
    if mode == "framework":
        return f"philosophy_expert:recommend_framework({subject_term}, Framework, Why)"
    raise ValueError(f"unsupported philosophy mode: {mode}")


def _string(value: str) -> str:
    return json.dumps(value, ensure_ascii=False)
