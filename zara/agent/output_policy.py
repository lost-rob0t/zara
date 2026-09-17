"""Bounded, request-local model advice evaluated by the canonical Prolog engine."""
from __future__ import annotations

import asyncio
import copy
from dataclasses import dataclass
import json
from typing import Any, Callable, Mapping, Sequence

MAX_TEXT_CHARS = 131072
MAX_ADVICE = 32
MAX_ADVICE_CHARS = 2048
MAX_REVISIONS = 3


class OutputPolicyError(RuntimeError):
    """Policy evaluation could not safely produce a decision."""


class OutputPolicyRejected(OutputPolicyError):
    """The model did not satisfy policy within the configured revision budget."""


@dataclass(frozen=True)
class PolicyAdvice:
    rule_id: str
    priority: int
    message: str

    def __post_init__(self) -> None:
        if not isinstance(self.rule_id, str) or not 1 <= len(self.rule_id) <= 64:
            raise ValueError("policy id must contain 1 to 64 characters")
        if type(self.priority) is not int or abs(self.priority) > 100000:
            raise ValueError("policy priority must be an integer from -100000 to 100000")
        if not isinstance(self.message, str) or not 1 <= len(self.message) <= MAX_ADVICE_CHARS:
            raise ValueError("policy advice must contain 1 to 2048 characters")


def _field(message: Any, name: str, default: Any = None) -> Any:
    return message.get(name, default) if isinstance(message, dict) else getattr(message, name, default)


def public_text(message: Any) -> str:
    content = _field(message, "content", "")
    if isinstance(content, str):
        return content
    if isinstance(content, list):
        return "".join(
            block["text"] for block in content
            if isinstance(block, dict) and block.get("type") == "text"
            and isinstance(block.get("text"), str)
        )
    return ""


def _has_tool_calls(message: Any) -> bool:
    extra = _field(message, "additional_kwargs", {}) or {}
    content = _field(message, "content", "")
    return bool(
        _field(message, "tool_calls", None)
        or extra.get("tool_calls") or extra.get("function_call")
        or (isinstance(content, list) and any(
            isinstance(block, dict) and block.get("type") in {"tool_use", "tool_call", "function_call"}
            for block in content
        ))
    )


def _revision_messages(messages: Sequence[Any], draft: Any, advice: Sequence[PolicyAdvice]) -> list[Any]:
    guidance = (
        "Application output-policy advice for this revision. Revise only the answer; "
        "do not call tools or invent evidence.\n"
        + "\n".join(f"[{item.rule_id}] {item.message}" for item in advice)
    )
    result = list(messages)
    if result and _field(result[0], "role", _field(result[0], "type")) == "system":
        first = copy.deepcopy(result[0])
        content = _field(first, "content", "")
        if isinstance(content, str):
            content = content + "\n\n" + guidance
        elif isinstance(content, list):
            content = content + [{"type": "text", "text": guidance}]
        else:
            raise OutputPolicyError("unsupported system-message content")
        if isinstance(first, dict):
            first["content"] = content
        else:
            first.content = content
        result[0] = first
    else:
        result.insert(0, {"role": "system", "content": guidance})
    result.extend((draft, {
        "role": "user",
        "content": "Revise the draft according to the application output-policy advice. Return only the revised answer, without tool calls.",
    }))
    return result


class PrologPolicy:
    """Pass output and trusted host context as data, never as executable goals."""

    def __init__(self, engine: Any, context: Mapping[str, str]) -> None:
        if not callable(getattr(engine, "query_once", None)):
            raise OutputPolicyError("output policy requires the canonical Prolog engine")
        if any(not isinstance(key, str) or not isinstance(value, str) or len(value) > 256
               for key, value in context.items()) or len(context) > 16:
            raise OutputPolicyError("invalid output-policy context")
        self.engine = engine
        self.context = dict(context)

    def evaluate(self, text: str) -> tuple[PolicyAdvice, ...]:
        if not isinstance(text, str) or len(text) > MAX_TEXT_CHARS:
            raise OutputPolicyError("model output exceeds the policy evaluation limit")
        context = json.dumps(self.context, ensure_ascii=False)
        goal = (
            f"output_policy:evaluate_json({json.dumps(text, ensure_ascii=False)}, "
            f"{json.dumps(context, ensure_ascii=False)}, Payload)"
        )
        try:
            row = self.engine.query_once(goal)
            if not isinstance(row, dict) or not isinstance(row.get("Payload"), (str, bytes)):
                raise ValueError("missing policy payload")
            if len(row["Payload"]) > 262144:
                raise ValueError("oversized policy payload")
            payload = json.loads(row["Payload"])
            if not isinstance(payload, dict) or payload.get("status") != "ok":
                raise ValueError("policy evaluation failed")
            entries = payload.get("advice")
            if not isinstance(entries, list) or len(entries) > MAX_ADVICE:
                raise ValueError("invalid advice list")
            advice = tuple(PolicyAdvice(item["id"], item["priority"], item["message"]) for item in entries)
            if len({item.rule_id for item in advice}) != len(advice):
                raise ValueError("duplicate policy ids")
            return tuple(sorted(advice, key=lambda item: (item.priority, item.rule_id)))
        except Exception:
            raise OutputPolicyError("Prolog output-policy evaluation failed") from None


class AdvisedModel:
    """Model adapter that publishes only an accepted final candidate.

    The canonical graph detects astream=None and uses its buffered path.
    Tool-call messages pass through untouched on the first generation. A
    policy revision may not introduce tools; no tool or graph is rerun.
    """

    astream = None

    def __init__(
        self, model: Any, evaluate: Callable[[str], Sequence[PolicyAdvice]], *, max_revisions: int = 2,
    ) -> None:
        if type(max_revisions) is not int or not 0 <= max_revisions <= MAX_REVISIONS:
            raise ValueError("max_revisions must be an integer from 0 to 3")
        if not callable(evaluate):
            raise TypeError("policy evaluator must be callable")
        self.model = model
        self.evaluate = evaluate
        self.max_revisions = max_revisions

    def bind_tools(self, tools: Any, **kwargs: Any) -> "AdvisedModel":
        return AdvisedModel(
            self.model.bind_tools(tools, **kwargs), self.evaluate, max_revisions=self.max_revisions,
        )

    async def ainvoke(self, messages: Sequence[Any], **kwargs: Any) -> Any:
        original = list(messages)
        request = original
        for revision in range(self.max_revisions + 1):
            response = await self.model.ainvoke(request, **kwargs)
            if _has_tool_calls(response):
                if revision:
                    raise OutputPolicyRejected("output-policy revision attempted a tool call")
                return response
            text = public_text(response)
            if len(text) > MAX_TEXT_CHARS:
                raise OutputPolicyError("model output exceeds the policy evaluation limit")
            try:
                advice = tuple(await asyncio.to_thread(self.evaluate, text))
                if len(advice) > MAX_ADVICE or any(not isinstance(item, PolicyAdvice) for item in advice):
                    raise ValueError("invalid output-policy advice")
                if len({item.rule_id for item in advice}) != len(advice):
                    raise ValueError("duplicate output-policy ids")
                advice = tuple(sorted(advice, key=lambda item: (item.priority, item.rule_id)))
            except Exception:
                raise OutputPolicyError("output-policy evaluation failed; draft withheld") from None
            if not advice:
                return response
            if revision == self.max_revisions:
                raise OutputPolicyRejected("output policy was not satisfied within the revision limit")
            request = _revision_messages(original, response, advice)
        raise AssertionError("bounded policy loop exhausted unexpectedly")
