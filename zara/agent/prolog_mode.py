"""Native Prolog conversation backend; no model or Prolog-RLM is involved."""
from __future__ import annotations

import asyncio
import json
from typing import Any

MAX_GOAL_CHARS = 16384
MAX_SOLUTIONS = 64


class PrologModeError(RuntimeError):
    """The native Prolog request could not produce a valid result."""


def normalize_goal(text: str) -> str:
    if not isinstance(text, str) or len(text) > MAX_GOAL_CHARS:
        raise PrologModeError("enter a Prolog goal of at most 16384 characters")
    value = text.strip()
    if value.startswith("?-"):
        value = value[2:].strip()
    elif value == "/prolog" or value.startswith("/prolog "):
        value = value[len("/prolog"):].strip()
    if not value:
        raise PrologModeError("enter a Prolog goal, for example member(X, [a,b]).")
    return value


def evaluate_goal(engine: Any, text: str, principal_id: str) -> dict[str, Any]:
    if not callable(getattr(engine, "query_once", None)):
        raise PrologModeError("native mode requires the canonical Prolog engine")
    goal_text = normalize_goal(text)
    if not isinstance(principal_id, str) or not 1 <= len(principal_id) <= 256:
        raise PrologModeError("native Prolog mode requires a valid host principal")
    goal = (
        f"prolog_mode:run_json({json.dumps(goal_text, ensure_ascii=False)}, "
        f"{json.dumps(principal_id, ensure_ascii=False)}, Payload)"
    )
    try:
        row = engine.query_once(goal)
        if not isinstance(row, dict) or not isinstance(row.get("Payload"), (str, bytes)):
            raise ValueError("missing native-mode result")
        if len(row["Payload"]) > 1048576:
            raise ValueError("oversized native-mode result")
        payload = json.loads(row["Payload"])
        if not isinstance(payload, dict) or payload.get("status") not in {"ok", "false", "error"}:
            raise ValueError("invalid native-mode status")
        if payload["status"] == "error":
            if payload.get("error") not in {"permission_denied", "syntax_error", "time_limit_exceeded", "inference_limit_exceeded", "execution_failed"}:
                raise ValueError("invalid native-mode error")
            return payload
        bindings = payload.get("bindings")
        if not isinstance(bindings, list) or len(bindings) > MAX_SOLUTIONS:
            raise ValueError("invalid native-mode bindings")
        if type(payload.get("limit_reached")) is not bool:
            raise ValueError("invalid native-mode limit indicator")
        if payload["status"] == "ok" and not bindings:
            raise ValueError("successful query must have a solution")
        if payload["status"] == "false" and (bindings or payload["limit_reached"]):
            raise ValueError("failed query cannot have solutions")
        for binding in bindings:
            if not isinstance(binding, dict) or any(
                not isinstance(key, str) or not isinstance(value, str) or len(value) > 4096
                for key, value in binding.items()
            ):
                raise ValueError("invalid native-mode binding")
        return payload
    except Exception:
        raise PrologModeError("native Prolog evaluation failed; check the engine and trusted rules") from None


def render_result(payload: dict[str, Any]) -> str:
    if payload["status"] == "error":
        messages = {
            "permission_denied": "Prolog permission denied. Grant the principal or goal in trusted Prolog configuration.",
            "syntax_error": "Prolog syntax error. Enter one callable goal.",
            "time_limit_exceeded": "Prolog query exceeded the two-second execution limit.",
            "inference_limit_exceeded": "Prolog query exceeded the inference limit.",
            "execution_failed": "Prolog query failed to execute. Inspect the trusted rules locally.",
        }
        return messages[payload["error"]]
    if payload["status"] == "false":
        return "false."
    lines = [", ".join(f"{key} = {value}" for key, value in row.items()) + "." if row else "true."
             for row in payload["bindings"]]
    if payload["limit_reached"]:
        lines.append("Solution limit reached (64); narrow the query.")
    return "\n".join(lines)


async def run_prolog_conversation_loop(
    llm_client: Any, tool_registry: Any, state: dict[str, Any], **kwargs: Any,
) -> dict[str, Any]:
    from langchain_core.messages import AIMessage

    try:
        payload = await asyncio.to_thread(
            evaluate_goal, getattr(tool_registry, "prolog_engine", None),
            state.get("user_input", ""), kwargs.get("principal_id", "local"),
        )
        response = render_result(payload)
    except PrologModeError as error:
        payload = {"status": "error", "error": "engine_unavailable"}
        response = str(error)
    message = AIMessage(content=response, response_metadata={"mode": "prolog", "prolog": payload})
    publisher = kwargs.get("stream_publisher")
    if publisher is not None:
        from .stream_events import Completed
        publisher(Completed(full_text=response))
    return {
        **state, "messages": [*state.get("messages", []), message],
        "response": response, "step_count": 1, "tool_results": [], "prolog": payload,
    }
