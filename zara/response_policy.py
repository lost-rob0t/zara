"""Bundled Prolog output-policy plugin; matching and policy live in Prolog."""
from __future__ import annotations

import asyncio
import json
import logging
import math
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)


class PolicyError(ValueError):
    pass


def visible_text(content: Any) -> str:
    if isinstance(content, str):
        return content
    if isinstance(content, list):
        return "".join(
            block["text"] for block in content
            if isinstance(block, dict) and block.get("type") == "text"
            and isinstance(block.get("text"), str)
        )
    return ""


def _bounded_integer(settings: dict, key: str, minimum: int, maximum: int) -> int:
    value = settings.get(key)
    if isinstance(value, bool) or not isinstance(value, int) or not minimum <= value <= maximum:
        raise PolicyError(f"{key} must be an integer from {minimum} to {maximum}")
    return value


def revision_advice(report: dict) -> str:
    if report.get("status") != "ok":
        return ""
    advice = [
        str(finding["advice"]) for finding in report.get("findings", [])
        if finding.get("repair") is True and finding.get("advice")
    ]
    if not advice:
        return ""
    header = (
        "Review the previous draft for these suspected issues. These are heuristic "
        "candidates, not established errors. Use the actual conversation and tool "
        "evidence. Keep supported claims. Correct only actual errors. Preserve "
        "justified safety refusals, honest uncertainty, and genuine capability limits. "
        "Never fabricate completed work, citations, tests, or tool access. Do not "
        "execute or request tools. Return only the final user-facing answer.\n"
    )
    return header + "\n".join(dict.fromkeys(advice))[:3500]


class PrologPolicy:
    def __init__(self, engine: Any, settings: dict) -> None:
        self.engine = engine
        self.settings = dict(settings)
        if not isinstance(settings.get("enabled"), bool):
            raise PolicyError("enabled must be boolean")
        if settings.get("mode") not in {"observe", "advise"}:
            raise PolicyError("mode must be observe or advise")
        self.max_input_chars = _bounded_integer(settings, "max_input_chars", 1, 262144)
        self.max_findings = _bounded_integer(settings, "max_findings", 1, 64)
        self.max_revisions = _bounded_integer(settings, "max_revisions", 0, 1)
        timeout = settings.get("revision_timeout_seconds")
        if isinstance(timeout, bool) or not isinstance(timeout, (int, float)):
            raise PolicyError("revision_timeout_seconds must be numeric")
        if not math.isfinite(timeout) or not 0 < timeout <= 120:
            raise PolicyError("revision_timeout_seconds must be greater than zero and at most 120")
        self.revision_timeout = float(timeout)

    def inspect(self, text: str) -> dict:
        if not isinstance(text, str):
            raise TypeError("policy text must be a string")
        if not self.settings["enabled"]:
            return {"status": "disabled", "findings": []}
        if len(text) > self.max_input_chars:
            return {"status": "input_limit", "findings": [], "truncated": True}
        try:
            quoted = json.dumps(text, ensure_ascii=False)
            row = self.engine.query_once(f"response_policy:analyze_json({quoted}, Json)")
            report = json.loads(row["Json"])
            self._validate_report(report)
            return report
        except Exception as error:
            logger.warning("[ResponsePolicy] Inspection unavailable (%s)", type(error).__name__)
            return {"status": "inspection_failed", "findings": []}

    def _validate_report(self, report: Any) -> None:
        if not isinstance(report, dict):
            raise PolicyError("policy result must be an object")
        if report.get("status") not in {"ok", "disabled", "input_limit", "budget_exceeded", "inspection_failed"}:
            raise PolicyError("invalid policy result status")
        findings = report.get("findings")
        if not isinstance(findings, list) or len(findings) > self.max_findings:
            raise PolicyError("invalid or excessive policy findings")
        for finding in findings:
            if not isinstance(finding, dict):
                raise PolicyError("invalid finding")
            for field, limit in (("id", 128), ("advice", 1024)):
                value = finding.get(field)
                if not isinstance(value, str) or not value or len(value) > limit:
                    raise PolicyError(f"invalid finding {field}")
            if finding.get("severity") not in {"info", "warning", "error"}:
                raise PolicyError("invalid finding severity")
            if not isinstance(finding.get("repair"), bool):
                raise PolicyError("invalid finding repair mode")

    def catalog(self) -> dict:
        row = self.engine.query_once("response_policy:catalog_json(Json)")
        result = json.loads(row["Json"])
        if not isinstance(result, dict) or not isinstance(result.get("rules"), list):
            raise PolicyError("invalid policy catalog")
        return result


class PolicyModel:
    """Provider facade with at most one tool-free, advisory-assisted revision.

    Advise mode buffers the draft before publishing it. This deliberately trades
    time-to-first-text for preventing an unrevised draft from reaching voice/UI.
    """
    def __init__(self, model: Any, policy: PrologPolicy, revision_model: Any = None) -> None:
        self.model = model
        self.policy = policy
        self.revision_model = model if revision_model is None else revision_model

    def __getattr__(self, name: str) -> Any:
        return getattr(self.model, name)

    def bind_tools(self, tools: list, **kwargs: Any) -> PolicyModel:
        return PolicyModel(self.model.bind_tools(tools, **kwargs), self.policy, self.revision_model)

    async def ainvoke(self, messages: list, **kwargs: Any) -> Any:
        draft = await self.model.ainvoke(messages, **kwargs)
        if getattr(draft, "tool_calls", None) or getattr(draft, "invalid_tool_calls", None):
            return draft
        report = await asyncio.to_thread(self.policy.inspect, visible_text(draft.content))
        audit = {"initial": report, "final": report, "revision_count": 0, "outcome": "observed"}
        advice = revision_advice(report)
        if self.policy.settings["mode"] != "advise" or not self.policy.max_revisions or not advice:
            return _annotate(draft, audit)

        from langchain_core.messages import HumanMessage

        prompt = [*messages, draft, HumanMessage(content=advice)]
        audit["revision_count"] = 1
        try:
            revised = await asyncio.wait_for(
                self.revision_model.ainvoke(prompt, **kwargs),
                timeout=self.policy.revision_timeout,
            )
        except asyncio.CancelledError:
            raise
        except Exception as error:
            logger.warning("[ResponsePolicy] Revision unavailable (%s)", type(error).__name__)
            audit["outcome"] = "revision_failed"
            return _annotate(draft, audit)
        if (getattr(revised, "tool_calls", None) or getattr(revised, "invalid_tool_calls", None)
                or not visible_text(revised.content).strip()):
            audit["outcome"] = "revision_rejected"
            return _annotate(draft, audit)
        audit["final"] = await asyncio.to_thread(self.policy.inspect, visible_text(revised.content))
        audit["outcome"] = "revised"
        return _annotate(revised, audit)

    async def astream(self, messages: list, **kwargs: Any):
        from langchain_core.messages import AIMessageChunk

        if self.policy.settings["mode"] == "observe" and callable(getattr(self.model, "astream", None)):
            aggregate = None
            async for chunk in self.model.astream(messages, **kwargs):
                aggregate = chunk if aggregate is None else aggregate + chunk
                yield chunk
            if aggregate is not None and not (
                getattr(aggregate, "tool_calls", None) or getattr(aggregate, "invalid_tool_calls", None)
            ):
                report = await asyncio.to_thread(self.policy.inspect, visible_text(aggregate.content))
                yield AIMessageChunk(content="", response_metadata={"zara_policy": {
                    "initial": report, "final": report, "revision_count": 0, "outcome": "observed",
                }})
            return
        result = await self.ainvoke(messages, **kwargs)
        yield AIMessageChunk(
            content=result.content,
            additional_kwargs=result.additional_kwargs,
            tool_calls=result.tool_calls,
            invalid_tool_calls=getattr(result, "invalid_tool_calls", []),
            id=result.id,
            response_metadata=result.response_metadata,
            usage_metadata=getattr(result, "usage_metadata", None),
        )


def _annotate(message: Any, audit: dict) -> Any:
    metadata = dict(getattr(message, "response_metadata", {}) or {})
    metadata["zara_policy"] = audit
    return message.model_copy(update={"response_metadata": metadata})


def install_policy(agent: Any) -> None:
    """Install the bundled plugin after the core Prolog engine is available."""
    if isinstance(agent.llm_client, PolicyModel):
        return
    engine = agent.prolog_engine
    if engine is None:
        return
    if engine.query_once("current_predicate(response_policy:settings_json/1)") is None:
        logger.warning("[ResponsePolicy] Prolog policy module is not loaded")
        return
    config_dir = getattr(agent.config, "config_dir", None)
    principal_id = getattr(agent.principal, "principal_id", "local")
    if config_dir is not None and principal_id == "local":
        extension = Path(config_dir) / "plugins" / "zara-policy" / "config.pl"
        if extension.is_file() and extension.resolve() not in getattr(engine, "loaded_files", set()):
            engine.consult(extension)
    row = engine.query_once("response_policy:settings_json(Json)")
    policy = PrologPolicy(engine, json.loads(row["Json"]))
    agent.response_policy = policy
    if not policy.settings["enabled"]:
        return
    from langchain_core.tools import StructuredTool

    def policy_check(text: str) -> str:
        """Inspect a draft using Prolog; candidates are not proof of an error."""
        return json.dumps(policy.inspect(text), ensure_ascii=False)

    def policy_catalog() -> str:
        """List the effective Prolog output-policy rules and their provenance."""
        return json.dumps(policy.catalog(), ensure_ascii=False)

    get_tools = getattr(agent.config, "get_tool_config", None)
    tools_config = get_tools() if callable(get_tools) else {}
    agent.tool_registry.register_tools([
        StructuredTool.from_function(callback)
        for callback in (policy_check, policy_catalog)
        if tools_config.get(callback.__name__, True)
    ])
    agent.llm_client = PolicyModel(agent.llm_client, policy)
