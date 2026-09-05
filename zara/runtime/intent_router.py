"""Deterministic Prolog-first intent routing for daemon turn execution.

The wake listener historically ran this stack in-process (command gate ->
Prolog intent resolution -> execution -> LLM agent fallback). Wake now
streams utterances to the daemon, so the same deterministic routing runs
here, inside the runtime backend, before any agent turn is created. AGENTS.md
requires the wake flow to attempt Prolog resolution first and to fall back to
the LLM conversation only when Prolog fails or returns ``ask``.
"""

from __future__ import annotations

import asyncio
import logging
from dataclasses import dataclass
from typing import Callable, Optional

from .. import command_gate
from ..latency import LatencyTrace
from ..python_skills import python_skills
from ..wake_words import WAKE_WORDS, find_wake_span
from .clarification import (
    ClarificationCoordinator,
    DialogueTemplate,
    OPEN_APP_TEMPLATE,
    SCHEDULE_TODO_TEMPLATE,
    SessionCloseReason,
    TEXT_MESSAGE_TEMPLATE,
)
from .frames import (
    BoolValue,
    DateTimeValue,
    DurationValue,
    NumberValue,
    RefValue,
    TextValue,
)

logger = logging.getLogger(__name__)

PENDING_DIALOGUE_TEMPLATES: dict[str, DialogueTemplate] = {
    "open": OPEN_APP_TEMPLATE,
    "text": TEXT_MESSAGE_TEMPLATE,
    "python(schedule_todo)": SCHEDULE_TODO_TEMPLATE,
}

GREETING_RESPONSE = "Yes?"
CONVERSATION_ENDED_RESPONSE = "Conversation ended"
CLARIFICATION_FAILED_RESPONSE = "I couldn't complete that."
_REGISTERED_APP_QUERY = (
    "(kb_device_providers:app_mapping(Name, _); "
    "kb_device_providers:direct_app(Name))"
)
_REGISTERED_APP_LIMIT = 256


@dataclass(frozen=True)
class RouteDecision:
    """Outcome of deterministic routing for one utterance."""

    action: str
    response: str = ""


class PrologFirstRouter:
    """Resolve command utterances through Prolog before the agent fallback."""

    def __init__(
        self,
        prolog,
        *,
        wake_words=None,
        principal_id: str = "local",
        conversation_id: str = "voice",
        clarifications: Optional[ClarificationCoordinator] = None,
        run_blocking: Optional[Callable] = None,
    ) -> None:
        self.prolog = prolog
        self.wake_words = list(wake_words) if wake_words else list(WAKE_WORDS)
        self.principal_id = principal_id
        self.conversation_id = conversation_id
        self.clarifications = (
            clarifications if clarifications is not None else ClarificationCoordinator()
        )
        self._run_blocking = run_blocking or asyncio.to_thread

    async def route(
        self,
        text: str,
        *,
        state: str = "passive",
        latency_trace: Optional[LatencyTrace] = None,
        conversation_id: Optional[str] = None,
    ) -> RouteDecision:
        conversation = conversation_id or self.conversation_id

        def _record(event: str, **metadata) -> None:
            if latency_trace is not None:
                latency_trace.record(event, **metadata)

        stripped = self._strip_wake_span(text)
        if not stripped:
            _record("route_selected", route="greeting")
            return RouteDecision("greeting", GREETING_RESPONSE)

        clarification_reply = await self._clarification_reply(
            stripped,
            conversation,
            _record,
        )
        if clarification_reply is not None:
            return clarification_reply

        open_candidate = command_gate.open_target_candidate(stripped)
        if open_candidate is not None:
            verb, target = open_candidate
            match = await self._registered_target_match(target)
            if match.status == "rewrite" and match.canonical is not None:
                stripped = f"{verb} {match.canonical}"

        if not command_gate.looks_like_command(stripped):
            recovered = await self._recover_target_only(stripped)
            if recovered is not None:
                stripped = recovered
            else:
                _record("prolog_result", status="skipped_non_command")
                _record("route_selected", route="agent_skipped_prolog")
                return RouteDecision("delegate")

        def _try_prolog():
            return self.prolog.resolve_intent(stripped, state=state)

        try:
            result = await self._run_blocking(_try_prolog)
        except Exception as error:
            logger.warning("Prolog intent resolution failed: %s", error)
            _record("prolog_result", status="error")
            return RouteDecision("delegate")

        if result is None:
            _record("prolog_result", status="no_match")
            return RouteDecision("delegate")

        intent = result.name
        args = list(result.args) if isinstance(result.args, (list, tuple)) else [
            result.args
        ]

        if intent == "end_conversation":
            _record("prolog_result", status="resolved")
            _record("route_selected", route="prolog_stop")
            return RouteDecision("end_conversation", CONVERSATION_ENDED_RESPONSE)

        if intent == "ask":
            _record("prolog_result", status="ask")
            return RouteDecision("delegate")

        if result.kind == "pending":
            return await self._handle_pending(intent, args, conversation, _record)

        if result.kind == "python":
            def _execute_skill():
                return python_skills.execute(intent, args)

            try:
                response = await self._run_blocking(_execute_skill)
            except Exception as error:
                logger.warning("Python skill %s failed: %s", intent, error)
                return RouteDecision("delegate")
            _record("prolog_result", status="python_skill")
            _record("route_selected", route="python")
            return RouteDecision("respond", str(response))

        def _execute_intent():
            return self.prolog.execute_intent(intent, args)

        try:
            executed = await self._run_blocking(_execute_intent)
        except Exception as error:
            logger.warning("Prolog intent execution failed: %s", error)
            _record("prolog_result", status="error")
            return RouteDecision("delegate")

        if executed:
            _record("prolog_result", status="executed")
            _record("route_selected", route="prolog")
            return RouteDecision("respond", f"Executed: {intent} {args}")

        _record("prolog_result", status="execution_failed")
        return RouteDecision("delegate")

    def _strip_wake_span(self, text: str) -> str:
        raw_text = text or ""
        span = find_wake_span(raw_text, self.wake_words)
        if span is None:
            return " ".join(raw_text.split())
        start, end = span
        remainder = f"{raw_text[:start]} {raw_text[end:]}"
        return " ".join(remainder.split()).strip(" \t\r\n,.:;!?-")

    async def _exact_app_mapping(self, target: str) -> bool:
        def _lookup():
            return self.prolog.get_app_mapping(target)

        try:
            return await self._run_blocking(_lookup) is not None
        except Exception as error:
            logger.warning("Exact app target lookup failed: %s", error)
            return False

    async def _registered_target_match(
        self,
        target: str,
    ) -> command_gate.RegisteredTargetMatch:
        if await self._exact_app_mapping(target):
            return command_gate.RegisteredTargetMatch("exact", target.casefold(), 0)

        def _query_registered():
            return self.prolog.query_all(
                _REGISTERED_APP_QUERY,
                max_solutions=_REGISTERED_APP_LIMIT,
            )

        try:
            rows = await self._run_blocking(_query_registered)
        except Exception as error:
            logger.warning("Registered app target discovery failed: %s", error)
            return command_gate.RegisteredTargetMatch("no_match")

        names: list[str] = []
        for row in rows:
            if not isinstance(row, dict):
                continue
            value = row.get("Name")
            if isinstance(value, bytes):
                try:
                    value = value.decode("utf-8")
                except UnicodeDecodeError:
                    continue
            if not isinstance(value, str):
                value = getattr(value, "value", None)
            if isinstance(value, str):
                names.append(value)
        return command_gate.match_registered_target(target, names)

    async def _recover_target_only(self, text: str) -> Optional[str]:
        target = command_gate.target_only_candidate(text)
        if target is None:
            return None
        match = await self._registered_target_match(target)
        if match.status not in {"exact", "rewrite"} or match.canonical is None:
            return None
        return f"open {match.canonical}"

    async def _clarification_reply(self, text: str, conversation: str, _record):
        session = self.clarifications.session_for(self.principal_id, conversation)
        if session is None:
            return None

        if command_gate.looks_like_command(text):
            self.clarifications.cancel(
                principal=self.principal_id,
                conversation_id=conversation,
                reason=SessionCloseReason.SUPERSEDED_BY_NEW_COMMAND,
            )
            return None

        outcome = self.clarifications.submit_follow_up(
            text,
            principal=self.principal_id,
            conversation_id=conversation,
        )
        if outcome.kind == "complete":
            _record("route_selected", route="clarification")
            response = await self._execute_clarification(outcome, text, conversation)
            return RouteDecision("respond", response)
        route = "clarification_stale" if outcome.kind == "stale" else "clarification"
        _record("route_selected", route=route)
        return RouteDecision("respond", outcome.message or "")

    async def _execute_clarification(self, outcome, command_text: str, conversation: str) -> str:
        session = outcome.session
        template = session.template
        frame = outcome.frame

        args: list = []
        for name in template.arg_order_names():
            value = frame.slot_value(name)
            if value is None:
                continue
            if isinstance(value, TextValue):
                args.append(value.text)
            elif isinstance(value, RefValue):
                args.append(value.id)
            elif isinstance(value, DurationValue):
                args.append(value.seconds)
            elif isinstance(value, NumberValue):
                args.append(value.value)
            elif isinstance(value, BoolValue):
                args.append(value.value)
            elif isinstance(value, DateTimeValue):
                args.append(
                    (
                        value.year,
                        value.month,
                        value.day,
                        value.hour,
                        value.minute,
                        value.second,
                    )
                )

        if template.intent_ns == "skill":
            def _execute_skill():
                return python_skills.execute(template.intent_name, args)

            response = await self._run_blocking(_execute_skill)
        elif await self._run_blocking(
            lambda: self.prolog.execute_intent(template.intent_name, args)
        ):
            response = f"Executed: {template.intent_name} {args}"
        else:
            self.clarifications.cancel(
                principal=self.principal_id,
                conversation_id=conversation,
            )
            logger.warning(
                "Clarification execution failed: intent=%s", template.intent_name
            )
            return CLARIFICATION_FAILED_RESPONSE

        self.clarifications.finish(
            principal=self.principal_id,
            conversation_id=conversation,
        )
        return str(response)

    async def _handle_pending(self, intent: str, args: list, conversation: str, _record):
        template = PENDING_DIALOGUE_TEMPLATES.get(intent)
        if template is not None:
            opened = self.clarifications.open(
                template,
                principal=self.principal_id,
                conversation_id=conversation,
            )
            _record("prolog_result", status="pending")
            _record(
                "route_selected",
                route="clarification" if opened.kind == "opened" else "clarification_capacity",
            )
            message = opened.message if opened.kind == "capacity" else opened.question
            return RouteDecision("respond", message or "")

        required = ", ".join(str(slot) for slot in args)
        _record("prolog_result", status="pending")
        _record("route_selected", route="pending")
        return RouteDecision("respond", f"Please provide: {required}.")
