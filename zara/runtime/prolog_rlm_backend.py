"""Experimental Prolog-RLM conversational backend for Zara.

This module is intentionally backend-scoped: RuntimeHost remains Zara's only
runtime owner, Zara's ToolRegistry remains effect authority, and Prolog-RLM runs
in a separate SWI-Prolog process so recursive completion cannot block Zara's
process-wide PySWIP engine.
"""

from __future__ import annotations

import asyncio
import hashlib
import json
import logging
import os
from pathlib import Path
from typing import Any, Optional

from ..agent.approval import ApprovalRequest, ToolApprovalController, valid_tool_name
from ..latency import LatencyTrace
from . import bridge as runtime_bridge
from . import events
from .backend import RuntimeBackend, RuntimeTurnResult, UnsupportedRuntimeCommand

logger = logging.getLogger(__name__)


class PrologRLMRuntimeError(RuntimeError):
    """The experimental Prolog-RLM runtime could not complete a bounded turn."""


class PrologRLMProtocolError(PrologRLMRuntimeError):
    """Prolog-RLM returned an invalid or unauthorized planner action."""


class PrologRLMSubprocessRunner:
    """Run bounded Prolog-RLM completions outside Zara's PySWIP process."""

    def __init__(self, config) -> None:
        self._config = config
        section = dict(config.get_section("prolog_rlm") or {})
        self._section = section
        self._processes: dict[str, asyncio.subprocess.Process] = {}
        self._root: Optional[Path] = None

    async def start(self) -> None:
        root_value = str(
            self._section.get("root")
            or os.getenv("ZARA_PROLOG_RLM_ROOT")
            or ""
        ).strip()
        if not root_value:
            raise PrologRLMRuntimeError(
                "Prolog-RLM root is not configured; set ZARA_PROLOG_RLM_ROOT or prolog_rlm.root"
            )
        root = Path(root_value).expanduser().resolve()
        entrypoint = root / "bin" / "prolog-rlm.pl"
        if not root.is_dir() or not entrypoint.is_file():
            raise PrologRLMRuntimeError(f"Prolog-RLM CLI is unavailable under {root}")
        self._root = root

    async def run(
        self,
        *,
        prompt: str,
        context: dict[str, Any],
        turn_id: str,
    ) -> dict[str, Any]:
        if self._root is None:
            await self.start()
        assert self._root is not None

        context_text = json.dumps(
            context,
            ensure_ascii=False,
            separators=(",", ":"),
            default=str,
        )
        context_limit = int(self._section.get("context_bytes", 8192))
        if context_limit < 1024:
            raise PrologRLMRuntimeError("prolog_rlm.context_bytes must be at least 1024")
        if len(context_text.encode("utf-8")) > context_limit:
            raise PrologRLMProtocolError(
                f"Prolog-RLM planner context exceeds configured {context_limit}-byte ceiling"
            )

        command = [
            str(self._section.get("swipl") or "swipl"),
            "-q",
            "-s",
            str(self._root / "bin" / "prolog-rlm.pl"),
            "--",
            "rlm",
            prompt,
            "--context",
            context_text,
            "--context-bytes",
            str(context_limit),
            "--max-tokens",
            str(int(self._section.get("max_tokens", 512))),
            "--max-cost",
            str(float(self._section.get("max_cost_usd", 0.25))),
            "--time-limit",
            str(float(self._section.get("time_limit", 120.0))),
            "--json",
        ]
        model = str(self._section.get("model") or "").strip()
        endpoint = str(self._section.get("endpoint") or "").strip()
        credential_env = str(
            self._section.get("credential_env") or "OPENAI_API_KEY"
        ).strip()
        reasoning_effort = str(self._section.get("reasoning_effort") or "").strip()
        if model:
            command.extend(["--model", model])
        if endpoint:
            if not model:
                raise PrologRLMRuntimeError(
                    "prolog_rlm.model is required when prolog_rlm.endpoint is set"
                )
            command.extend(["--endpoint", endpoint])
            if bool(self._section.get("no_credential", False)):
                command.append("--no-credential")
            elif credential_env:
                command.extend(["--credential-env", credential_env])
        if reasoning_effort:
            command.extend(["--reasoning-effort", reasoning_effort])

        if turn_id in self._processes:
            raise PrologRLMRuntimeError(
                f"Prolog-RLM turn {turn_id!r} already has a live subprocess"
            )

        process = await asyncio.create_subprocess_exec(
            *command,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
        )
        self._processes[turn_id] = process
        process_timeout = float(self._section.get("time_limit", 120.0)) + 5.0
        try:
            stdout, stderr = await asyncio.wait_for(
                process.communicate(), timeout=process_timeout
            )
        except asyncio.TimeoutError as error:
            await self._terminate_process(process)
            raise PrologRLMRuntimeError(
                "Prolog-RLM subprocess exceeded its wall-clock limit"
            ) from error
        except asyncio.CancelledError:
            await self._terminate_process(process)
            raise
        finally:
            self._processes.pop(turn_id, None)

        if process.returncode != 0:
            detail = stderr.decode("utf-8", "replace").strip()
            if len(detail) > 1000:
                detail = detail[-1000:]
            raise PrologRLMRuntimeError(
                f"Prolog-RLM exited with status {process.returncode}: "
                f"{detail or 'no diagnostic'}"
            )
        return self._decode_action(stdout)

    async def cancel(self, turn_id: str) -> None:
        process = self._processes.get(turn_id)
        if process is not None:
            await self._terminate_process(process)

    async def stop(self) -> None:
        for process in tuple(self._processes.values()):
            await self._terminate_process(process)
        self._processes.clear()

    @staticmethod
    async def _terminate_process(process: asyncio.subprocess.Process) -> None:
        if process.returncode is not None:
            return
        try:
            process.terminate()
        except ProcessLookupError:
            return
        try:
            await asyncio.wait_for(process.wait(), timeout=2.0)
        except asyncio.TimeoutError:
            try:
                process.kill()
            except ProcessLookupError:
                return
            await process.wait()

    @classmethod
    def _decode_action(cls, stdout: bytes) -> dict[str, Any]:
        text = stdout.decode("utf-8", "replace").strip()
        envelope = None
        candidates = [text] + list(
            reversed([line.strip() for line in text.splitlines() if line.strip()])
        )
        for candidate in candidates:
            try:
                value = json.loads(candidate)
            except json.JSONDecodeError:
                continue
            if (
                isinstance(value, dict)
                and value.get("schema") == "prolog-rlm.trace.v1"
            ):
                envelope = value
                break
        if envelope is None:
            raise PrologRLMProtocolError(
                "Prolog-RLM did not emit a trace JSON envelope"
            )
        payload = envelope.get("payload")
        output = cls._completion_text(payload)
        return cls._parse_action(output)

    @classmethod
    def _completion_text(cls, payload: Any) -> str:
        if not isinstance(payload, dict):
            raise PrologRLMProtocolError(
                "Prolog-RLM completion payload is not an object"
            )
        value = payload.get("value")
        if isinstance(value, str):
            return value
        if isinstance(value, dict):
            for key in ("text", "reasoning"):
                candidate = value.get(key)
                if isinstance(candidate, str) and candidate.strip():
                    return candidate
        raise PrologRLMProtocolError(
            "Prolog-RLM completion has no text channel"
        )

    @staticmethod
    def _parse_action(text: str) -> dict[str, Any]:
        candidate = text.strip()
        if candidate.startswith("```") and candidate.endswith("```"):
            lines = candidate.splitlines()
            if len(lines) >= 3:
                candidate = "\n".join(lines[1:-1]).strip()
        try:
            action = json.loads(candidate)
        except json.JSONDecodeError as error:
            raise PrologRLMProtocolError(
                "Prolog-RLM planner output is not valid JSON"
            ) from error
        if not isinstance(action, dict):
            raise PrologRLMProtocolError(
                "Prolog-RLM planner action must be an object"
            )
        action_type = action.get("type")
        if action_type == "final":
            response = action.get("text")
            if not isinstance(response, str) or not response.strip():
                raise PrologRLMProtocolError(
                    "final action requires non-empty text"
                )
            return {"type": "final", "text": response.strip()}
        if action_type == "tool":
            name = action.get("name")
            arguments = action.get("arguments")
            if not valid_tool_name(name):
                raise PrologRLMProtocolError("tool action has an invalid name")
            if not isinstance(arguments, dict):
                raise PrologRLMProtocolError(
                    "tool action arguments must be an object"
                )
            return {"type": "tool", "name": name, "arguments": arguments}
        raise PrologRLMProtocolError(
            "planner action type must be 'tool' or 'final'"
        )


class PrologRLMRuntimeBackend(RuntimeBackend):
    """Experimental Prolog-RLM backend behind Zara's canonical RuntimeHost."""

    _PLANNER_PROMPT = (
        "You are Zara's Prolog-RLM agentic planner. Return ONLY one compact JSON object. "
        "Never claim a tool ran unless an observation says it ran. "
        "To request a host tool use: "
        "{\"type\":\"tool\",\"name\":\"tool_name\",\"arguments\":{...}}. "
        "To answer the user: {\"type\":\"final\",\"text\":\"...\"}. "
        "Use only tools present in the supplied tool catalog."
    )

    def __init__(
        self,
        config,
        *,
        principal=None,
        prolog_engine=None,
        router=None,
        runner=None,
        tool_registry=None,
        approval_controller: Optional[ToolApprovalController] = None,
        memory_manager=None,
    ) -> None:
        self._config = config
        self._principal = principal
        self._prolog_engine = prolog_engine
        self._router = router
        self._runner = runner or PrologRLMSubprocessRunner(config)
        self._tool_registry = tool_registry
        self._approval_controller = approval_controller
        self._memory_manager = memory_manager
        self._conversation_manager = None
        self._publisher = runtime_bridge.publish
        self._started = False
        self._memory_session: Optional[str] = None
        self._history: dict[str, list[dict[str, str]]] = {}

    def bind_event_publisher(self, publisher) -> None:
        self._publisher = publisher
        if self._approval_controller is not None:
            self._approval_controller.bind_event_publisher(publisher)

    async def start(self) -> None:
        if self._started:
            return
        start_runner = getattr(self._runner, "start", None)
        if start_runner is not None:
            await start_runner()

        if self._memory_manager is None:
            from ..memory import build_memory_manager

            self._memory_manager = build_memory_manager(
                self._config.get_section("memory"),
                principal=self._principal,
            )
        if self._tool_registry is None:
            from ..agent.tools.registry import ToolRegistry

            self._tool_registry = ToolRegistry(self._prolog_engine, self._config)
            self._tool_registry.load_builtin_tools(self._memory_manager)
            for plugin_dir in self._config.get_module_search_paths():
                self._tool_registry.load_user_tools(str(plugin_dir))
        await self._tool_registry.prepare_async()

        if self._approval_controller is None:
            approval = self._config.get_section("tool_approval")
            self._approval_controller = ToolApprovalController(
                timeout_seconds=float(approval.get("timeout_seconds", 300.0)),
                max_pending=int(approval.get("max_pending", 8)),
                publisher=self._publisher,
            )
        else:
            self._approval_controller.bind_event_publisher(self._publisher)

        from ..agent.conversation import ConversationManager

        agent = self._config.get_section("agent")
        self._conversation_manager = ConversationManager(
            timeout_seconds=int(agent.get("conversation_timeout", 60)),
            principal=self._principal,
        )
        self._started = True

    async def submit_turn(
        self,
        text: str,
        *,
        turn_id: str,
        conversation_id: Optional[str] = None,
        context_ids: tuple[str, ...] = (),
        latency_trace: Optional[LatencyTrace] = None,
    ) -> RuntimeTurnResult:
        if (
            not self._started
            or self._tool_registry is None
            or self._approval_controller is None
        ):
            raise RuntimeError("runtime backend is not started")
        if context_ids:
            raise UnsupportedRuntimeCommand(
                "context attachments are not wired into the Prolog-RLM backend yet"
            )

        routed = await self._route_before_agent(
            text,
            conversation_id=conversation_id,
            latency_trace=latency_trace,
        )
        if routed is not None:
            return routed

        agent_config = self._config.get_section("agent")
        max_steps = int(agent_config.get("max_steps", 10))
        if max_steps < 1:
            raise PrologRLMRuntimeError("agent.max_steps must be at least one")

        observations: list[dict[str, Any]] = []
        tool_results: list[dict[str, Any]] = []
        for step in range(max_steps):
            context = await self._planner_context(
                text,
                conversation_id=conversation_id,
                observations=observations,
                step=step,
                max_steps=max_steps,
            )
            action = await self._runner.run(
                prompt=self._PLANNER_PROMPT,
                context=context,
                turn_id=turn_id,
            )
            action = self._validate_action(action)
            if action["type"] == "final":
                response = action["text"]
                self._publisher(
                    events.AssistantStarted(
                        turn_id=turn_id,
                        conversation_id=conversation_id,
                        label="prolog-rlm",
                    )
                )
                self._publisher(
                    events.AssistantComplete(
                        turn_id=turn_id,
                        conversation_id=conversation_id,
                        label="prolog-rlm",
                        text=response,
                        success=True,
                    )
                )
                await self._record_completed_turn(
                    text, response, conversation_id
                )
                return RuntimeTurnResult(
                    response=response,
                    tool_results=tuple(tool_results),
                    metadata={"backend": "prolog_rlm", "steps": step + 1},
                )

            result = await self._execute_tool_action(
                action,
                turn_id=turn_id,
                conversation_id=conversation_id,
                step=step,
            )
            observations.append(result)
            tool_results.append(result)

        raise PrologRLMProtocolError(
            f"Prolog-RLM reached Zara's {max_steps}-step agent ceiling without a final action"
        )

    async def _route_before_agent(
        self,
        text: str,
        *,
        conversation_id: Optional[str],
        latency_trace: Optional[LatencyTrace],
    ) -> Optional[RuntimeTurnResult]:
        manager = self._conversation_manager
        if manager is not None:
            manager.update_activity()
        if self._router is None or manager is None:
            return None
        in_conversation = bool(manager.in_conversation)
        state = "conversation" if in_conversation else "passive"
        decision = await self._router.route(
            text,
            state=state,
            latency_trace=latency_trace,
            conversation_id=conversation_id,
        )
        if decision.action == "greeting":
            manager.enter_conversation()
            self._clear_history(conversation_id)
            return RuntimeTurnResult(response=decision.response)
        if decision.action == "end_conversation":
            manager.exit_conversation()
            self._clear_history(conversation_id)
            await self._rotate_memory_session()
            return RuntimeTurnResult(response=decision.response)
        if decision.action == "respond":
            await self._record_completed_turn(
                text, decision.response, conversation_id
            )
            return RuntimeTurnResult(response=decision.response)
        if not in_conversation:
            manager.enter_conversation()
            self._clear_history(conversation_id)
        return None

    async def _planner_context(
        self,
        text: str,
        *,
        conversation_id: Optional[str],
        observations: list[dict[str, Any]],
        step: int,
        max_steps: int,
    ) -> dict[str, Any]:
        from ..agent.prompting import build_agent_system_prompt

        memory_context = await self._memory_context(text)
        history_limit = int(
            self._config.get_section("llm").get("history_limit", 20)
        )
        history = self._history.get(
            self._conversation_key(conversation_id), []
        )[-max(0, history_limit) :]
        return {
            "system_prompt": build_agent_system_prompt(self._config),
            "user_input": text,
            "tools": self._tool_catalog(),
            "observations": observations,
            "conversation": history,
            "memories": memory_context,
            "step": step + 1,
            "max_steps": max_steps,
        }

    async def _memory_context(self, text: str) -> list[str]:
        memory = self._memory_manager
        if memory is None:
            return []
        memory_config = self._config.get_section("memory")
        top_k = int(memory_config.get("top_k", 5))
        max_chars = int(memory_config.get("max_chars", 1200))
        try:
            records = await asyncio.to_thread(memory.retrieve, text, k=top_k)
        except Exception:
            logger.warning(
                "Memory retrieval failed for Prolog-RLM turn", exc_info=True
            )
            return []
        rendered: list[str] = []
        used = 0
        for record in records or []:
            value = record.get("text") if isinstance(record, dict) else str(record)
            if not value:
                continue
            value = str(value)
            remaining = max_chars - used
            if remaining <= 0:
                break
            value = value[:remaining]
            rendered.append(value)
            used += len(value)
        return rendered

    def _tool_catalog(self) -> list[dict[str, Any]]:
        assert self._tool_registry is not None
        section = self._config.get_section("prolog_rlm")
        max_tools = int(section.get("max_tools", 64))
        if max_tools < 1:
            raise PrologRLMRuntimeError("prolog_rlm.max_tools must be at least one")
        catalog = []
        for name in sorted(self._tool_registry.list_tools())[:max_tools]:
            tool = self._tool_registry.get_tool(name)
            if tool is None:
                continue
            description = str(getattr(tool, "description", ""))[:512]
            schema = self._json_safe(getattr(tool, "args", {}))
            catalog.append(
                {
                    "name": name,
                    "description": description,
                    "arguments": schema,
                    "approval_required": bool(
                        self._tool_registry.requires_approval(name)
                    ),
                }
            )
        return catalog

    @classmethod
    def _json_safe(cls, value: Any) -> Any:
        if value is None or isinstance(value, (str, int, float, bool)):
            return value
        if isinstance(value, dict):
            return {
                str(key): cls._json_safe(item) for key, item in value.items()
            }
        if isinstance(value, (list, tuple)):
            return [cls._json_safe(item) for item in value]
        return str(value)

    @staticmethod
    def _validate_action(action: Any) -> dict[str, Any]:
        if not isinstance(action, dict):
            raise PrologRLMProtocolError(
                "Prolog-RLM runner returned a non-object action"
            )
        action_type = action.get("type")
        if action_type == "final":
            text = action.get("text")
            if not isinstance(text, str) or not text.strip():
                raise PrologRLMProtocolError(
                    "final action requires non-empty text"
                )
            return {"type": "final", "text": text.strip()}
        if action_type == "tool":
            name = action.get("name")
            arguments = action.get("arguments")
            if not valid_tool_name(name):
                raise PrologRLMProtocolError("tool action has an invalid name")
            if not isinstance(arguments, dict):
                raise PrologRLMProtocolError(
                    "tool action arguments must be an object"
                )
            return {"type": "tool", "name": name, "arguments": arguments}
        raise PrologRLMProtocolError(
            "runner action type must be 'tool' or 'final'"
        )

    async def _execute_tool_action(
        self,
        action: dict[str, Any],
        *,
        turn_id: str,
        conversation_id: Optional[str],
        step: int,
    ) -> dict[str, Any]:
        assert self._tool_registry is not None
        assert self._approval_controller is not None
        name = action["name"]
        arguments = action["arguments"]
        tool = self._tool_registry.get_tool(name)
        if tool is None:
            raise PrologRLMProtocolError(
                f"Prolog-RLM requested unknown tool {name!r}"
            )

        tool_run_id = self._tool_run_id(turn_id, step, name)
        if self._tool_registry.requires_approval(name):
            resolution = await self._approval_controller.wait_for_decision(
                ApprovalRequest(
                    tool_run_id=tool_run_id,
                    tool_name=name,
                    turn_id=turn_id,
                    conversation_id=conversation_id,
                )
            )
            if resolution.decision != "approve":
                return {
                    "tool": name,
                    "arguments": arguments,
                    "result": f"tool {resolution.decision}",
                    "success": False,
                }
        else:
            self._publisher(
                events.ToolQueued(
                    turn_id=turn_id,
                    conversation_id=conversation_id,
                    label=name,
                    tool_run_id=tool_run_id,
                    tool_name=name,
                )
            )

        self._publisher(
            events.ToolStarted(
                turn_id=turn_id,
                conversation_id=conversation_id,
                label=name,
                tool_run_id=tool_run_id,
                tool_name=name,
            )
        )
        try:
            output = await asyncio.to_thread(
                self._tool_registry.execute_tool,
                name,
                **arguments,
            )
        except asyncio.CancelledError:
            self._publisher(
                events.ToolCancelled(
                    turn_id=turn_id,
                    conversation_id=conversation_id,
                    label=name,
                    tool_run_id=tool_run_id,
                    tool_name=name,
                    reason="turn cancelled",
                )
            )
            raise
        except Exception as error:
            reason = str(error)[:1000]
            self._publisher(
                events.ToolFailed(
                    turn_id=turn_id,
                    conversation_id=conversation_id,
                    label=name,
                    tool_run_id=tool_run_id,
                    tool_name=name,
                    reason=reason,
                )
            )
            return {
                "tool": name,
                "arguments": arguments,
                "result": reason,
                "success": False,
            }

        result = str(output)
        self._publisher(
            events.ToolCompleted(
                turn_id=turn_id,
                conversation_id=conversation_id,
                label=name,
                tool_run_id=tool_run_id,
                tool_name=name,
                success=True,
            )
        )
        return {
            "tool": name,
            "arguments": arguments,
            "result": result,
            "success": True,
        }

    @staticmethod
    def _tool_run_id(turn_id: str, step: int, name: str) -> str:
        payload = f"{turn_id}\0{step}\0{name}".encode("utf-8", "replace")
        digest = hashlib.blake2s(payload, digest_size=12).hexdigest()
        return f"rlm-{digest}"

    async def _record_completed_turn(
        self,
        user_text: str,
        response: str,
        conversation_id: Optional[str],
    ) -> None:
        key = self._conversation_key(conversation_id)
        history = self._history.setdefault(key, [])
        history.extend(
            [
                {"role": "user", "text": user_text},
                {"role": "assistant", "text": response},
            ]
        )
        del history[:-40]

        memory = self._memory_manager
        if memory is None:
            return
        try:
            if self._memory_session is None:
                self._memory_session = await asyncio.to_thread(
                    memory.start_session
                )
            await asyncio.to_thread(
                memory.add_message,
                self._memory_session,
                "user",
                user_text,
            )
            if response:
                await asyncio.to_thread(
                    memory.add_message,
                    self._memory_session,
                    "assistant",
                    response,
                )
        except Exception:
            logger.warning(
                "Memory persistence failed for Prolog-RLM turn", exc_info=True
            )

    async def _rotate_memory_session(self) -> None:
        memory = self._memory_manager
        if memory is None:
            return
        try:
            if self._memory_session is not None:
                await asyncio.to_thread(
                    memory.summarise_session, self._memory_session
                )
            self._memory_session = await asyncio.to_thread(memory.start_session)
        except Exception:
            logger.warning(
                "Memory session rotation failed for Prolog-RLM", exc_info=True
            )

    @staticmethod
    def _conversation_key(conversation_id: Optional[str]) -> str:
        return conversation_id or "__default__"

    def _clear_history(self, conversation_id: Optional[str]) -> None:
        self._history.pop(self._conversation_key(conversation_id), None)

    async def cancel_turn(self, turn_id: str) -> None:
        if self._approval_controller is not None:
            await self._approval_controller.cancel_turn(turn_id)
        await self._runner.cancel(turn_id)

    def register_tools(self, tools) -> None:
        if self._tool_registry is None:
            raise RuntimeError("runtime backend is not started")
        self._tool_registry.register_tools(list(tools))

    def unregister_tools(self, names) -> None:
        if self._tool_registry is not None:
            self._tool_registry.unregister_tools(list(names))

    async def approve_tool(self, tool_run_id: str) -> None:
        if self._approval_controller is None:
            raise RuntimeError("runtime backend is not started")
        await self._approval_controller.approve(tool_run_id)

    async def reject_tool(self, tool_run_id: str, reason: str = "") -> None:
        if self._approval_controller is None:
            raise RuntimeError("runtime backend is not started")
        await self._approval_controller.reject(tool_run_id, reason)

    async def stop(self) -> None:
        self._started = False
        if self._approval_controller is not None:
            await self._approval_controller.shutdown()
        if self._tool_registry is not None:
            await self._tool_registry.shutdown_async()
        await self._runner.stop()
        self._history.clear()
