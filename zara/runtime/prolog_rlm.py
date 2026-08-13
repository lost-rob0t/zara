"""Experimental Prolog-RLM runtime backend.

Each Zara turn owns an isolated SWI-Prolog sidecar process. This deliberately
keeps long-running RLM work out of Zara's process-wide PySWIP lock while
preserving RuntimeHost turn correlation and cancellation.
"""

from __future__ import annotations

import ast
import asyncio
import json
import os
import time
import uuid
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Callable, Optional, Sequence

from . import bridge, events
from .backend import RuntimeBackend, RuntimeTurnResult, UnsupportedRuntimeCommand


PROLOG_RLM_REVISION = "4cdc9854a510a2d07b559e9ae34491d43d81301a"
_DEFAULT_MODEL = "openrouter/free"
_MAX_PROTOCOL_LINE = 1_048_576
_MAX_CALCULATOR_EXPRESSION = 128
_MAX_CALCULATOR_NODES = 64
_MAX_CALCULATOR_DEPTH = 12
_MAX_CALCULATOR_EXPONENT = 1000
_MAX_TOOL_OUTPUT_BYTES = 512


class PrologRLMError(RuntimeError):
    def __init__(
        self,
        message: str,
        *,
        kind: str = "runtime_error",
        details: Optional[dict[str, Any]] = None,
    ) -> None:
        super().__init__(message)
        self.kind = kind
        self.details = details or {}


@dataclass
class _ActiveRequest:
    turn_id: str
    conversation_id: Optional[str]
    request_id: str
    process: asyncio.subprocess.Process
    cancel_lock: asyncio.Lock
    cancel_sent: bool = False


CommandBuilder = Callable[[bool], Sequence[str]]


class PrologRLMBackend(RuntimeBackend):
    """RuntimeBackend adapter for the pinned Prolog-RLM supervisor."""

    def __init__(
        self,
        config=None,
        *,
        command_builder: Optional[CommandBuilder] = None,
    ) -> None:
        if config is None:
            from zara.config import get_config

            config = get_config()

        self._config = config
        section = config.get_section("prolog_rlm")
        self._mode = str(section.get("mode", "rlm"))
        if self._mode not in {"rlm", "direct"}:
            raise ValueError("prolog_rlm.mode must be 'rlm' or 'direct'")

        self._model = str(section.get("model", _DEFAULT_MODEL) or _DEFAULT_MODEL)
        self._request_timeout = self._positive_float(
            section.get("request_timeout", 45.0),
            "prolog_rlm.request_timeout",
        )
        self._cancel_grace = self._positive_float(
            section.get("cancel_grace", 1.5),
            "prolog_rlm.cancel_grace",
        )
        self._planner_max_tokens = self._positive_int(
            section.get("planner_max_tokens", 512),
            "prolog_rlm.planner_max_tokens",
        )
        self._max_recursion_depth = self._nonnegative_int(
            section.get("max_recursion_depth", 0),
            "prolog_rlm.max_recursion_depth",
        )
        self._max_model_calls = self._positive_int(
            section.get("max_model_calls", 4),
            "prolog_rlm.max_model_calls",
        )
        self._max_total_tokens = self._positive_int(
            section.get("max_total_tokens", 8192),
            "prolog_rlm.max_total_tokens",
        )
        self._max_cost_usd = self._nonnegative_float(
            section.get("max_cost_usd", 0.25),
            "prolog_rlm.max_cost_usd",
        )
        self._planner_instruction = str(
            section.get("planner_instruction", "") or self._default_planner_instruction()
        )

        self._rlm_root = str(
            section.get("root", "") or os.getenv("ZARA_PROLOG_RLM_ROOT", "")
        )
        default_sidecar = (
            Path(__file__).resolve().parents[2] / "modules" / "rlm_sidecar.pl"
        )
        self._sidecar = str(
            section.get("sidecar", "")
            or os.getenv("ZARA_RLM_SIDECAR", "")
            or default_sidecar
        )
        self._command_builder = command_builder
        self._publisher = bridge.publish
        self._active: dict[str, _ActiveRequest] = {}
        self._active_lock = asyncio.Lock()
        self._started = False

    @staticmethod
    def _positive_float(value: Any, field: str) -> float:
        if isinstance(value, bool):
            raise ValueError(f"{field} must be a positive number")
        number = float(value)
        if number <= 0:
            raise ValueError(f"{field} must be a positive number")
        return number

    @staticmethod
    def _nonnegative_float(value: Any, field: str) -> float:
        if isinstance(value, bool):
            raise ValueError(f"{field} must be a non-negative number")
        number = float(value)
        if number < 0:
            raise ValueError(f"{field} must be a non-negative number")
        return number

    @staticmethod
    def _positive_int(value: Any, field: str) -> int:
        if isinstance(value, bool) or not isinstance(value, int) or value < 1:
            raise ValueError(f"{field} must be a positive integer")
        return value

    @staticmethod
    def _nonnegative_int(value: Any, field: str) -> int:
        if isinstance(value, bool) or not isinstance(value, int) or value < 0:
            raise ValueError(f"{field} must be a non-negative integer")
        return value

    def bind_event_publisher(self, publisher) -> None:
        self._publisher = publisher

    def _publish(self, event: events.RuntimeEvent) -> None:
        self._publisher(event)

    def _default_planner_instruction(self) -> str:
        return (
            "For an ordinary conversational turn, prefer the smallest valid plan. "
            "If no calculator is needed, return a plan that calls model openrouter "
            "with the existing query variable as the prompt, binds answer, then "
            "returns answer. Use calculator only for arithmetic. Do not recurse "
            "unless the granted capabilities include rlm. Never invent tools."
        )

    def _command(self, *, probe: bool) -> tuple[str, ...]:
        if self._command_builder is not None:
            return tuple(self._command_builder(probe))
        if not self._rlm_root:
            raise PrologRLMError(
                "Prolog-RLM source is unavailable; run Zara through its Nix environment",
                kind="rlm_root_unavailable",
            )
        command = ["swipl", "-q", "-f", "none", "-s", self._sidecar, "--"]
        if probe:
            command.extend(["--probe", self._rlm_root])
        else:
            command.append(self._rlm_root)
        return tuple(command)

    async def start(self) -> None:
        if self._started:
            return

        command = self._command(probe=True)
        try:
            process = await asyncio.create_subprocess_exec(
                *command,
                stdin=asyncio.subprocess.DEVNULL,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
            )
        except FileNotFoundError as error:
            raise PrologRLMError(
                f"Prolog-RLM sidecar executable is unavailable: {command[0]}",
                kind="swi_unavailable",
            ) from error
        except OSError as error:
            raise PrologRLMError(
                f"Prolog-RLM sidecar failed to start: {error}",
                kind="sidecar_startup_failed",
            ) from error

        try:
            stdout, _stderr = await asyncio.wait_for(
                process.communicate(),
                timeout=min(self._request_timeout, 10.0),
            )
        except asyncio.TimeoutError as error:
            process.kill()
            await process.wait()
            raise PrologRLMError(
                "Prolog-RLM sidecar probe timed out",
                kind="sidecar_startup_timeout",
            ) from error

        if process.returncode != 0:
            raise PrologRLMError(
                "Prolog-RLM modules failed to load",
                kind="sidecar_startup_failed",
                details={"exit_status": process.returncode},
            )

        try:
            message = self._decode_message(stdout.strip())
        except PrologRLMError as error:
            raise PrologRLMError(
                "Prolog-RLM sidecar probe returned malformed output",
                kind="sidecar_protocol_error",
            ) from error

        if message.get("type") != "ready":
            raise PrologRLMError(
                "Prolog-RLM sidecar probe did not report ready",
                kind="sidecar_startup_failed",
            )
        if message.get("revision") != PROLOG_RLM_REVISION:
            raise PrologRLMError(
                "Prolog-RLM sidecar revision does not match Zara's pinned integration",
                kind="revision_mismatch",
                details={
                    "expected": PROLOG_RLM_REVISION,
                    "actual": message.get("revision"),
                },
            )
        self._started = True

    async def submit_turn(
        self,
        text: str,
        *,
        turn_id: str,
        conversation_id: Optional[str] = None,
        context_ids: tuple[str, ...] = (),
    ) -> RuntimeTurnResult:
        if not self._started:
            raise RuntimeError("runtime backend is not started")
        if context_ids:
            raise UnsupportedRuntimeCommand(
                "context attachments are not wired into the Prolog-RLM backend yet"
            )

        request_id = f"rlm-{uuid.uuid4().hex}"
        command = self._command(probe=False)
        try:
            process = await asyncio.create_subprocess_exec(
                *command,
                stdin=asyncio.subprocess.PIPE,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
            )
        except FileNotFoundError as error:
            raise PrologRLMError(
                f"Prolog-RLM sidecar executable is unavailable: {command[0]}",
                kind="swi_unavailable",
            ) from error
        except OSError as error:
            raise PrologRLMError(
                f"Prolog-RLM sidecar failed to start: {error}",
                kind="sidecar_startup_failed",
            ) from error

        active = _ActiveRequest(
            turn_id=turn_id,
            conversation_id=conversation_id,
            request_id=request_id,
            process=process,
            cancel_lock=asyncio.Lock(),
        )
        async with self._active_lock:
            self._active[turn_id] = active

        request = {
            "type": "invoke",
            "request_id": request_id,
            "turn_id": turn_id,
            "conversation_id": conversation_id,
            "query": text,
            "context": "",
            "options": {
                "mode": self._mode,
                "model": self._model,
                "planner_instruction": self._planner_instruction,
                "planner_max_tokens": self._planner_max_tokens,
                "max_recursion_depth": self._max_recursion_depth,
                "max_model_calls": self._max_model_calls,
                "max_total_tokens": self._max_total_tokens,
                "max_cost_usd": self._max_cost_usd,
                "time_limit": self._request_timeout,
            },
        }

        try:
            await self._write_message(active, request)
            return await self._consume_request(active)
        except asyncio.CancelledError:
            await asyncio.shield(self._cancel_active(active))
            raise
        finally:
            await self._cleanup_active(active)

    async def _consume_request(self, active: _ActiveRequest) -> RuntimeTurnResult:
        if active.process.stdout is None:
            raise PrologRLMError(
                "Prolog-RLM sidecar stdout is unavailable",
                kind="sidecar_protocol_error",
            )

        tool_results: list[dict[str, Any]] = []
        deadline = time.monotonic() + self._request_timeout
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                await self._cancel_active(active)
                raise PrologRLMError(
                    "Prolog-RLM request timed out",
                    kind="timeout",
                )

            try:
                line = await asyncio.wait_for(
                    active.process.stdout.readline(),
                    timeout=remaining,
                )
            except asyncio.TimeoutError as error:
                await self._cancel_active(active)
                raise PrologRLMError(
                    "Prolog-RLM request timed out",
                    kind="timeout",
                ) from error

            if not line:
                returncode = await active.process.wait()
                raise PrologRLMError(
                    f"Prolog-RLM sidecar exited before completing the request ({returncode})",
                    kind="sidecar_crash",
                    details={"exit_status": returncode},
                )
            if len(line) > _MAX_PROTOCOL_LINE:
                await self._cancel_active(active)
                raise PrologRLMError(
                    "Prolog-RLM sidecar emitted an oversized protocol message",
                    kind="sidecar_protocol_error",
                )

            message = self._decode_message(line)
            message_request_id = message.get("request_id")
            if message_request_id not in {None, active.request_id}:
                raise PrologRLMError(
                    "Prolog-RLM sidecar response correlation mismatch",
                    kind="request_correlation_error",
                )

            message_type = message.get("type")
            if message_type in {"started", "model_started", "model_completed"}:
                continue
            if message_type == "tool_call":
                tool_results.append(await self._handle_tool_call(active, message))
                continue
            if message_type == "completed":
                result = message.get("result")
                if not isinstance(result, dict):
                    raise PrologRLMError(
                        "Prolog-RLM completed without a structured result",
                        kind="sidecar_protocol_error",
                    )
                text = result.get("text")
                if not isinstance(text, str):
                    raise PrologRLMError(
                        "Prolog-RLM completed without response text",
                        kind="sidecar_protocol_error",
                    )
                await self._wait_for_exit(active)
                return RuntimeTurnResult(
                    response=text,
                    tool_results=tuple(tool_results),
                    metadata=result,
                )
            if message_type == "failed":
                await self._wait_for_exit(active)
                error = message.get("error")
                if not isinstance(error, dict):
                    error = {"kind": "rlm_error", "message": "Prolog-RLM request failed"}
                raise PrologRLMError(
                    str(error.get("message") or "Prolog-RLM request failed"),
                    kind=str(error.get("kind") or "rlm_error"),
                    details=error,
                )
            if message_type == "cancelled":
                await self._wait_for_exit(active)
                raise asyncio.CancelledError

            raise PrologRLMError(
                f"Unknown Prolog-RLM sidecar message type: {message_type!r}",
                kind="sidecar_protocol_error",
            )

    async def _handle_tool_call(
        self,
        active: _ActiveRequest,
        message: dict[str, Any],
    ) -> dict[str, Any]:
        tool_call_id = message.get("tool_call_id")
        tool_name = message.get("tool")
        args = message.get("args")
        if not isinstance(tool_call_id, str) or not tool_call_id:
            raise PrologRLMError(
                "Prolog-RLM tool call is missing tool_call_id",
                kind="sidecar_protocol_error",
            )

        self._publish(
            events.ToolStarted(
                turn_id=active.turn_id,
                conversation_id=active.conversation_id,
                label="prolog-rlm",
                tool_run_id=tool_call_id,
                tool_name=str(tool_name) if tool_name is not None else None,
            )
        )

        if tool_name != "calculator":
            error = {
                "kind": "capability_denied",
                "message": "tool is not exposed by the Zara Prolog-RLM bridge",
            }
            await self._write_message(
                active,
                {
                    "type": "tool_result",
                    "request_id": active.request_id,
                    "tool_call_id": tool_call_id,
                    "status": "error",
                    "error": error,
                },
            )
            self._publish_tool_failed(active, tool_call_id, tool_name, error["message"])
            return {
                "tool_run_id": tool_call_id,
                "tool_name": tool_name,
                "success": False,
                "error": error,
            }

        try:
            result = self._invoke_calculator(args)
        except ValueError as error:
            structured = {"kind": "invalid_arguments", "message": str(error)}
            await self._write_message(
                active,
                {
                    "type": "tool_result",
                    "request_id": active.request_id,
                    "tool_call_id": tool_call_id,
                    "status": "error",
                    "error": structured,
                },
            )
            self._publish_tool_failed(active, tool_call_id, tool_name, str(error))
            return {
                "tool_run_id": tool_call_id,
                "tool_name": tool_name,
                "success": False,
                "error": structured,
            }

        await self._write_message(
            active,
            {
                "type": "tool_result",
                "request_id": active.request_id,
                "tool_call_id": tool_call_id,
                "status": "ok",
                "value": result,
            },
        )
        self._publish(
            events.ToolCompleted(
                turn_id=active.turn_id,
                conversation_id=active.conversation_id,
                label="prolog-rlm",
                tool_run_id=tool_call_id,
                tool_name=tool_name,
                success=True,
            )
        )
        return {
            "tool_run_id": tool_call_id,
            "tool_name": tool_name,
            "success": True,
            "result": result,
        }

    def _publish_tool_failed(
        self,
        active: _ActiveRequest,
        tool_call_id: str,
        tool_name: Any,
        reason: str,
    ) -> None:
        self._publish(
            events.ToolFailed(
                turn_id=active.turn_id,
                conversation_id=active.conversation_id,
                label="prolog-rlm",
                tool_run_id=tool_call_id,
                tool_name=str(tool_name) if tool_name is not None else None,
                reason=reason,
            )
        )

    def _invoke_calculator(self, args: Any) -> str:
        if not isinstance(args, dict):
            raise ValueError("calculator arguments must be an object")
        if set(args) != {"expression"}:
            raise ValueError("calculator requires exactly one expression argument")
        expression = args.get("expression")
        if not isinstance(expression, str) or not expression:
            raise ValueError("calculator expression must be a non-empty string")
        if len(expression) > _MAX_CALCULATOR_EXPRESSION:
            raise ValueError("calculator expression is too long")

        try:
            tree = ast.parse(expression, mode="eval")
        except SyntaxError as error:
            raise ValueError("calculator expression is invalid") from error
        nodes = list(ast.walk(tree))
        if len(nodes) > _MAX_CALCULATOR_NODES:
            raise ValueError("calculator expression is too complex")
        self._validate_calculator_ast(tree.body, depth=0)

        from zara.agent.tools.builtin_tools import calculator

        result = str(calculator.invoke({"expression": expression}))
        if len(result.encode("utf-8")) > _MAX_TOOL_OUTPUT_BYTES:
            raise ValueError("calculator output is too large")
        return result

    def _validate_calculator_ast(self, node: ast.AST, *, depth: int) -> None:
        if depth > _MAX_CALCULATOR_DEPTH:
            raise ValueError("calculator expression is too deeply nested")
        if isinstance(node, ast.Constant):
            if isinstance(node.value, bool) or not isinstance(node.value, (int, float)):
                raise ValueError("calculator constants must be numeric")
            return
        if isinstance(node, ast.UnaryOp) and isinstance(node.op, (ast.UAdd, ast.USub)):
            self._validate_calculator_ast(node.operand, depth=depth + 1)
            return
        if isinstance(
            node,
            ast.BinOp,
        ) and isinstance(
            node.op,
            (ast.Add, ast.Sub, ast.Mult, ast.Div, ast.FloorDiv, ast.Mod, ast.Pow),
        ):
            if isinstance(node.op, ast.Pow):
                exponent = node.right
                if (
                    not isinstance(exponent, ast.Constant)
                    or isinstance(exponent.value, bool)
                    or not isinstance(exponent.value, (int, float))
                    or abs(exponent.value) > _MAX_CALCULATOR_EXPONENT
                ):
                    raise ValueError("calculator exponent is outside the allowed bound")
            self._validate_calculator_ast(node.left, depth=depth + 1)
            self._validate_calculator_ast(node.right, depth=depth + 1)
            return
        raise ValueError(f"calculator expression uses unsupported syntax: {type(node).__name__}")

    async def cancel_turn(self, turn_id: str) -> None:
        async with self._active_lock:
            active = self._active.get(turn_id)
        if active is not None:
            await self._cancel_active(active)

    async def _cancel_active(self, active: _ActiveRequest) -> None:
        async with active.cancel_lock:
            process = active.process
            if process.returncode is not None:
                return
            if not active.cancel_sent:
                active.cancel_sent = True
                try:
                    await self._write_message(
                        active,
                        {
                            "type": "cancel",
                            "request_id": active.request_id,
                        },
                    )
                except (BrokenPipeError, ConnectionResetError, PrologRLMError):
                    pass

            try:
                await asyncio.wait_for(process.wait(), timeout=self._cancel_grace)
                return
            except asyncio.TimeoutError:
                process.terminate()

            try:
                await asyncio.wait_for(process.wait(), timeout=self._cancel_grace)
            except asyncio.TimeoutError:
                process.kill()
                await process.wait()

    async def _wait_for_exit(self, active: _ActiveRequest) -> None:
        process = active.process
        if process.returncode is not None:
            return
        try:
            await asyncio.wait_for(process.wait(), timeout=self._cancel_grace)
        except asyncio.TimeoutError:
            process.terminate()
            try:
                await asyncio.wait_for(process.wait(), timeout=self._cancel_grace)
            except asyncio.TimeoutError:
                process.kill()
                await process.wait()

    async def _cleanup_active(self, active: _ActiveRequest) -> None:
        if active.process.returncode is None:
            await self._cancel_active(active)
        async with self._active_lock:
            current = self._active.get(active.turn_id)
            if current is active:
                self._active.pop(active.turn_id, None)

    async def stop(self) -> None:
        async with self._active_lock:
            active = tuple(self._active.values())
        if active:
            await asyncio.gather(
                *(self._cancel_active(request) for request in active),
                return_exceptions=True,
            )
        self._started = False

    async def _write_message(
        self,
        active: _ActiveRequest,
        message: dict[str, Any],
    ) -> None:
        stdin = active.process.stdin
        if stdin is None or stdin.is_closing():
            raise PrologRLMError(
                "Prolog-RLM sidecar stdin is unavailable",
                kind="sidecar_protocol_error",
            )
        encoded = json.dumps(
            message,
            ensure_ascii=False,
            separators=(",", ":"),
        ).encode("utf-8") + b"\n"
        if len(encoded) > _MAX_PROTOCOL_LINE:
            raise PrologRLMError(
                "Prolog-RLM protocol request is too large",
                kind="sidecar_protocol_error",
            )
        stdin.write(encoded)
        await stdin.drain()

    @staticmethod
    def _decode_message(data: bytes) -> dict[str, Any]:
        try:
            decoded = json.loads(data)
        except (UnicodeDecodeError, json.JSONDecodeError) as error:
            raise PrologRLMError(
                "Prolog-RLM sidecar emitted malformed JSON",
                kind="sidecar_protocol_error",
            ) from error
        if not isinstance(decoded, dict):
            raise PrologRLMError(
                "Prolog-RLM sidecar message must be a JSON object",
                kind="sidecar_protocol_error",
            )
        return decoded


__all__ = [
    "PROLOG_RLM_REVISION",
    "PrologRLMBackend",
    "PrologRLMError",
]
