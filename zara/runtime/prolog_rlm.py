"""Loopback ZARA-RUNTIME/1 client for the native Prolog-RLM sidecar."""

from __future__ import annotations

import asyncio
import http.client
import json
import re
import secrets
from typing import Optional
from urllib.parse import urlsplit

from langchain_core.tools import BaseTool

from .backend import RuntimeBackend, RuntimeTurnResult
from .events import ToolWaitingForUser
from .host_read_callback import HostReadCallbackServer
from .host_effect_callback import HostEffectCallbackServer
from .turn_context import current_turn_capability_lease


class PrologRLMRuntimeBackend(RuntimeBackend):
    def __init__(self, endpoint: str, *, principal_id: str, timeout: float = 120.0, read_tooling=None, host_token: Optional[str] = None, effect_journal=None, provider: Optional[dict] = None) -> None:
        parsed = urlsplit(endpoint)
        if parsed.scheme != "http" or parsed.hostname != "127.0.0.1" or parsed.username or parsed.password or parsed.query or parsed.fragment or parsed.path.rstrip("/") != "/zara-runtime/v1":
            raise ValueError("Prolog-RLM endpoint must use 127.0.0.1 and ZARA-RUNTIME/1 HTTP")
        if not isinstance(principal_id, str) or not principal_id:
            raise ValueError("principal_id is required")
        if timeout <= 0:
            raise ValueError("timeout must be positive")
        self._host = parsed.hostname
        self._port = parsed.port or 80
        self._principal_id = principal_id
        self._timeout = timeout
        self._provider = _validated_provider(provider)
        self._started = False
        self._history: dict[str, list[tuple[str, str]]] = {}
        self._registered_tools: dict[str, BaseTool] = {}
        self._publisher = None
        self._pending_approvals: dict[str, dict] = {}
        if read_tooling is not None and not host_token:
            raise ValueError("Prolog-RLM host token is required for file tools")
        self._host_token = host_token
        self._read_tooling = read_tooling
        self._callback = (
            HostReadCallbackServer(read_tooling, token=host_token, generation=secrets.randbits(63) or 1)
            if read_tooling is not None else None
        )
        self._generation = self._callback.generation if self._callback else None
        if effect_journal is not None:
            if not host_token:
                raise ValueError("Prolog-RLM host token is required for effects")
            if self._generation is None:
                self._generation = secrets.randbits(63) or 1
            self._effect = HostEffectCallbackServer(
                self._registered_tools, effect_journal, principal_id=principal_id,
                token=host_token, generation=self._generation,
            )
        else:
            self._effect = None
        self._available_tools: tuple[str, ...] = ("file_read",) if self._callback else ()

    @property
    def principal_id(self) -> str:
        return self._principal_id

    def register_tools(self, tools) -> None:
        staged = tuple(tools)
        if any(not isinstance(tool, BaseTool) for tool in staged):
            raise TypeError("Prolog-RLM tools must be LangChain BaseTool instances")
        names = [tool.name for tool in staged]
        if len(set(names)) != len(names) or any(name in self._registered_tools for name in names):
            raise ValueError("Prolog-RLM tool is already registered")
        self._registered_tools.update((tool.name, tool) for tool in staged)

    def bind_event_publisher(self, publisher) -> None:
        self._publisher = publisher

    def unregister_tools(self, names) -> None:
        for name in names:
            self._registered_tools.pop(name, None)

    def _request(self, route: str, payload: Optional[dict] = None) -> dict:
        body = None if payload is None else json.dumps(payload).encode("utf-8")
        connection = http.client.HTTPConnection(self._host, self._port, timeout=self._timeout)
        try:
            headers = {"Content-Type": "application/json"} if body is not None else {}
            if self._host_token:
                headers["Authorization"] = f"Bearer {self._host_token}"
            connection.request(
                "POST" if body is not None else "GET",
                f"/zara-runtime/v1/{route}",
                body=body,
                headers=headers,
            )
            response = connection.getresponse()
            if response.status != 200 or response.getheader("Content-Type", "").split(";", 1)[0].strip() != "application/json":
                raise RuntimeError("Prolog-RLM returned an invalid HTTP response")
            content = response.read(262145)
            if len(content) > 262144:
                raise RuntimeError("Prolog-RLM response exceeds size limit")
            result = json.loads(content)
        finally:
            connection.close()
        if not isinstance(result, dict):
            raise RuntimeError("Prolog-RLM returned an invalid response")
        return result

    async def start(self) -> None:
        if self._effect is not None:
            self._effect.start()
        if self._callback is not None:
            self._callback.start()
            try:
                configured = await asyncio.to_thread(self._request, "host-config", {
                    "protocol": "ZARA-RUNTIME/1",
                    "endpoint": self._callback.endpoint,
                    "generation": self._generation,
                    "tools": [{
                        "name": "file_read",
                        "description": "Read UTF-8 text in the configured repository",
                        "arguments": {"type": "object", "properties": {"path": {"type": "string"}}, "required": ["path"]},
                        "result": {"type": "object", "properties": {"content": {"type": "string"}}, "required": ["content"]},
                    }],
                })
                if configured != {"protocol": "ZARA-RUNTIME/1", "status": "configured", "generation": self._generation}:
                    raise RuntimeError("Prolog-RLM host tool configuration was rejected")
            except BaseException:
                self._callback.stop()
                if self._effect is not None:
                    self._effect.stop()
                raise
        try:
            discovery = await asyncio.to_thread(self._request, "discover")
            descriptors = discovery.get("runtimes", [])
            if not isinstance(descriptors, list) or len(descriptors) != 1:
                raise RuntimeError("Prolog-RLM discovery returned invalid runtimes")
            descriptor = descriptors[0]
            if not isinstance(descriptor, dict) or descriptor.get("id") != "prolog-rlm":
                raise RuntimeError("Prolog-RLM discovery returned an unexpected runtime")
            if descriptor.get("protocol") != "ZARA-RUNTIME/1":
                raise RuntimeError("Prolog-RLM protocol is incompatible")
            if descriptor.get("installed") is not True or descriptor.get("available") is not True or descriptor.get("health") not in {"ready", "busy"}:
                raise RuntimeError("Prolog-RLM runtime is unavailable")
            if self._callback is not None and descriptor.get("supports_host_tools") is not True:
                raise RuntimeError("Prolog-RLM host tools are unavailable")
        except BaseException:
            if self._callback is not None:
                self._callback.stop()
            if self._effect is not None:
                self._effect.stop()
            raise
        self._started = True

    async def finalize_tools(self) -> None:
        if not self._started:
            raise RuntimeError("Prolog-RLM runtime is not started")
        if not self._registered_tools:
            return
        read_tools = tuple(tool for tool in self._registered_tools.values()
                           if (tool.metadata or {}).get("zara_read_only") is True)
        effect_tools = tuple(tool for tool in self._registered_tools.values()
                             if tool.name in {"code_tasks", "code_task_cancel"})
        if not read_tools and not effect_tools:
            return
        if self._host_token is None:
            raise RuntimeError("Prolog-RLM host token is required for registered tools")
        if effect_tools and self._effect is None:
            raise RuntimeError("Prolog-RLM effect journal is required for registered effect tools")
        if read_tools:
            if self._callback is not None:
                await asyncio.to_thread(self._callback.stop)
            self._callback = HostReadCallbackServer(
                self._read_tooling, token=self._host_token,
                generation=self._generation, read_tools=read_tools,
            )
            self._callback.start()
        descriptors = []
        if self._read_tooling is not None:
            descriptors.append({
                "name": "file_read", "description": "Read UTF-8 text in the configured repository",
                "arguments": {"type": "object", "properties": {"path": {"type": "string"}}, "required": ["path"]},
                "result": {"type": "object", "properties": {"content": {"type": "string"}}, "required": ["content"]},
            })
        if self._callback is not None:
            descriptors.extend(self._callback.read_tool_descriptors)
        for name, tool in self._registered_tools.items():
            if name not in {"code_tasks", "code_task_cancel"}:
                continue
            if not (tool.metadata or {}).get("zara_requires_approval"):
                raise RuntimeError("Prolog-RLM effect tool lacks approval policy")
            schema = tool.args_schema.model_json_schema() if tool.args_schema else tool.get_input_schema().model_json_schema()
            descriptors.append({
                "name": name, "description": tool.description[:512], "effect": "process",
                "arguments": _reduced_schema(schema, schema.get("$defs", {})),
                "result": {"type": "object", "properties": {"result": {"type": "string"}}, "required": ["result"]},
            })
        configured = await asyncio.to_thread(self._request, "host-config", {
            "protocol": "ZARA-RUNTIME/1",
            "endpoint": self._callback.endpoint if self._callback else "http://127.0.0.1:1/zara-host-tool/v1/read",
            **({"effect_endpoint": self._effect.endpoint} if effect_tools else {}),
            "generation": self._generation,
            "tools": descriptors,
        })
        if configured != {"protocol": "ZARA-RUNTIME/1", "status": "configured", "generation": self._generation}:
            raise RuntimeError("Prolog-RLM effect tool configuration was rejected")
        self._available_tools = tuple(descriptor["name"] for descriptor in descriptors)

    async def submit_turn(
        self,
        text: str,
        *,
        turn_id: str,
        conversation_id: Optional[str] = None,
        context_ids: tuple[str, ...] = (),
        system_context: Optional[str] = None,
        conversation_history: Optional[list] = None,
        latency_trace=None,
    ) -> RuntimeTurnResult:
        if not self._started:
            raise RuntimeError("Prolog-RLM runtime is not started")
        if context_ids:
            raise RuntimeError("Prolog-RLM sidecar does not support context handles")
        key = conversation_id or "default"
        if conversation_history is None:
            entries = self._history.get(key, [])
        else:
            entries = []
            for entry in conversation_history:
                if not isinstance(entry, dict) or entry.get("role") not in {"user", "assistant", "system"} or not isinstance(entry.get("content"), str):
                    raise ValueError("conversation_history contains an invalid message")
                entries.append((entry["role"], entry["content"]))
        history = "\n".join(f"{role}: {content}" for role, content in entries)
        context = "\n".join(part for part in (system_context or "", history) if part)
        if len(context) > 262144:
            raise ValueError("Prolog-RLM inline context exceeds size limit")
        request = {"protocol": "ZARA-RUNTIME/1", "request_id": turn_id, "mode": "rlm", "prompt": text, "inline_context": context}
        if self._provider is not None:
            request["provider"] = self._provider
        if self._callback is not None:
            self._callback.register_turn(turn_id, current_turn_capability_lease())
        if self._available_tools:
            request["host_tools"] = list(self._available_tools)
        try:
            response = await asyncio.to_thread(self._request, "generate", request)
            while response.get("status") == "approval_required":
                response = await self._await_approval(response, turn_id, conversation_id)
        finally:
            if self._callback is not None:
                self._callback.unregister_turn(turn_id)
        if response.get("protocol") != "ZARA-RUNTIME/1" or response.get("runtime_id") != "prolog-rlm" or response.get("request_id") != turn_id:
            raise RuntimeError("Prolog-RLM response identity mismatch")
        if response.get("status") != "completed":
            error = response.get("error")
            reason = error.get("message", "runtime failed") if isinstance(error, dict) else "runtime failed"
            raise RuntimeError(f"Prolog-RLM {reason}")
        answer = response.get("text")
        if not isinstance(answer, str):
            raise RuntimeError("Prolog-RLM returned invalid text")
        metadata = {"runtime_id": "prolog-rlm"}
        if conversation_history is None:
            metadata.update({"history_turn_id": turn_id, "history_key": key, "history_input": text})
        return RuntimeTurnResult(response=answer, metadata=metadata)

    async def _await_approval(self, response: dict, turn_id: str, conversation_id: Optional[str]) -> dict:
        if response.get("protocol") != "ZARA-RUNTIME/1" or response.get("runtime_id") != "prolog-rlm" or response.get("request_id") != turn_id or response.get("generation") != self._generation:
            raise RuntimeError("Prolog-RLM approval identity mismatch")
        approvals = response.get("approvals")
        if not isinstance(approvals, list) or len(approvals) != 1 or not isinstance(approvals[0], dict):
            raise RuntimeError("Prolog-RLM returned invalid approvals")
        approval = approvals[0]
        approval_id = approval.get("id")
        name = approval.get("name")
        if (not isinstance(approval_id, str) or not approval_id
                or name not in {"code_tasks", "code_task_cancel"}
                or name not in self._registered_tools
                or approval.get("effect") not in {"write", "process"}
                or not isinstance(approval.get("args"), dict)
                or any(not isinstance(approval.get(key), str) or not approval[key]
                       for key in ("fingerprint", "attempt_id", "effect_fingerprint", "idempotency_key"))):
            raise RuntimeError("Prolog-RLM requested an unknown tool approval")
        if approval_id in self._pending_approvals:
            raise RuntimeError("Prolog-RLM duplicated an approval id")
        pending = await asyncio.to_thread(self._request, "host-pending", {
            "protocol": "ZARA-RUNTIME/1", "request_id": turn_id,
            "generation": self._generation,
        })
        if (pending.get("protocol") != "ZARA-RUNTIME/1"
                or pending.get("request_id") != turn_id
                or pending.get("generation") != self._generation
                or pending.get("status") != "approval_required"
                or approval not in pending.get("approvals", [])):
            raise RuntimeError("Prolog-RLM pending approval mismatch")
        lease = current_turn_capability_lease()
        if not lease.active or lease.turn_id != turn_id:
            raise RuntimeError("Prolog-RLM turn capability is stale")
        if self._effect is None:
            raise RuntimeError("Prolog-RLM effect callback is unavailable")
        future = asyncio.get_running_loop().create_future()
        self._pending_approvals[approval_id] = {
            "turn_id": turn_id, "generation": self._generation, "lease": lease,
            "future": future, "approval": approval,
        }
        self._effect.register_approval(turn_id, approval, lease)
        try:
            if self._publisher is not None:
                self._publisher(ToolWaitingForUser(
                    turn_id=turn_id, conversation_id=conversation_id,
                    tool_run_id=approval_id, tool_name=name,
                    prompt=f"Allow {name} to run?",
                ))
            return await future
        finally:
            self._effect.unregister_approval(approval_id)
            self._pending_approvals.pop(approval_id, None)

    async def approve_tool(self, tool_run_id: str) -> None:
        await self._decide_tool(tool_run_id, "approve")

    async def reject_tool(self, tool_run_id: str, reason: str = "") -> None:
        await self._decide_tool(tool_run_id, "deny")

    async def _decide_tool(self, tool_run_id: str, decision: str) -> None:
        pending = self._pending_approvals.get(tool_run_id)
        if pending is None or not pending["lease"].active or pending["future"].done():
            raise RuntimeError("Prolog-RLM approval is unknown or stale")
        identity = {"protocol": "ZARA-RUNTIME/1", "request_id": pending["turn_id"],
                    "generation": pending["generation"], "approval_id": tool_run_id}
        try:
            reply = await asyncio.to_thread(self._request, "host-approval", {**identity, "decision": decision})
            if any(reply.get(key) != value for key, value in identity.items()) or reply.get("status") != "accepted":
                raise RuntimeError("Prolog-RLM approval response mismatch")
            for _ in range(100):
                if not pending["lease"].active:
                    raise RuntimeError("Prolog-RLM approval became stale")
                status = await asyncio.to_thread(self._request, "host-approval-status", identity)
                if any(status.get(key) != value for key, value in identity.items()):
                    raise RuntimeError("Prolog-RLM approval status mismatch")
                if status.get("status") == "completed":
                    result = await asyncio.to_thread(self._request, "resume", identity)
                    if result.get("request_id") != pending["turn_id"] or result.get("protocol") != "ZARA-RUNTIME/1":
                        raise RuntimeError("Prolog-RLM resumed turn identity mismatch")
                    pending["future"].set_result(result)
                    return
                if status.get("status") not in {"accepted", "running", "in_progress"}:
                    raise RuntimeError("Prolog-RLM approval did not complete")
                await asyncio.sleep(0.1)
            raise RuntimeError("Prolog-RLM approval timed out")
        except BaseException as error:
            if not pending["future"].done():
                pending["future"].set_exception(error)
            raise

    def commit_turn_result(
        self,
        result: RuntimeTurnResult,
        *,
        turn_id: str,
        conversation_id: Optional[str] = None,
    ) -> None:
        metadata = result.metadata
        key = conversation_id or "default"
        if metadata.get("history_turn_id") != turn_id or metadata.get("history_key") != key:
            return
        prompt = metadata.get("history_input")
        if not isinstance(prompt, str):
            return
        if key not in self._history and len(self._history) >= 32:
            self._history.pop(next(iter(self._history)))
        entries = self._history.setdefault(key, [])
        entries.extend((("user", prompt), ("assistant", result.response)))
        while len(entries) > 16 or sum(len(content) for _, content in entries) > 32768:
            entries.pop(0)

    async def cancel_turn(self, turn_id: str) -> None:
        for pending in self._pending_approvals.values():
            if pending["turn_id"] == turn_id and not pending["future"].done():
                pending["future"].cancel()
        if self._started:
            await asyncio.to_thread(self._request, "cancel", {"request_id": turn_id})

    async def stop(self) -> None:
        self._started = False
        self._history.clear()
        self._registered_tools.clear()
        if self._callback is not None:
            await asyncio.to_thread(self._callback.stop)
        if self._effect is not None:
            await asyncio.to_thread(self._effect.stop)


def _reduced_schema(schema: dict, definitions: dict) -> dict:
    if "$ref" in schema:
        name = schema["$ref"].removeprefix("#/$defs/")
        if name not in definitions:
            raise ValueError("Prolog-RLM tool schema has an unknown reference")
        return _reduced_schema(definitions[name], definitions)
    kind = schema.get("type")
    if kind not in {"object", "array", "string", "integer", "number", "boolean"}:
        raise ValueError("Prolog-RLM tool schema uses an unsupported type")
    result = {"type": kind}
    for field in ("minLength", "maxLength", "minItems", "maxItems", "minimum", "maximum", "enum"):
        if field in schema:
            result[field] = schema[field]
    if kind == "object":
        result["properties"] = {
            name: _reduced_schema(value, definitions)
            for name, value in schema.get("properties", {}).items()
        }
        result["required"] = schema.get("required", [])
    if kind == "array":
        result["items"] = _reduced_schema(schema["items"], definitions)
    return result


def _validated_provider(provider: Optional[dict]) -> Optional[dict]:
    if provider is None:
        return None
    if not isinstance(provider, dict) or provider.get("kind") not in {"openrouter", "openai_compatible"}:
        raise ValueError("Prolog-RLM provider kind is invalid")
    kind = provider["kind"]
    allowed = {"kind", "model"} if kind == "openrouter" else {
        "kind", "endpoint", "model", "credential_env", "no_credential",
    }
    if set(provider) - allowed:
        raise ValueError("Prolog-RLM provider contains unsupported fields")
    for field in ("model", "endpoint"):
        if field in provider and (not isinstance(provider[field], str) or not provider[field] or len(provider[field]) > 512):
            raise ValueError(f"Prolog-RLM provider {field} is invalid")
    if kind == "openai_compatible":
        if "endpoint" not in provider or "model" not in provider:
            raise ValueError("Prolog-RLM provider requires endpoint and model")
        credential_env = provider.get("credential_env")
        if credential_env is not None and (not isinstance(credential_env, str)
                                           or not re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]{0,127}", credential_env)):
            raise ValueError("Prolog-RLM provider credential_env is invalid")
        if "no_credential" in provider and provider["no_credential"] is not True:
            raise ValueError("Prolog-RLM provider no_credential must be true")
        if provider.get("no_credential") is True and credential_env is not None:
            raise ValueError("Prolog-RLM provider cannot combine credential settings")
        if provider.get("no_credential") is True:
            parsed = urlsplit(provider["endpoint"])
            if parsed.scheme != "http" or parsed.hostname != "127.0.0.1":
                raise ValueError("Prolog-RLM provider without credentials must use local loopback")
    return dict(provider)
