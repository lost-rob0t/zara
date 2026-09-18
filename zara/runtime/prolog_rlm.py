"""Optional Prolog-RLM implementation of Zara's RuntimeBackend contract."""

from __future__ import annotations

import asyncio
from typing import Any, Mapping, Optional

from ..latency import LatencyTrace
from .backend import RuntimeBackend, RuntimeTurnResult, UnsupportedRuntimeCommand
from .discovery import (
    PROLOG_RLM_RUNTIME_ID,
    ZARA_RUNTIME_PROTOCOL,
    PrologRlmSidecarClient,
    RuntimeDiscoveryError,
)

_RUNTIME_ERROR_KINDS = {
    "unavailable",
    "incompatible_protocol",
    "invalid_request",
    "unauthorized",
    "capability_denied",
    "provider_error",
    "model_error",
    "context_error",
    "tool_error",
    "budget_exceeded",
    "timeout",
    "cancelled",
    "transport_error",
    "runtime_error",
}


class PrologRlmRuntimeError(RuntimeError):
    """A typed, safe Prolog-RLM failure crossing the Zara runtime boundary."""

    def __init__(self, message: str, *, kind: str = "runtime_error") -> None:
        normalized_kind = kind if kind in _RUNTIME_ERROR_KINDS else "runtime_error"
        self.kind = normalized_kind
        super().__init__(f"{normalized_kind}: {message}")


class PrologRlmRuntimeBackend(RuntimeBackend):
    """Thin Zara host adapter over an installed Prolog-RLM sidecar.

    No Python LLM client or AgentManager is constructed on this path. Python
    stays responsible for host context/plugin/platform integration while
    Prolog-RLM owns provider/model/planner/runtime behavior.
    """

    def __init__(
        self,
        config=None,
        *,
        client: PrologRlmSidecarClient | Any | None = None,
        principal_id: str = "local",
    ) -> None:
        runtime_config = {}
        if config is not None:
            getter = getattr(config, "get_runtime_config", None)
            if callable(getter):
                runtime_config = getter()
        endpoint = runtime_config.get("prolog_rlm_endpoint")
        timeout = float(runtime_config.get("request_timeout", 30.0))
        if client is None:
            kwargs: dict[str, Any] = {"timeout": timeout}
            if endpoint:
                kwargs["endpoint"] = endpoint
            client = PrologRlmSidecarClient(**kwargs)
        self._client = client
        normalized_principal = str(principal_id).strip()
        if not normalized_principal:
            raise ValueError("principal_id must be non-empty")
        self._principal_id = normalized_principal
        self._descriptor = None
        self._generation = 0
        self._publisher = None
        self._registered_tools: dict[str, object] = {}
        self._cancelled_turns: set[str] = set()

    @property
    def principal_id(self) -> str:
        return self._principal_id

    def bind_event_publisher(self, publisher) -> None:
        self._publisher = publisher

    async def start(self) -> None:
        try:
            descriptors = await asyncio.to_thread(self._client.discover)
        except RuntimeDiscoveryError:
            raise PrologRlmRuntimeError(
                "Prolog-RLM runtime discovery failed",
                kind="unavailable",
            ) from None
        descriptor = next(
            (item for item in descriptors if item.id == PROLOG_RLM_RUNTIME_ID),
            None,
        )
        if descriptor is None:
            raise PrologRlmRuntimeError(
                "Prolog-RLM is not an available installed runtime",
                kind="unavailable",
            )
        if descriptor.protocol != ZARA_RUNTIME_PROTOCOL:
            raise PrologRlmRuntimeError(
                "Prolog-RLM runtime protocol is incompatible",
                kind="incompatible_protocol",
            )
        if not descriptor.selectable:
            raise PrologRlmRuntimeError(
                "Prolog-RLM is not an available installed runtime",
                kind="unavailable",
            )
        self._generation += 1
        self._cancelled_turns.clear()
        self._descriptor = descriptor

    async def submit_turn(
        self,
        text: str,
        *,
        turn_id: str,
        conversation_id: Optional[str] = None,
        context_ids: tuple[str, ...] = (),
        system_context: Optional[str] = None,
        conversation_history: Optional[list] = None,
        latency_trace: Optional[LatencyTrace] = None,
    ) -> RuntimeTurnResult:
        del latency_trace
        descriptor = self._require_started()
        runtime_generation = self._generation
        if context_ids and not descriptor.supports_context_handles:
            raise UnsupportedRuntimeCommand(
                "selected Prolog-RLM runtime does not support Zara context handles"
            )

        messages = _normalized_messages(conversation_history or ())
        messages.append({"role": "user", "content": _bounded_text(text, 131072, "turn text")})

        request_id = _bounded_id(turn_id, "turn_id")
        if request_id in self._cancelled_turns:
            raise PrologRlmRuntimeError(
                "Prolog-RLM turn was cancelled",
                kind="cancelled",
            )
        request: dict[str, Any] = {
            "protocol": ZARA_RUNTIME_PROTOCOL,
            "request_id": request_id,
            "mode": "rlm",
            "messages": messages,
            "metadata": {
                "principal_id": self._principal_id,
            },
        }
        if conversation_id:
            request["metadata"]["conversation_id"] = _bounded_id(
                conversation_id,
                "conversation_id",
            )
        if context_ids:
            request["context_handles"] = _bounded_context_handles(context_ids)
        if system_context:
            request["inline_context"] = _bounded_text(
                system_context,
                262144,
                "system context",
            )

        try:
            reply = await asyncio.to_thread(self._client.generate, request)
        except RuntimeDiscoveryError:
            raise PrologRlmRuntimeError(
                "Prolog-RLM runtime transport failed",
                kind="transport_error",
            ) from None
        if runtime_generation != self._generation or self._descriptor is not descriptor:
            raise PrologRlmRuntimeError("Prolog-RLM runtime generation changed")
        if request_id in self._cancelled_turns:
            self._cancelled_turns.discard(request_id)
            raise PrologRlmRuntimeError(
                "Prolog-RLM turn was cancelled",
                kind="cancelled",
            )
        return _turn_result_from_reply(reply, request_id=request_id)

    async def cancel_turn(self, turn_id: str) -> None:
        self._require_started()
        request_id = _bounded_id(turn_id, "turn_id")
        self._cancelled_turns.add(request_id)
        try:
            await asyncio.to_thread(
                self._client.cancel,
                request_id,
            )
        except RuntimeDiscoveryError:
            raise PrologRlmRuntimeError(
                "Prolog-RLM cancellation transport failed",
                kind="transport_error",
            ) from None

    def register_tools(self, tools) -> None:
        """Retain host-owned plugin registrations without granting them to RLM.

        ZARA-RUNTIME/1 host-tool projection is a separate capability. Until the
        discovered runtime advertises supports_host_tools, registrations remain
        host-owned and cannot be invoked by Prolog-RLM.
        """

        for tool in tools:
            name = getattr(tool, "name", None)
            if not isinstance(name, str) or not name.strip():
                raise ValueError("registered runtime tools require a bounded name")
            normalized = name.strip()
            if len(normalized) > 128:
                raise ValueError("registered runtime tool name is too long")
            self._registered_tools[normalized] = tool

    def unregister_tools(self, names) -> None:
        for name in names:
            self._registered_tools.pop(str(name), None)

    def customization_diagnostics(self):
        descriptor = self._require_started()
        return {
            "runtime_id": descriptor.id,
            "protocol": descriptor.protocol,
            "profiles": descriptor.profiles,
            "supports_host_tools": descriptor.supports_host_tools,
        }

    async def approve_tool(self, tool_run_id: str) -> None:
        raise UnsupportedRuntimeCommand(
            "Prolog-RLM host-tool approvals are unavailable until host-tool capability is negotiated"
        )

    async def reject_tool(self, tool_run_id: str, reason: str = "") -> None:
        del tool_run_id, reason
        raise UnsupportedRuntimeCommand(
            "Prolog-RLM host-tool approvals are unavailable until host-tool capability is negotiated"
        )

    async def stop(self) -> None:
        self._generation += 1
        self._descriptor = None
        self._registered_tools.clear()
        self._cancelled_turns.clear()

    def _require_started(self):
        if self._descriptor is None:
            raise PrologRlmRuntimeError("Prolog-RLM runtime backend is not started")
        return self._descriptor


def _normalized_messages(messages) -> list[dict[str, str]]:
    normalized: list[dict[str, str]] = []
    if len(messages) > 512:
        raise ValueError("conversation history exceeds runtime bound")
    for message in messages:
        role = _message_role(message)
        if role is None:
            continue
        content = getattr(message, "content", None)
        if content is None and isinstance(message, Mapping):
            content = message.get("content")
        if not isinstance(content, str):
            continue
        normalized.append(
            {
                "role": role,
                "content": _bounded_text(content, 65536, "message content"),
            }
        )
    return normalized


def _message_role(message) -> str | None:
    role = getattr(message, "type", None)
    if role is None and isinstance(message, Mapping):
        role = message.get("role") or message.get("type")
    mapping = {
        "human": "user",
        "user": "user",
        "ai": "assistant",
        "assistant": "assistant",
        "system": "system",
    }
    return mapping.get(str(role).lower()) if role is not None else None


def _turn_result_from_reply(
    reply: Mapping[str, Any],
    *,
    request_id: str,
) -> RuntimeTurnResult:
    if not isinstance(reply, Mapping):
        raise PrologRlmRuntimeError("Prolog-RLM returned an invalid response")
    if reply.get("protocol") != ZARA_RUNTIME_PROTOCOL:
        raise PrologRlmRuntimeError(
            "Prolog-RLM returned an incompatible response",
            kind="incompatible_protocol",
        )
    if reply.get("runtime_id") != PROLOG_RLM_RUNTIME_ID:
        raise PrologRlmRuntimeError("Prolog-RLM response runtime identity changed")
    if reply.get("request_id") != request_id:
        raise PrologRlmRuntimeError("Prolog-RLM response request identity changed")
    status = reply.get("status")
    if status == "completed":
        text = reply.get("text", "")
        if not isinstance(text, str):
            raise PrologRlmRuntimeError("Prolog-RLM completed without normalized text")
        metadata: dict[str, Any] = {"runtime": PROLOG_RLM_RUNTIME_ID}
        result = reply.get("result")
        if isinstance(result, (dict, list, str, int, float, bool)) or result is None:
            metadata["result"] = result
        return RuntimeTurnResult(response=text, metadata=metadata)

    error = reply.get("error")
    kind = error.get("kind") if isinstance(error, Mapping) else "runtime_error"
    if not isinstance(kind, str) or kind not in _RUNTIME_ERROR_KINDS:
        kind = "runtime_error"
    raise PrologRlmRuntimeError(
        "Prolog-RLM runtime request failed",
        kind=kind,
    )


def _bounded_context_handles(context_ids) -> list[str]:
    if len(context_ids) > 256:
        raise ValueError("context handles exceed runtime bound")
    return [_bounded_id(value, "context handle") for value in context_ids]


def _bounded_text(value: str, maximum: int, field: str) -> str:
    if not isinstance(value, str):
        raise TypeError(f"{field} must be text")
    if len(value) > maximum:
        raise ValueError(f"{field} exceeds runtime bound")
    return value


def _bounded_id(value: str, field: str) -> str:
    text = _bounded_text(value, 128, field).strip()
    if not text or any(ord(char) < 0x20 for char in text):
        raise ValueError(f"{field} is invalid")
    return text


__all__ = ["PrologRlmRuntimeBackend", "PrologRlmRuntimeError"]
