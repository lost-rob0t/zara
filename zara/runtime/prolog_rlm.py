"""Loopback ZARA-RUNTIME/1 client for the native Prolog-RLM sidecar."""

from __future__ import annotations

import asyncio
import http.client
import json
from typing import Optional
from urllib.parse import urlsplit

from .backend import RuntimeBackend, RuntimeTurnResult


class PrologRLMRuntimeBackend(RuntimeBackend):
    def __init__(self, endpoint: str, *, principal_id: str, timeout: float = 120.0) -> None:
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
        self._started = False
        self._history: dict[str, list[tuple[str, str]]] = {}

    @property
    def principal_id(self) -> str:
        return self._principal_id

    def _request(self, route: str, payload: Optional[dict] = None) -> dict:
        body = None if payload is None else json.dumps(payload).encode("utf-8")
        connection = http.client.HTTPConnection(self._host, self._port, timeout=self._timeout)
        try:
            connection.request(
                "POST" if body is not None else "GET",
                f"/zara-runtime/v1/{route}",
                body=body,
                headers={"Content-Type": "application/json"} if body is not None else {},
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
        self._started = True

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
        response = await asyncio.to_thread(self._request, "generate", request)
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
        entries = self._history.setdefault(key, [])
        entries.extend((("user", prompt), ("assistant", result.response)))
        while len(entries) > 16 or sum(len(content) for _, content in entries) > 32768:
            entries.pop(0)

    async def cancel_turn(self, turn_id: str) -> None:
        if self._started:
            await asyncio.to_thread(self._request, "cancel", {"request_id": turn_id})

    async def stop(self) -> None:
        self._started = False
        self._history.clear()
