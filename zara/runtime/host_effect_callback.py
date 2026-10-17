"""Authenticated, turn-fenced callback for approved Prolog-RLM effects."""

from __future__ import annotations

import hmac
import json
import threading
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from typing import Any

from langchain_core.tools import BaseTool

from zara.runtime.turn_context import TurnCapabilityLease
from zara.tasks.effect_journal import RepositoryEffectJournal


class HostEffectCallbackServer:
    PATH = "/zara-host-tool/v1/effect"
    PROTOCOL = "ZARA-HOST-EFFECT/1"
    MAX_REQUEST_BYTES = 16384

    def __init__(self, tools: dict[str, BaseTool], journal: RepositoryEffectJournal,
                 *, principal_id: str, token: str, generation: int) -> None:
        if not token or not principal_id or generation < 1:
            raise ValueError("effect callback requires principal, token, and generation")
        self._tools = tools
        self._journal = journal
        self._principal_id = principal_id
        self._token = token
        self._generation = generation
        self._approvals: dict[str, tuple[str, dict, TurnCapabilityLease]] = {}
        self._lock = threading.RLock()
        self._server: ThreadingHTTPServer | None = None
        self._thread: threading.Thread | None = None

    @property
    def endpoint(self) -> str:
        if self._server is None:
            raise RuntimeError("effect callback is not running")
        return f"http://127.0.0.1:{self._server.server_port}{self.PATH}"

    def register_approval(self, request_id: str, approval: dict, lease: TurnCapabilityLease) -> None:
        if request_id != lease.turn_id or not lease.active:
            raise RuntimeError("effect approval has no active turn")
        approval_id = approval.get("id")
        if not isinstance(approval_id, str) or approval.get("name") not in self._tools:
            raise ValueError("effect approval is unknown")
        with self._lock, lease.registration():
            if approval_id in self._approvals:
                raise ValueError("duplicate effect approval")
            self._approvals[approval_id] = (request_id, approval, lease)

    def unregister_approval(self, approval_id: str) -> None:
        with self._lock:
            self._approvals.pop(approval_id, None)

    def start(self) -> None:
        if self._server is not None:
            return
        callback = self

        class Handler(BaseHTTPRequestHandler):
            def do_POST(self) -> None:
                if self.path != callback.PATH:
                    self._reply(404, {"status": "denied"})
                    return
                length = self.headers.get("Content-Length", "")
                if not length.isdecimal() or int(length) > callback.MAX_REQUEST_BYTES:
                    self._reply(400, {"status": "denied"})
                    return
                try:
                    payload = json.loads(self.rfile.read(int(length)))
                except (ValueError, UnicodeDecodeError):
                    self._reply(400, {"status": "denied"})
                    return
                self._reply(200, callback._handle(payload, self.headers.get("Authorization", "")))

            def _reply(self, status: int, payload: dict[str, Any]) -> None:
                encoded = json.dumps(payload, separators=(",", ":")).encode()
                self.send_response(status)
                self.send_header("Content-Type", "application/json")
                self.send_header("Content-Length", str(len(encoded)))
                self.end_headers()
                self.wfile.write(encoded)

            def log_message(self, *_args: object) -> None:
                pass

        self._server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
        self._thread = threading.Thread(target=self._server.serve_forever, daemon=True)
        self._thread.start()

    def stop(self) -> None:
        with self._lock:
            self._approvals.clear()
        if self._server is not None:
            self._server.shutdown()
            self._server.server_close()
            self._server = None
        if self._thread is not None:
            self._thread.join(timeout=2)
            self._thread = None

    def _handle(self, payload: object, authorization: str) -> dict[str, Any]:
        if not isinstance(payload, dict):
            return {"status": "denied"}
        response = {key: payload.get(key) for key in (
            "protocol", "request_id", "tool_call_id", "generation", "attempt_id", "effect_fingerprint"
        )}
        response["status"] = "denied"
        supplied = authorization.removeprefix("Bearer ") if authorization.startswith("Bearer ") else ""
        if not hmac.compare_digest(supplied, self._token):
            return response
        if payload.get("protocol") != self.PROTOCOL or type(payload.get("generation")) is not int or payload["generation"] != self._generation:
            return response
        if payload.get("tool_call_id") != payload.get("attempt_id"):
            return response
        with self._lock:
            for request_id, approval, lease in self._approvals.values():
                if request_id != payload.get("request_id") or not lease.active:
                    continue
                if any(payload.get(key) != approval.get(key) for key in (
                    "attempt_id", "effect_fingerprint", "idempotency_key", "name", "args"
                )):
                    continue
                if approval.get("effect") not in {"write", "process"}:
                    continue
                try:
                    with lease.registration():
                        claim = self._journal.claim(
                            self._principal_id, approval["attempt_id"],
                            approval["effect_fingerprint"], approval["name"], approval["args"],
                        )
                        if claim.status == "replay":
                            value = claim.result
                        elif claim.status == "claimed":
                            result = self._tools[approval["name"]].invoke(approval["args"])
                            if not isinstance(result, str):
                                result = json.dumps(result, separators=(",", ":"))
                            self._journal.complete(claim, result)
                            value = result
                        elif claim.status == "denied":
                            return response
                        else:
                            response["status"] = "indeterminate"
                            return response
                except Exception:
                    response["status"] = "indeterminate"
                    return response
                response["status"] = "observed"
                response["value"] = {"result": value}
                return response
        return response
