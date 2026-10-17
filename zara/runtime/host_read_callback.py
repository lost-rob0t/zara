"""Turn-fenced, loopback callback for Prolog-RLM read-only tools."""

from __future__ import annotations

import hmac
import json
import threading
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from typing import Any, Sequence

from langchain_core.tools import BaseTool
from pydantic import BaseModel, ValidationError

from zara.agent.tools.file_tools import FileReadArgs, FileTooling
from zara.runtime.turn_context import TurnCapabilityLease


class HostReadCallbackServer:
    PATH = "/zara-host-tool/v1/read"
    PROTOCOL = "ZARA-HOST-TOOL/1"
    MAX_REQUEST_BYTES = 8192
    MAX_RESULT_CHARS = 24000

    def __init__(self, tooling: FileTooling, *, token: str, generation: int,
                 read_tools: Sequence[BaseTool] = ()) -> None:
        if not token or not isinstance(generation, int) or generation < 1:
            raise ValueError("callback token and positive generation are required")
        self._tooling = tooling
        self._read_tools = {
            tool.name: tool for tool in read_tools
            if isinstance(tool, BaseTool)
            and tool.name != "file_read"
            and isinstance(tool.args_schema, type)
            and issubclass(tool.args_schema, BaseModel)
            and (tool.metadata or {}).get("zara_read_only") is True
            and (tool.metadata or {}).get("zara_requires_approval") is not True
            and (tool.metadata or {}).get("zara_supports_cancellation") is not True
        }
        self._token = token
        self._generation = generation
        self._turns: dict[str, TurnCapabilityLease] = {}
        self._lock = threading.RLock()
        self._server: ThreadingHTTPServer | None = None
        self._thread: threading.Thread | None = None

    @property
    def port(self) -> int:
        if self._server is None:
            raise RuntimeError("callback server is not running")
        return self._server.server_port

    @property
    def endpoint(self) -> str:
        return f"http://127.0.0.1:{self.port}{self.PATH}"

    @property
    def generation(self) -> int:
        return self._generation

    @property
    def read_tool_names(self) -> tuple[str, ...]:
        return tuple(self._read_tools)

    @property
    def read_tool_descriptors(self) -> tuple[dict[str, Any], ...]:
        result = {"type": "object", "properties": {"content": {"type": "string"}}, "required": ["content"]}
        return tuple({
            "name": name,
            "description": tool.description[:512],
            "arguments": self._reduce_schema(tool.args_schema.model_json_schema()),
            "result": result,
        } for name, tool in self._read_tools.items())

    @classmethod
    def _reduce_schema(cls, schema: dict[str, Any]) -> dict[str, Any]:
        kind = schema.get("type")
        if kind == "object":
            properties = schema.get("properties")
            if not isinstance(properties, dict):
                raise ValueError("read tool arguments need object properties")
            reduced = {"type": "object", "properties": {
                name: cls._reduce_schema(value) for name, value in properties.items()
            }}
            required = schema.get("required", [])
            if required:
                reduced["required"] = required
            return reduced
        if kind == "array":
            return {"type": "array", "items": cls._reduce_schema(schema["items"])}
        if kind in {"string", "integer", "number", "boolean"}:
            return {"type": kind}
        raise ValueError("read tool arguments use an unsupported schema")

    def register_turn(self, request_id: str, lease: TurnCapabilityLease) -> None:
        if not request_id or request_id != lease.turn_id:
            raise ValueError("request and turn capability must match")
        with self._lock, lease.registration():
            self._turns[request_id] = lease

    def unregister_turn(self, request_id: str) -> None:
        with self._lock:
            self._turns.pop(request_id, None)

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
                encoded = json.dumps(payload, separators=(",", ":")).encode("utf-8")
                self.send_response(status)
                self.send_header("Content-Type", "application/json")
                self.send_header("Content-Length", str(len(encoded)))
                self.end_headers()
                self.wfile.write(encoded)

            def log_message(self, format: str, *args: object) -> None:
                pass

        self._server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
        self._thread = threading.Thread(target=self._server.serve_forever, daemon=True)
        self._thread.start()

    def stop(self) -> None:
        with self._lock:
            self._turns.clear()
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
        response = {
            "protocol": self.PROTOCOL,
            "request_id": payload.get("request_id"),
            "tool_call_id": payload.get("tool_call_id"),
            "generation": payload.get("generation"),
            "status": "denied",
        }
        supplied = authorization.removeprefix("Bearer ") if authorization.startswith("Bearer ") else ""
        try:
            authorized = hmac.compare_digest(supplied, self._token)
        except TypeError:
            authorized = False
        if not authorized:
            return response
        request_id = payload.get("request_id")
        if (
            payload.get("protocol") != self.PROTOCOL
            or not isinstance(request_id, str)
            or not isinstance(payload.get("tool_call_id"), str)
            or type(payload.get("generation")) is not int
            or payload["generation"] != self._generation
            or not isinstance(payload.get("name"), str)
        ):
            return response
        with self._lock:
            lease = self._turns.get(request_id)
        if lease is None:
            return response
        name = payload["name"]
        if name == "file_read":
            try:
                arguments = FileReadArgs.model_validate(payload.get("args"))
            except ValidationError:
                return response
        else:
            tool = self._read_tools.get(name)
            supplied = payload.get("args")
            if tool is None or not isinstance(supplied, dict) or not set(supplied).issubset(tool.args_schema.model_fields):
                return response
            try:
                arguments = tool.args_schema.model_validate(supplied)
            except ValidationError:
                return response
        try:
            with lease.registration():
                if name == "file_read":
                    value = self._tooling.read_file(arguments.path, arguments.max_lines)
                else:
                    value = tool.invoke(arguments.model_dump())
        except Exception:
            return response
        if not isinstance(value, str) or len(value) > self.MAX_RESULT_CHARS or value.startswith("Error:"):
            return response
        response["status"] = "ok"
        response["value"] = {"content": value}
        return response
