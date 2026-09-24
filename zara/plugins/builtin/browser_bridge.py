from __future__ import annotations

import asyncio
import hmac
import json
import os
import threading
import uuid
from typing import Any, Optional
from urllib.parse import urlsplit

from aiohttp import WSMsgType, web
from langchain_core.tools import StructuredTool

from zara.plugins import PluginMetadata, ServicePlugin


DEFAULT_HOST = "127.0.0.1"
DEFAULT_PORT = 8765
DEFAULT_TIMEOUT_SECONDS = 20.0
MAX_REQUEST_BYTES = 64 * 1024
MAX_RESULT_BYTES = 16 * 1024 * 1024
MAX_SELECTOR_CHARS = 2048
MAX_TYPED_CHARS = 100_000
MAX_READ_CHARS = 100_000

READ_ACTIONS = {
    "tabs.list",
    "page.read",
    "page.elements",
    "page.extract",
    "page.screenshot",
}
WRITE_ACTIONS = {
    "tabs.open",
    "tabs.navigate",
    "page.click",
    "page.type",
    "page.submit",
}
ALLOWED_ACTIONS = READ_ACTIONS | WRITE_ACTIONS


class BrowserBridgePlugin(ServicePlugin):
    """Authenticated bridge between Zara tools and a real browser WebExtension."""

    enabled_by_default = False
    metadata = PluginMetadata(
        name="browser-bridge",
        version="0.1.0",
        description="Capability-gated bridge to the Zara browser add-on.",
    )

    def __init__(self) -> None:
        self._configuration: dict[str, Any] = {}
        self._token = ""
        self._host = DEFAULT_HOST
        self._port = DEFAULT_PORT
        self._timeout = DEFAULT_TIMEOUT_SECONDS
        self._loop: Optional[asyncio.AbstractEventLoop] = None
        self._runner: Optional[web.AppRunner] = None
        self._socket: Optional[web.WebSocketResponse] = None
        self._pending: dict[str, asyncio.Future] = {}
        self._ready = threading.Event()
        self._startup_error: Optional[BaseException] = None

    def start(self, runtime) -> None:
        self._configuration = dict(runtime.configuration)
        self._host = str(self._configuration.get("host", DEFAULT_HOST)).strip()
        self._port = self._bounded_port(self._configuration.get("port", DEFAULT_PORT))
        self._timeout = self._bounded_timeout(
            self._configuration.get("timeout_seconds", DEFAULT_TIMEOUT_SECONDS)
        )
        self._token = self._resolve_token(self._configuration)
        self._require_safe_bind(self._host, self._configuration)
        self._ready.clear()
        self._startup_error = None
        runtime.start_worker("browser-bridge", self._worker)
        if not self._ready.wait(timeout=3.0):
            raise RuntimeError("browser bridge did not become ready")
        if self._startup_error is not None:
            raise RuntimeError(f"browser bridge failed to start: {self._startup_error}")

    def stop(self) -> None:
        loop = self._loop
        if loop is not None and loop.is_running():
            try:
                asyncio.run_coroutine_threadsafe(self._close_browser(), loop)
            except RuntimeError:
                pass

    def tools(self):
        return (
            StructuredTool.from_function(
                self.browser_tabs,
                name="browser_tabs",
                description="List open browser tabs with sanitized metadata.",
            ),
            StructuredTool.from_function(
                self.browser_read,
                name="browser_read",
                description="Read visible text and page metadata from a browser tab.",
            ),
            StructuredTool.from_function(
                self.browser_elements,
                name="browser_elements",
                description=(
                    "List interactive page elements with generated CSS selectors."
                ),
            ),
            StructuredTool.from_function(
                self.browser_extract,
                name="browser_extract",
                description="Extract bounded text and attributes from a CSS selector.",
            ),
            StructuredTool.from_function(
                self.browser_screenshot,
                name="browser_screenshot",
                description=(
                    "Capture the visible active browser page as a data URL suitable "
                    "for a multimodal model."
                ),
            ),
            StructuredTool.from_function(
                self.browser_open,
                name="browser_open",
                description="Open an HTTP or HTTPS URL in a new browser tab.",
                metadata={"zara_requires_approval": True},
            ),
            StructuredTool.from_function(
                self.browser_navigate,
                name="browser_navigate",
                description="Navigate a browser tab to an HTTP or HTTPS URL.",
                metadata={"zara_requires_approval": True},
            ),
            StructuredTool.from_function(
                self.browser_click,
                name="browser_click",
                description="Click a CSS-selected element in the browser page.",
                metadata={"zara_requires_approval": True},
            ),
            StructuredTool.from_function(
                self.browser_type,
                name="browser_type",
                description="Type bounded text into a CSS-selected editable element.",
                metadata={"zara_requires_approval": True},
            ),
            StructuredTool.from_function(
                self.browser_submit,
                name="browser_submit",
                description="Submit a CSS-selected form or submit control.",
                metadata={"zara_requires_approval": True},
            ),
        )

    def browser_tabs(self) -> dict[str, Any]:
        return self._rpc_sync("tabs.list", {})

    def browser_read(
        self,
        tab_id: Optional[int] = None,
        max_chars: int = 20_000,
    ) -> dict[str, Any]:
        args = self._tab_args(tab_id)
        args["max_chars"] = self._bounded_chars(max_chars)
        return self._rpc_sync("page.read", args)

    def browser_elements(
        self,
        tab_id: Optional[int] = None,
        max_items: int = 100,
    ) -> dict[str, Any]:
        args = self._tab_args(tab_id)
        args["max_items"] = self._bounded_items(max_items)
        return self._rpc_sync("page.elements", args)

    def browser_extract(
        self,
        selector: str,
        tab_id: Optional[int] = None,
        max_chars: int = 10_000,
    ) -> dict[str, Any]:
        args = self._tab_args(tab_id)
        args["selector"] = self._selector(selector)
        args["max_chars"] = self._bounded_chars(max_chars)
        return self._rpc_sync("page.extract", args)

    def browser_screenshot(self, tab_id: Optional[int] = None) -> dict[str, Any]:
        return self._rpc_sync("page.screenshot", self._tab_args(tab_id))

    def browser_open(self, url: str, active: bool = True) -> dict[str, Any]:
        return self._rpc_sync(
            "tabs.open",
            {"url": self._url(url), "active": bool(active)},
        )

    def browser_navigate(
        self,
        url: str,
        tab_id: Optional[int] = None,
    ) -> dict[str, Any]:
        args = self._tab_args(tab_id)
        args["url"] = self._url(url)
        return self._rpc_sync("tabs.navigate", args)

    def browser_click(
        self,
        selector: str,
        tab_id: Optional[int] = None,
    ) -> dict[str, Any]:
        args = self._tab_args(tab_id)
        args["selector"] = self._selector(selector)
        return self._rpc_sync("page.click", args)

    def browser_type(
        self,
        selector: str,
        text: str,
        tab_id: Optional[int] = None,
        clear: bool = True,
    ) -> dict[str, Any]:
        if not isinstance(text, str):
            raise TypeError("text must be a string")
        if len(text) > MAX_TYPED_CHARS:
            raise ValueError(f"text must not exceed {MAX_TYPED_CHARS} characters")
        args = self._tab_args(tab_id)
        args.update(
            {
                "selector": self._selector(selector),
                "text": text,
                "clear": bool(clear),
            }
        )
        return self._rpc_sync("page.type", args)

    def browser_submit(
        self,
        selector: str,
        tab_id: Optional[int] = None,
    ) -> dict[str, Any]:
        args = self._tab_args(tab_id)
        args["selector"] = self._selector(selector)
        return self._rpc_sync("page.submit", args)

    def _worker(self, stop_event: threading.Event) -> None:
        try:
            asyncio.run(self._serve(stop_event))
        except BaseException as error:
            self._startup_error = error
            self._ready.set()
            raise

    async def _serve(self, stop_event: threading.Event) -> None:
        self._loop = asyncio.get_running_loop()
        app = web.Application(client_max_size=MAX_REQUEST_BYTES)
        app.router.add_get("/v1/status", self._http_status)
        app.router.add_post("/v1/rpc", self._http_rpc)
        app.router.add_get("/v1/browser", self._browser_socket)
        runner = web.AppRunner(app, access_log=None)
        self._runner = runner
        await runner.setup()
        site = web.TCPSite(runner, self._host, self._port)
        try:
            await site.start()
            self._ready.set()
            while not stop_event.is_set():
                await asyncio.sleep(0.1)
        finally:
            self._ready.set()
            await self._close_browser()
            self._fail_pending(RuntimeError("browser bridge stopped"))
            await runner.cleanup()
            self._runner = None
            self._loop = None

    async def _http_status(self, _request: web.Request) -> web.Response:
        return web.json_response(
            {
                "service": "zara-browser-bridge",
                "version": 1,
                "browser_connected": self._socket is not None
                and not self._socket.closed,
            }
        )

    async def _http_rpc(self, request: web.Request) -> web.Response:
        if not self._authorized(request):
            raise web.HTTPUnauthorized()
        try:
            payload = await request.json()
        except (json.JSONDecodeError, ValueError, TypeError):
            raise web.HTTPBadRequest(text="invalid JSON")
        if not isinstance(payload, dict):
            raise web.HTTPBadRequest(text="request must be an object")
        action = payload.get("action")
        args = payload.get("args", {})
        if action not in ALLOWED_ACTIONS or not isinstance(args, dict):
            raise web.HTTPBadRequest(text="invalid browser action")
        try:
            result = await self._send_rpc(action, args)
        except TimeoutError:
            return web.json_response(
                {"ok": False, "error": "browser operation timed out"},
                status=504,
            )
        except (ConnectionError, RuntimeError) as error:
            return web.json_response(
                {"ok": False, "error": str(error)[:300]},
                status=503,
            )
        return web.json_response({"ok": True, "result": result})

    async def _browser_socket(self, request: web.Request) -> web.StreamResponse:
        origin = request.headers.get("Origin", "")
        if not (
            origin.startswith("chrome-extension://")
            or origin.startswith("moz-extension://")
        ):
            raise web.HTTPForbidden(text="browser extension origin required")

        ws = web.WebSocketResponse(max_msg_size=MAX_RESULT_BYTES)
        await ws.prepare(request)
        try:
            hello = await asyncio.wait_for(ws.receive(), timeout=5.0)
            if hello.type != WSMsgType.TEXT:
                await ws.close(code=1008, message=b"authentication required")
                return ws
            try:
                payload = json.loads(hello.data)
            except json.JSONDecodeError:
                payload = {}
            token = payload.get("token") if isinstance(payload, dict) else None
            if (
                not isinstance(payload, dict)
                or payload.get("type") != "hello"
                or not isinstance(token, str)
                or not hmac.compare_digest(token, self._token)
            ):
                await ws.close(code=1008, message=b"authentication failed")
                return ws

            previous = self._socket
            self._socket = ws
            if previous is not None and previous is not ws and not previous.closed:
                await previous.close(code=1001, message=b"replaced")
            await ws.send_json({"type": "ready", "protocol": 1})

            async for message in ws:
                if message.type == WSMsgType.TEXT:
                    self._handle_browser_message(message.data)
                elif message.type in {WSMsgType.ERROR, WSMsgType.CLOSE}:
                    break
        finally:
            if self._socket is ws:
                self._socket = None
                self._fail_pending(ConnectionError("browser add-on disconnected"))
        return ws

    def _handle_browser_message(self, raw: str) -> None:
        try:
            payload = json.loads(raw)
        except json.JSONDecodeError:
            return
        if not isinstance(payload, dict) or payload.get("type") != "result":
            return
        request_id = payload.get("id")
        if not isinstance(request_id, str):
            return
        future = self._pending.pop(request_id, None)
        if future is None or future.done():
            return
        if payload.get("ok") is True:
            result = payload.get("result", {})
            try:
                encoded = json.dumps(result, separators=(",", ":")).encode("utf-8")
            except (TypeError, ValueError) as error:
                future.set_exception(RuntimeError(f"invalid browser result: {error}"))
                return
            if len(encoded) > MAX_RESULT_BYTES:
                future.set_exception(RuntimeError("browser result exceeds size limit"))
                return
            future.set_result(result)
        else:
            error = str(payload.get("error", "browser operation failed"))[:300]
            future.set_exception(RuntimeError(error))

    async def _send_rpc(self, action: str, args: dict[str, Any]) -> dict[str, Any]:
        ws = self._socket
        if ws is None or ws.closed:
            raise ConnectionError("Zara browser add-on is not connected")
        request_id = uuid.uuid4().hex
        loop = asyncio.get_running_loop()
        future = loop.create_future()
        self._pending[request_id] = future
        try:
            await ws.send_json(
                {
                    "type": "call",
                    "id": request_id,
                    "action": action,
                    "args": args,
                }
            )
            result = await asyncio.wait_for(future, timeout=self._timeout)
            if not isinstance(result, dict):
                raise RuntimeError("browser add-on returned a non-object result")
            return result
        finally:
            self._pending.pop(request_id, None)

    def _rpc_sync(self, action: str, args: dict[str, Any]) -> dict[str, Any]:
        if action not in ALLOWED_ACTIONS:
            raise ValueError("unsupported browser action")
        loop = self._loop
        if loop is None or not loop.is_running():
            raise RuntimeError("browser bridge is not running")
        future = asyncio.run_coroutine_threadsafe(self._send_rpc(action, args), loop)
        return future.result(timeout=self._timeout + 2.0)

    async def _close_browser(self) -> None:
        ws = self._socket
        self._socket = None
        if ws is not None and not ws.closed:
            await ws.close(code=1001, message=b"Zara bridge stopping")

    def _fail_pending(self, error: BaseException) -> None:
        pending = tuple(self._pending.values())
        self._pending.clear()
        for future in pending:
            if not future.done():
                future.set_exception(error)

    def _authorized(self, request: web.Request) -> bool:
        header = request.headers.get("Authorization", "")
        prefix = "Bearer "
        if not header.startswith(prefix):
            return False
        candidate = header[len(prefix) :]
        return bool(candidate) and hmac.compare_digest(candidate, self._token)

    @staticmethod
    def _resolve_token(configuration: dict[str, Any]) -> str:
        env_name = str(
            configuration.get("token_env", "ZARA_BROWSER_BRIDGE_TOKEN")
        ).strip()
        if not env_name:
            raise RuntimeError("browser bridge token_env must not be empty")
        token = os.environ.get(env_name, "")
        if len(token) < 24:
            raise RuntimeError(
                f"browser bridge token environment variable {env_name!r} "
                "must contain at least 24 characters"
            )
        return token

    @staticmethod
    def _require_safe_bind(host: str, configuration: dict[str, Any]) -> None:
        if host in {"127.0.0.1", "::1"}:
            return
        raise RuntimeError(
            "browser bridge cleartext transport requires a literal loopback host"
        )

    @staticmethod
    def _bounded_port(value: Any) -> int:
        try:
            port = int(value)
        except (TypeError, ValueError) as error:
            raise ValueError("browser bridge port must be an integer") from error
        if not 1 <= port <= 65535:
            raise ValueError("browser bridge port must be between 1 and 65535")
        return port

    @staticmethod
    def _bounded_timeout(value: Any) -> float:
        try:
            timeout = float(value)
        except (TypeError, ValueError) as error:
            raise ValueError("browser bridge timeout must be numeric") from error
        if not 1.0 <= timeout <= 60.0:
            raise ValueError("browser bridge timeout must be between 1 and 60 seconds")
        return timeout

    @staticmethod
    def _tab_args(tab_id: Optional[int]) -> dict[str, Any]:
        if tab_id is None:
            return {}
        if not isinstance(tab_id, int) or isinstance(tab_id, bool) or tab_id <= 0:
            raise ValueError("tab_id must be a positive integer")
        return {"tab_id": tab_id}

    @staticmethod
    def _selector(selector: str) -> str:
        if not isinstance(selector, str):
            raise TypeError("selector must be a string")
        selector = selector.strip()
        if not selector or len(selector) > MAX_SELECTOR_CHARS:
            raise ValueError(
                f"selector must contain 1 to {MAX_SELECTOR_CHARS} characters"
            )
        return selector

    @staticmethod
    def _bounded_items(value: Any) -> int:
        try:
            count = int(value)
        except (TypeError, ValueError) as error:
            raise ValueError("max_items must be an integer") from error
        if not 1 <= count <= 500:
            raise ValueError("max_items must be between 1 and 500")
        return count

    @staticmethod
    def _bounded_chars(value: Any) -> int:
        try:
            count = int(value)
        except (TypeError, ValueError) as error:
            raise ValueError("max_chars must be an integer") from error
        if not 1 <= count <= MAX_READ_CHARS:
            raise ValueError(f"max_chars must be between 1 and {MAX_READ_CHARS}")
        return count

    @staticmethod
    def _url(url: str) -> str:
        if not isinstance(url, str):
            raise TypeError("url must be a string")
        url = url.strip()
        parsed = urlsplit(url)
        if parsed.scheme not in {"http", "https"} or not parsed.netloc:
            raise ValueError("browser URLs must use http or https")
        if len(url) > 8192:
            raise ValueError("url must not exceed 8192 characters")
        return url


def create_plugin():
    return BrowserBridgePlugin()
