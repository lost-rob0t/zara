from __future__ import annotations

import asyncio
import os
import socket
import threading
from types import SimpleNamespace

from aiohttp import ClientSession, WSServerHandshakeError

from zara.plugins.builtin.browser_bridge import (
    BrowserBridgePlugin,
    create_plugin,
)


TOKEN = "test-browser-bridge-token-0123456789"


def free_port() -> int:
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as listener:
        listener.bind(("127.0.0.1", 0))
        return listener.getsockname()[1]


class FakeRuntime:
    def __init__(self, configuration):
        self.configuration = configuration
        self.stop_event = threading.Event()
        self.thread = None

    def start_worker(self, _name, target):
        self.thread = threading.Thread(
            target=target,
            args=(self.stop_event,),
            daemon=True,
        )
        self.thread.start()
        return SimpleNamespace()

    def shutdown(self, plugin):
        plugin.stop()
        self.stop_event.set()
        if self.thread is not None:
            self.thread.join(timeout=5)
            assert not self.thread.is_alive()


def test_browser_tools_mark_mutations_for_approval():
    plugin = create_plugin()
    tools = {tool.name: tool for tool in plugin.tools()}

    assert set(tools) == {
        "browser_tabs",
        "browser_read",
        "browser_elements",
        "browser_extract",
        "browser_screenshot",
        "browser_open",
        "browser_navigate",
        "browser_click",
        "browser_type",
        "browser_submit",
    }

    for name in (
        "browser_tabs",
        "browser_read",
        "browser_elements",
        "browser_extract",
        "browser_screenshot",
    ):
        assert not (tools[name].metadata or {}).get("zara_requires_approval", False)

    for name in (
        "browser_open",
        "browser_navigate",
        "browser_click",
        "browser_type",
        "browser_submit",
    ):
        assert tools[name].metadata["zara_requires_approval"] is True


def test_browser_bridge_validates_urls_and_bind_policy():
    assert BrowserBridgePlugin._url("https://example.com/a") == "https://example.com/a"

    for invalid in ("javascript:alert(1)", "file:///tmp/a", "data:text/plain,hi"):
        try:
            BrowserBridgePlugin._url(invalid)
        except ValueError:
            pass
        else:
            raise AssertionError(f"accepted unsafe browser URL {invalid!r}")

    BrowserBridgePlugin._require_safe_bind("127.0.0.1", {})
    BrowserBridgePlugin._require_safe_bind("::1", {})
    for unsafe_host, configuration in (
        ("localhost", {}),
        ("0.0.0.0", {}),
        ("0.0.0.0", {"allow_remote": True}),
        ("192.0.2.10", {"allow_remote": True}),
    ):
        try:
            BrowserBridgePlugin._require_safe_bind(unsafe_host, configuration)
        except RuntimeError:
            pass
        else:
            raise AssertionError(
                f"cleartext browser bridge accepted unsafe host {unsafe_host!r}"
            )


def test_browser_bridge_requires_strong_environment_token(monkeypatch):
    monkeypatch.delenv("ZARA_BROWSER_BRIDGE_TOKEN", raising=False)
    try:
        BrowserBridgePlugin._resolve_token({})
    except RuntimeError:
        pass
    else:
        raise AssertionError("missing token was accepted")

    monkeypatch.setenv("ZARA_BROWSER_BRIDGE_TOKEN", TOKEN)
    assert BrowserBridgePlugin._resolve_token({}) == TOKEN


def test_browser_bridge_http_to_extension_round_trip(monkeypatch):
    port = free_port()
    monkeypatch.setenv("ZARA_BROWSER_BRIDGE_TOKEN", TOKEN)
    plugin = BrowserBridgePlugin()
    runtime = FakeRuntime(
        {
            "host": "127.0.0.1",
            "port": port,
            "timeout_seconds": 2,
        }
    )
    plugin.start(runtime)

    async def scenario():
        async with ClientSession() as session:
            try:
                await session.ws_connect(
                    f"http://127.0.0.1:{port}/v1/browser"
                )
            except WSServerHandshakeError as error:
                assert error.status == 403
            else:
                raise AssertionError("origin-less browser WebSocket was accepted")

            websocket = await session.ws_connect(
                f"http://127.0.0.1:{port}/v1/browser",
                headers={"Origin": "moz-extension://zara-test"},
            )
            await websocket.send_json({"type": "hello", "token": TOKEN, "protocol": 1})
            ready = await websocket.receive_json(timeout=2)
            assert ready == {"type": "ready", "protocol": 1}

            unauthorized = await session.post(
                f"http://127.0.0.1:{port}/v1/rpc",
                json={"action": "tabs.list", "args": {}},
            )
            assert unauthorized.status == 401

            async def extension_reply():
                call = await websocket.receive_json(timeout=2)
                assert call["type"] == "call"
                assert call["action"] == "tabs.list"
                await websocket.send_json(
                    {
                        "type": "result",
                        "id": call["id"],
                        "ok": True,
                        "result": {
                            "tabs": [
                                {
                                    "id": 7,
                                    "title": "Example",
                                    "url": "https://example.com/",
                                }
                            ]
                        },
                    }
                )

            reply_task = asyncio.create_task(extension_reply())
            response = await session.post(
                f"http://127.0.0.1:{port}/v1/rpc",
                headers={"Authorization": f"Bearer {TOKEN}"},
                json={"action": "tabs.list", "args": {}},
            )
            body = await response.json()
            await reply_task
            assert response.status == 200
            assert body["ok"] is True
            assert body["result"]["tabs"][0]["id"] == 7
            await websocket.close()

    try:
        asyncio.run(scenario())
    finally:
        runtime.shutdown(plugin)
