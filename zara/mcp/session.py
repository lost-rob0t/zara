"""Actor-owned MCP server sessions."""

from __future__ import annotations

import asyncio
import json
import logging
import os
import sys
from contextlib import asynccontextmanager
from dataclasses import dataclass
from enum import Enum
from typing import Any, AsyncIterator, Callable, Coroutine, TextIO

from .config import MCPServerConfig

logger = logging.getLogger(__name__)


class MCPSessionState(str, Enum):
    CONFIGURED = "configured"
    STARTING = "starting"
    INITIALIZING = "initializing"
    READY = "ready"
    FAILED = "failed"
    STOPPING = "stopping"
    STOPPED = "stopped"


class _RedactingWriter:
    """Provide a real subprocess fd without exposing configured env values.

    Python subprocess launchers consume ``fileno()`` directly, so a normal
    ``write()`` wrapper cannot redact child stderr. When configured environment
    values are present, route the child's stderr to ``os.devnull`` instead.
    With no configured values, retain normal stderr diagnostics.
    """

    def __init__(self, target: TextIO, secrets: list[str]):
        self.target = target
        self.secrets = tuple(value for value in secrets if value)
        self._sink: TextIO = (
            open(os.devnull, "w", encoding=getattr(target, "encoding", None) or "utf-8")
            if self.secrets
            else target
        )

    def fileno(self) -> int:
        return self._sink.fileno()

    def write(self, text: str) -> int:
        if self.secrets:
            return len(text)
        return self.target.write(text)

    def flush(self) -> None:
        self._sink.flush()

    def close(self) -> None:
        if self._sink is not self.target:
            self._sink.close()


class MCPUnavailableError(RuntimeError):
    pass


class MCPRequestError(RuntimeError):
    pass


@dataclass
class _Command:
    operation: str
    args: tuple[Any, ...]
    kwargs: dict[str, Any]
    future: asyncio.Future[Any]


def _model_dump(value: Any) -> Any:
    if hasattr(value, "model_dump"):
        return value.model_dump(mode="json", by_alias=False, exclude_none=True)
    if isinstance(value, list):
        return [_model_dump(item) for item in value]
    if isinstance(value, tuple):
        return [_model_dump(item) for item in value]
    if isinstance(value, dict):
        return {str(key): _model_dump(item) for key, item in value.items()}
    return value


def normalize_tool_result(result: Any) -> dict[str, Any]:
    """Preserve every MCP content block plus structured content."""
    return {
        "is_error": bool(getattr(result, "is_error", False)),
        "content": _model_dump(getattr(result, "content", [])),
        "structured_content": _model_dump(getattr(result, "structured_content", None)),
        "meta": _model_dump(getattr(result, "meta", None)),
    }


class MCPServerActor:
    """One asyncio actor owns one MCP ``Client`` and its transport lifecycle."""

    def __init__(
        self,
        config: MCPServerConfig,
        *,
        on_capability_change: Callable[[str], Coroutine[Any, Any, None]] | None = None,
    ):
        self.config = config
        self.state = MCPSessionState.CONFIGURED
        self.error: str | None = None
        self.protocol_version: str | None = None
        self.server_info: Any = None
        self.capabilities: Any = None
        self.instructions: str | None = None
        self._queue: asyncio.Queue[_Command | None] = asyncio.Queue()
        self._task: asyncio.Task[None] | None = None
        self._ready: asyncio.Future[None] | None = None
        self._client: Any = None
        self._subscription_task: asyncio.Task[None] | None = None
        self._resource_subscriptions: tuple[str, ...] = ()
        self._inflight: set[asyncio.Task[None]] = set()
        self._on_capability_change = on_capability_change

    async def start(self) -> None:
        if self._task and not self._task.done():
            assert self._ready is not None
            await self._ready
            return
        loop = asyncio.get_running_loop()
        self._ready = loop.create_future()
        self._task = asyncio.create_task(self._run(), name=f"zara-mcp-{self.config.name}")
        try:
            await asyncio.wait_for(asyncio.shield(self._ready), timeout=self.config.connect_timeout)
        except asyncio.TimeoutError as error:
            self.error = f"connection timed out after {self.config.connect_timeout:g}s"
            self.state = MCPSessionState.FAILED
            self._task.cancel()
            await asyncio.gather(self._task, return_exceptions=True)
            raise MCPRequestError(f"MCP server {self.config.name!r} {self.error}") from error

    async def request(self, operation: str, *args: Any, **kwargs: Any) -> Any:
        await self.start()
        if self.state is not MCPSessionState.READY:
            raise MCPRequestError(
                f"MCP server {self.config.name!r} is {self.state.value}: {self.error or 'not ready'}"
            )
        loop = asyncio.get_running_loop()
        future: asyncio.Future[Any] = loop.create_future()
        await self._queue.put(_Command(operation, args, kwargs, future))
        try:
            return await asyncio.wait_for(future, timeout=self.config.request_timeout)
        except asyncio.TimeoutError as error:
            if not future.done():
                future.cancel()
            raise MCPRequestError(
                f"MCP request timed out: {self.config.name}/{operation}"
            ) from error

    async def stop(self) -> None:
        if not self._task:
            self.state = MCPSessionState.STOPPED
            return
        if self._task.done():
            self.state = MCPSessionState.STOPPED
            return
        self.state = MCPSessionState.STOPPING
        await self._queue.put(None)
        try:
            await asyncio.wait_for(self._task, timeout=max(5.0, self.config.connect_timeout))
        except asyncio.TimeoutError:
            self._task.cancel()
            await asyncio.gather(self._task, return_exceptions=True)
        finally:
            self.state = MCPSessionState.STOPPED

    async def _run(self) -> None:
        self.state = MCPSessionState.STARTING
        logger.info("MCP server %s starting (%s)", self.config.name, self.config.transport)
        try:
            async with self._open_client() as client:
                self._client = client
                self.state = MCPSessionState.INITIALIZING
                self.protocol_version = str(getattr(client, "protocol_version", "") or "") or None
                self.server_info = _model_dump(getattr(client, "server_info", None))
                self.capabilities = getattr(client, "server_capabilities", None)
                self.instructions = getattr(client, "instructions", None)
                self.state = MCPSessionState.READY
                logger.info(
                    "MCP server %s ready protocol=%s capabilities=%s",
                    self.config.name,
                    self.protocol_version,
                    self._capability_names(),
                )
                if self._ready and not self._ready.done():
                    self._ready.set_result(None)
                self._subscription_task = self._start_subscription_listener(client)
                await self._command_loop(client)
        except asyncio.CancelledError:
            raise
        except Exception as error:
            self.error = f"{type(error).__name__}: {error}"
            self.state = MCPSessionState.FAILED
            logger.warning("MCP server %s failed: %s", self.config.name, self.error)
            if self._ready and not self._ready.done():
                self._ready.set_exception(MCPRequestError(self.error))
            self._fail_pending(self.error)
        finally:
            if self._subscription_task:
                self._subscription_task.cancel()
                await asyncio.gather(self._subscription_task, return_exceptions=True)
                self._subscription_task = None
            self._client = None
            if self.state not in {MCPSessionState.FAILED, MCPSessionState.STOPPED}:
                self.state = MCPSessionState.STOPPED
            logger.info("MCP server %s disconnected", self.config.name)

    async def _command_loop(self, client: Any) -> None:
        try:
            while True:
                command = await self._queue.get()
                if command is None:
                    return
                if command.future.cancelled():
                    continue
                task = asyncio.create_task(
                    self._execute_command(client, command),
                    name=f"zara-mcp-{self.config.name}-{command.operation}",
                )
                self._inflight.add(task)
                task.add_done_callback(self._inflight.discard)

                def cancel_request(future: asyncio.Future[Any], operation_task: asyncio.Task[None] = task) -> None:
                    if future.cancelled() and not operation_task.done():
                        operation_task.cancel()

                command.future.add_done_callback(cancel_request)
        finally:
            if self._inflight:
                for task in tuple(self._inflight):
                    task.cancel()
                await asyncio.gather(*tuple(self._inflight), return_exceptions=True)
                self._inflight.clear()

    async def _execute_command(self, client: Any, command: _Command) -> None:
        try:
            target = getattr(client, command.operation)
            result = await target(*command.args, **command.kwargs)
        except asyncio.CancelledError:
            if not command.future.done():
                command.future.cancel()
            raise
        except Exception as error:
            message = (
                f"MCP {self.config.name}/{command.operation} failed: "
                f"{type(error).__name__}: {error}"
            )
            if not command.future.done():
                command.future.set_exception(MCPRequestError(message))
            self.error = message
            self.state = MCPSessionState.FAILED
            await self._queue.put(None)
        else:
            if not command.future.done():
                command.future.set_result(result)

    def _fail_pending(self, message: str) -> None:
        while not self._queue.empty():
            command = self._queue.get_nowait()
            if command is not None and not command.future.done():
                command.future.set_exception(MCPRequestError(message))

    def _capability_names(self) -> list[str]:
        caps = self.capabilities
        if caps is None:
            return []
        names = []
        for name in ("tools", "resources", "prompts", "completions"):
            if getattr(caps, name, None) is not None:
                names.append(name)
        return names

    async def update_resource_subscriptions(self, uris: list[str]) -> None:
        """Update modern resource subscriptions without restarting the MCP session."""
        wanted = tuple(sorted(set(uris)))
        if wanted == self._resource_subscriptions:
            return
        self._resource_subscriptions = wanted
        if self.state is not MCPSessionState.READY or self._client is None:
            return
        if self._subscription_task is not None:
            self._subscription_task.cancel()
            await asyncio.gather(self._subscription_task, return_exceptions=True)
        self._subscription_task = self._start_subscription_listener(self._client)

    def _start_subscription_listener(self, client: Any) -> asyncio.Task[None] | None:
        if not hasattr(client, "listen") or self._on_capability_change is None:
            return None
        caps = self.capabilities
        tools = bool(getattr(getattr(caps, "tools", None), "list_changed", False))
        prompts = bool(getattr(getattr(caps, "prompts", None), "list_changed", False))
        resources = bool(getattr(getattr(caps, "resources", None), "list_changed", False))
        if not any((tools, prompts, resources, self._resource_subscriptions)):
            return None
        return asyncio.create_task(
            self._listen_changes(
                client,
                tools=tools,
                prompts=prompts,
                resources=resources,
                resource_subscriptions=self._resource_subscriptions,
            ),
            name=f"zara-mcp-listen-{self.config.name}",
        )

    async def _listen_changes(
        self,
        client: Any,
        *,
        tools: bool,
        prompts: bool,
        resources: bool,
        resource_subscriptions: tuple[str, ...],
    ) -> None:
        try:
            async with client.listen(
                tools_list_changed=tools,
                prompts_list_changed=prompts,
                resources_list_changed=resources,
                resource_subscriptions=resource_subscriptions,
            ) as subscription:
                async for _event in subscription:
                    if self._on_capability_change:
                        await self._on_capability_change(self.config.name)
        except asyncio.CancelledError:
            raise
        except Exception as error:
            logger.debug("MCP %s change subscription unavailable: %s", self.config.name, error)

    @asynccontextmanager
    async def _open_client(self) -> AsyncIterator[Any]:
        try:
            from mcp import Client
            from mcp.client.stdio import StdioServerParameters, stdio_client
            from mcp.client.streamable_http import streamable_http_client
        except (ImportError, ModuleNotFoundError) as error:
            raise MCPUnavailableError(
                "MCP support requires the official Python SDK v2 (`mcp>=2,<3`)"
            ) from error

        if not hasattr(Client, "listen"):
            raise MCPUnavailableError(
                "MCP Python SDK v2 is required; install `mcp>=2,<3`"
            )

        if self.config.transport == "stdio":
            resolved_env = self.config.resolved_env()
            params = StdioServerParameters(
                command=str(self.config.command),
                args=list(self.config.args),
                env=resolved_env or None,
                cwd=self.config.cwd,
            )
            errlog = _RedactingWriter(sys.stderr, list(resolved_env.values()))
            try:
                transport = stdio_client(params, errlog=errlog)
                # The actor's request timeout is the single public deadline.
                # Disabling the SDK's overlapping read timeout avoids races in
                # which its internal cancellation escapes as CancelledError.
                async with Client(
                    transport,
                    read_timeout_seconds=None,
                    cache=None,
                ) as client:
                    yield client
            finally:
                errlog.close()
            return

        try:
            import httpx2
        except ImportError as error:
            raise MCPUnavailableError("MCP HTTP transport requires `httpx2`") from error

        # Bound connection establishment here; per-operation deadlines remain
        # owned by MCPServerActor.request for consistent stdio/HTTP semantics.
        timeout = httpx2.Timeout(None, connect=self.config.connect_timeout)
        async with httpx2.AsyncClient(
            headers=self.config.resolved_headers(),
            timeout=timeout,
            follow_redirects=True,
        ) as http_client:
            transport = streamable_http_client(str(self.config.url), http_client=http_client)
            async with Client(transport, read_timeout_seconds=None, cache=None) as client:
                yield client

    def snapshot(self) -> dict[str, Any]:
        return {
            "name": self.config.name,
            "state": self.state.value,
            "transport": self.config.transport,
            "protocol_version": self.protocol_version,
            "server_info": self.server_info,
            "capabilities": self._capability_names(),
            "error": self.error,
        }
