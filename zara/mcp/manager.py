"""MCP capability manager and LangChain adapter."""

from __future__ import annotations

import asyncio
import hashlib
import json
import logging
import re
import time
from dataclasses import dataclass
from typing import Any, Iterable

from langchain_core.tools import StructuredTool

from .config import MCPConfigStore, MCPServerConfig
from .session import MCPRequestError, MCPServerActor, normalize_tool_result

logger = logging.getLogger(__name__)

_MODEL_NAME_RE = re.compile(r"[^A-Za-z0-9_-]+")


@dataclass(frozen=True)
class MCPToolBinding:
    identity: str
    model_name: str
    server: str
    remote_name: str
    description: str
    input_schema: dict[str, Any]


class MCPManager:
    """Own configured MCP actors and adapt their capabilities into Zara."""

    def __init__(self, zara_config: Any, tool_registry: Any | None = None):
        self.zara_config = zara_config
        self.tool_registry = tool_registry
        self.store = MCPConfigStore(zara_config)
        self.configs: dict[str, MCPServerConfig] = self.store.load()
        raw = self.store.raw()
        self.refresh_interval = float(raw.get("refresh_interval", 30.0))
        self.actors: dict[str, MCPServerActor] = {}
        self.tools: dict[str, MCPToolBinding] = {}
        self.resources: dict[str, list[dict[str, Any]]] = {}
        self.resource_templates: dict[str, list[dict[str, Any]]] = {}
        self.prompts: dict[str, list[dict[str, Any]]] = {}
        self._registered_model_names: set[str] = set()
        self._last_refresh = 0.0
        self._start_lock = asyncio.Lock()
        self._refresh_lock = asyncio.Lock()
        self._started = False
        self._dirty_servers: set[str] = set()

    @property
    def enabled_configs(self) -> list[MCPServerConfig]:
        return [config for config in self.configs.values() if config.enabled]

    async def ensure_started(self) -> None:
        if not self.enabled_configs:
            self._started = True
            return
        async with self._start_lock:
            if not self._started:
                for config in self.enabled_configs:
                    self.actors[config.name] = MCPServerActor(
                        config,
                        on_capability_change=self._mark_dirty,
                    )
                results = await asyncio.gather(
                    *(actor.start() for actor in self.actors.values()),
                    return_exceptions=True,
                )
                for name, result in zip(self.actors, results):
                    if isinstance(result, BaseException):
                        logger.warning("MCP server %s unavailable: %s", name, result)
                self._started = True
                await self.refresh(force=True)
                return

        restarted = False
        for config in self.enabled_configs:
            actor = self.actors.get(config.name)
            if actor is None or actor.state.value in {"failed", "stopped"}:
                if actor is not None:
                    await actor.stop()
                replacement = MCPServerActor(
                    config,
                    on_capability_change=self._mark_dirty,
                )
                self.actors[config.name] = replacement
                try:
                    await replacement.start()
                except Exception as error:
                    logger.warning("MCP server %s reconnect failed: %s", config.name, error)
                self._dirty_servers.add(config.name)
                restarted = True

        if restarted or self._dirty_servers or time.monotonic() - self._last_refresh >= self.refresh_interval:
            await self.refresh(force=restarted or bool(self._dirty_servers))

    async def _mark_dirty(self, server: str) -> None:
        self._dirty_servers.add(server)

    async def refresh(self, *, force: bool = False) -> None:
        if not self.actors:
            return
        if not force and time.monotonic() - self._last_refresh < self.refresh_interval:
            return
        async with self._refresh_lock:
            server_names = set(self._dirty_servers) if self._dirty_servers else set(self.actors)
            if force and not server_names:
                server_names = set(self.actors)
            results = await asyncio.gather(
                *(self._discover_server(name) for name in sorted(server_names)),
                return_exceptions=True,
            )
            for name, result in zip(sorted(server_names), results):
                if isinstance(result, BaseException):
                    logger.warning("MCP capability refresh failed for %s: %s", name, result)
            self._dirty_servers.difference_update(server_names)
            self._last_refresh = time.monotonic()
            self._sync_tool_registry()

    async def _discover_server(self, server: str) -> None:
        actor = self.actors[server]
        if actor.state.value != "ready":
            return
        caps = actor.capabilities
        discovered_tools: list[Any] = []
        resources: list[Any] = []
        templates: list[Any] = []
        prompts: list[Any] = []

        if getattr(caps, "tools", None) is not None:
            discovered_tools = await self._collect_pages(actor, "list_tools", "tools")
        if getattr(caps, "resources", None) is not None:
            resources = await self._collect_pages(actor, "list_resources", "resources")
            templates = await self._collect_pages(
                actor,
                "list_resource_templates",
                "resource_templates",
            )
        if getattr(caps, "prompts", None) is not None:
            prompts = await self._collect_pages(actor, "list_prompts", "prompts")

        # Replace only this server's records. A failed peer cannot erase another.
        self.tools = {identity: binding for identity, binding in self.tools.items() if binding.server != server}
        for remote in discovered_tools:
            binding = self._make_binding(server, remote)
            self.tools[binding.identity] = binding
        self.resources[server] = [self._dump(item) for item in resources]
        self.resource_templates[server] = [self._dump(item) for item in templates]
        self.prompts[server] = [self._dump(item) for item in prompts]
        await actor.update_resource_subscriptions(
            [str(item.get("uri")) for item in self.resources[server] if item.get("uri")]
        )
        logger.info(
            "MCP discovered server=%s tools=%d resources=%d templates=%d prompts=%d",
            server,
            len(discovered_tools),
            len(resources),
            len(templates),
            len(prompts),
        )

    async def _collect_pages(self, actor: MCPServerActor, operation: str, attr: str) -> list[Any]:
        items: list[Any] = []
        cursor: str | None = None
        while True:
            kwargs = {"cursor": cursor} if cursor else {}
            result = await actor.request(operation, **kwargs)
            items.extend(getattr(result, attr, []) or [])
            cursor = getattr(result, "next_cursor", None)
            if not cursor:
                return items

    @staticmethod
    def _dump(value: Any) -> dict[str, Any]:
        if hasattr(value, "model_dump"):
            return value.model_dump(mode="json", by_alias=False, exclude_none=True)
        if isinstance(value, dict):
            return dict(value)
        return {"value": str(value)}

    def _make_binding(self, server: str, remote: Any) -> MCPToolBinding:
        remote_name = str(remote.name)
        identity = f"mcp.{server}.{remote_name}"
        base = _MODEL_NAME_RE.sub("_", f"mcp__{server}__{remote_name}").strip("_")
        if not base:
            base = "mcp_tool"
        model_name = base[:63]
        occupied = {
            binding.model_name
            for binding in self.tools.values()
            if binding.identity != identity
        }
        if self.tool_registry is not None:
            occupied.update(
                name
                for name in self.tool_registry.list_tools()
                if name not in self._registered_model_names
            )
        if model_name in occupied:
            digest = hashlib.sha1(identity.encode("utf-8")).hexdigest()[:8]
            model_name = f"{model_name[:54]}_{digest}"
        description = getattr(remote, "description", None) or getattr(remote, "title", None) or remote_name
        schema = getattr(remote, "input_schema", None) or {"type": "object", "properties": {}}
        return MCPToolBinding(
            identity=identity,
            model_name=model_name,
            server=server,
            remote_name=remote_name,
            description=f"[MCP: {server}] {description}",
            input_schema=dict(schema),
        )

    def _sync_tool_registry(self) -> None:
        if self.tool_registry is None:
            return
        for model_name in list(self._registered_model_names):
            self.tool_registry.unregister_tool(model_name)
        self._registered_model_names.clear()

        for binding in sorted(self.tools.values(), key=lambda item: item.identity):
            tool = self._build_langchain_tool(binding)
            self.tool_registry.register_tool(tool)
            self._registered_model_names.add(binding.model_name)

        if any(self.resources.values()) or any(self.resource_templates.values()):
            for resource_tool in (self._build_resource_list_tool(), self._build_resource_tool()):
                self.tool_registry.register_tool(resource_tool)
                self._registered_model_names.add(resource_tool.name)
        if any(self.prompts.values()):
            for prompt_tool in (self._build_prompt_list_tool(), self._build_prompt_tool()):
                self.tool_registry.register_tool(prompt_tool)
                self._registered_model_names.add(prompt_tool.name)

    def _build_langchain_tool(self, binding: MCPToolBinding) -> StructuredTool:
        async def invoke(**arguments: Any) -> dict[str, Any]:
            return await self.call_tool(binding.server, binding.remote_name, arguments)

        return StructuredTool.from_function(
            coroutine=invoke,
            name=binding.model_name,
            description=binding.description,
            args_schema=binding.input_schema,
        )

    def _build_resource_list_tool(self) -> StructuredTool:
        async def list_resources(server: str | None = None) -> dict[str, Any]:
            names = [server] if server else sorted(self.resources)
            return {
                name: {
                    "resources": self.resources.get(name, []),
                    "templates": self.resource_templates.get(name, []),
                }
                for name in names
                if name in self.actors
            }

        return StructuredTool.from_function(
            coroutine=list_resources,
            name="mcp__list_resources",
            description="List MCP resources and templates, optionally for one server.",
        )

    def _build_resource_tool(self) -> StructuredTool:
        async def read_resource(server: str, uri: str) -> dict[str, Any]:
            return await self.read_resource(server, uri)

        return StructuredTool.from_function(
            coroutine=read_resource,
            name="mcp__read_resource",
            description=(
                "Read an MCP resource after selecting it from the MCP capability context. "
                "Resources remain MCP resources; this is Zara's generic access operation."
            ),
        )

    def _build_prompt_list_tool(self) -> StructuredTool:
        async def list_prompts(server: str | None = None) -> dict[str, Any]:
            names = [server] if server else sorted(self.prompts)
            return {name: self.prompts.get(name, []) for name in names if name in self.actors}

        return StructuredTool.from_function(
            coroutine=list_prompts,
            name="mcp__list_prompts",
            description="List MCP prompts, optionally for one server.",
        )

    def _build_prompt_tool(self) -> StructuredTool:
        async def get_prompt(
            server: str,
            name: str,
            arguments: dict[str, str] | None = None,
        ) -> dict[str, Any]:
            return await self.get_prompt(server, name, arguments or {})

        return StructuredTool.from_function(
            coroutine=get_prompt,
            name="mcp__get_prompt",
            description=(
                "Render an MCP prompt advertised in the MCP capability context. "
                "The result preserves prompt message roles and content."
            ),
        )

    async def call_tool(self, server: str, tool: str, arguments: dict[str, Any]) -> dict[str, Any]:
        actor = self._actor(server)
        logger.info("MCP call %s.%s started", server, tool)
        try:
            result = await actor.request("call_tool", tool, arguments)
        except MCPRequestError:
            await self._recover_failed_actor(server)
            raise
        normalized = normalize_tool_result(result)
        normalized.update({"mcp_server": server, "tool": tool})
        logger.info("MCP call %s.%s completed error=%s", server, tool, normalized["is_error"])
        return normalized

    async def read_resource(self, server: str, uri: str) -> dict[str, Any]:
        actor = self._actor(server)
        try:
            result = await actor.request("read_resource", uri)
        except MCPRequestError:
            await self._recover_failed_actor(server)
            raise
        return {
            "mcp_server": server,
            "uri": uri,
            "contents": self._dump_list(getattr(result, "contents", [])),
            "meta": self._dump_optional(getattr(result, "meta", None)),
        }

    async def get_prompt(self, server: str, name: str, arguments: dict[str, str]) -> dict[str, Any]:
        actor = self._actor(server)
        try:
            result = await actor.request("get_prompt", name, arguments)
        except MCPRequestError:
            await self._recover_failed_actor(server)
            raise
        return {
            "mcp_server": server,
            "prompt": name,
            "description": getattr(result, "description", None),
            "messages": self._dump_list(getattr(result, "messages", [])),
            "meta": self._dump_optional(getattr(result, "meta", None)),
        }

    async def _recover_failed_actor(self, server: str) -> None:
        """Reconnect a failed server for future calls without replaying this one."""
        actor = self.actors.get(server)
        config = self.configs.get(server)
        if actor is None or config is None or actor.state.value not in {"failed", "stopped"}:
            return
        await actor.stop()
        replacement = MCPServerActor(config, on_capability_change=self._mark_dirty)
        self.actors[server] = replacement
        try:
            await replacement.start()
        except Exception as error:
            logger.warning("MCP server %s recovery failed: %s", server, error)
            return
        self._dirty_servers.add(server)
        try:
            await self.refresh(force=True)
        except Exception as error:
            logger.warning("MCP server %s post-reconnect refresh failed: %s", server, error)

    def _actor(self, server: str) -> MCPServerActor:
        actor = self.actors.get(server)
        if actor is None:
            raise MCPRequestError(f"Unknown or disabled MCP server {server!r}")
        return actor

    @staticmethod
    def _dump_list(values: Iterable[Any]) -> list[Any]:
        result = []
        for value in values:
            if hasattr(value, "model_dump"):
                result.append(value.model_dump(mode="json", by_alias=False, exclude_none=True))
            elif isinstance(value, dict):
                result.append(dict(value))
            else:
                result.append(str(value))
        return result

    @staticmethod
    def _dump_optional(value: Any) -> Any:
        if value is None:
            return None
        if hasattr(value, "model_dump"):
            return value.model_dump(mode="json", by_alias=False, exclude_none=True)
        return value

    def system_context(self) -> str | None:
        if not self.actors:
            return None
        lines = [
            "# MCP capability routing",
            "MCP capabilities below are native Zara capabilities for this turn.",
            "When a request clearly matches an MCP tool, resource, or prompt, prefer it over query_prolog even if the utterance starts with search/find/list/show/lookup.",
            "Never invent an MCP capability that is not listed.",
        ]
        if self.tools:
            lines.append("MCP tools:")
            for binding in sorted(self.tools.values(), key=lambda item: item.identity):
                lines.append(f"- {binding.model_name}: {binding.description}")
        resource_lines = []
        for server, items in sorted(self.resources.items()):
            for item in items:
                uri = item.get("uri")
                name = item.get("name") or item.get("title") or uri
                resource_lines.append(f"- {server}: {name} ({uri})")
        for server, items in sorted(self.resource_templates.items()):
            for item in items:
                uri = item.get("uri_template") or item.get("uriTemplate")
                name = item.get("name") or item.get("title") or uri
                resource_lines.append(f"- {server}: {name} ({uri}) [template]")
        if resource_lines:
            lines.append("MCP resources (use mcp__list_resources for the full list, mcp__read_resource to read):")
            lines.extend(resource_lines[:25])
            if len(resource_lines) > 25:
                lines.append(f"- ... {len(resource_lines) - 25} more resource entries")
        prompt_lines = []
        for server, items in sorted(self.prompts.items()):
            for item in items:
                prompt_lines.append(f"- {server}: {item.get('name')} — {item.get('description', '')}")
        if prompt_lines:
            lines.append("MCP prompts (use mcp__list_prompts for the full list, mcp__get_prompt to render):")
            lines.extend(prompt_lines[:25])
            if len(prompt_lines) > 25:
                lines.append(f"- ... {len(prompt_lines) - 25} more prompts")
        failures = [actor for actor in self.actors.values() if actor.state.value == "failed"]
        if failures:
            lines.append("Unavailable MCP servers:")
            lines.extend(f"- {actor.config.name}: {actor.error}" for actor in failures)
        return "\n".join(lines)

    def status(self) -> list[dict[str, Any]]:
        snapshots = []
        for name, config in sorted(self.configs.items()):
            actor = self.actors.get(name)
            if actor is None:
                snapshots.append(
                    {
                        "name": name,
                        "state": "disabled" if not config.enabled else "configured",
                        "transport": config.transport,
                        "protocol_version": None,
                        "server_info": None,
                        "capabilities": [],
                        "error": None,
                    }
                )
            else:
                snapshots.append(actor.snapshot())
        return snapshots

    async def shutdown(self) -> None:
        actors = list(self.actors.values())
        await asyncio.gather(*(actor.stop() for actor in actors), return_exceptions=True)
        self.actors.clear()
        if self.tool_registry is not None:
            for model_name in list(self._registered_model_names):
                self.tool_registry.unregister_tool(model_name)
        self._registered_model_names.clear()
        self._started = False
