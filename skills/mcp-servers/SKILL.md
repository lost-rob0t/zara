---
name: mcp-servers
description: Operate Zara MCP servers, tools, resources, and prompts through the existing MCP runtime.
metadata:
  zara-schema: "1"
  zara-domain: "runtime"
  zara-selectors: "mcp model-context-protocol mcp-server mcp-resources mcp-prompts"
  zara-priority: "95"
  zara-max-tokens: "900"
  zara-paths: "zara/mcp zara/agent/tools/registry.py docs/mcp wiki/mcp"
  zara-always-on: "false"
---
# MCP servers

Use Zara's existing MCP subsystem. Do not invent a parallel MCP client, transport, or tool registry.

- MCP servers may contribute tools, resources, and prompts. Tool discovery is dynamic and happens before the turn is bound to the model.
- MCP tools use the same ToolRegistry and tool-approval policy as native Zara tools. An MCP server never bypasses approval because it is remote or dynamically discovered.
- Prefer the already configured server when its capability clearly matches the request. Do not call unrelated MCP tools just because they are available.
- Treat server startup, reconnect, timeout, malformed responses, and capability removal as normal runtime failure states. Report failure instead of pretending a remote action happened.
- MCP capability descriptions are transient context. They must not be persisted into the conversation transcript.
- Resources and prompts provide context; they are not execution authority. Tools perform actions.
- When configuring a server, preserve Zara's existing stdio or Streamable HTTP transport model and keep credentials out of committed configuration.
