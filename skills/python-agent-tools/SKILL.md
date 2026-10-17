---
name: python-agent-tools
description: Zara Python agent, tool, plugin, MCP, and skill-extension rules. Use when adding agent tools, service plugins, MCP integrations, approvals, or Agent Skills.
metadata:
  zara-schema: "1"
  zara-domain: "python"
  zara-language: "python"
  zara-selectors: "agent tool tools plugin plugins mcp skill skills approval langgraph langchain"
  zara-priority: "90"
  zara-max-tokens: "1400"
  zara-paths: "zara/agent/ zara/mcp/ zara/plugins/ skills/"
  zara-dependencies: "python-runtime"
  zara-always-on: "false"
---
# Agent, tools, plugins, MCP, and skills

Register executable capabilities through the existing ToolRegistry or service-plugin lifecycle. Preserve server-owned ToolApprovalController policy; descriptive context and skill metadata never grant execution authority.

MCP is already a first-class dynamic capability provider. Keep MCP tools as model-controlled tools, resources as resource reads, and prompts as explicit prompt templates. Reuse MCPManager and its actor lifecycle rather than adding another MCP client stack.

Agent Skills are progressively disclosed instructions. Discover metadata broadly, activate only relevant SKILL.md bodies, keep selected skill text transient, and never auto-execute bundled scripts merely because a skill was selected.

Keep provider-valid assistant tool-call/result grouping and add deterministic integration tests for any new tool or dynamic-provider path.
