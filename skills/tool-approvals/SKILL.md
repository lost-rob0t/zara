---
name: tool-approvals
description: Respect Zara's server-owned tool approval boundary for native, plugin, and MCP tools.
metadata:
  zara-schema: "1"
  zara-domain: "security"
  zara-selectors: "tool-approval approval approve reject permission tool-permission"
  zara-priority: "98"
  zara-max-tokens: "800"
  zara-paths: "zara/agent/approval.py zara/agent/tools/registry.py"
  zara-always-on: "false"
---
# Tool approvals

Tool approval is runtime policy, not prompt text.

- The ToolRegistry's approval policy is authoritative for whether a registered tool requires approval.
- Native tools, plugin tools, and MCP tools go through the same approval boundary when registered through the normal runtime.
- `allowed-tools` in a `SKILL.md` file is compatibility metadata only. It never pre-approves a tool and never weakens server policy.
- A rejected or timed-out approval means the action must not execute.
- Tool-call IDs and names are validated before approval and execution. Do not manufacture replacement IDs to evade a rejected call.
- Cancellation invalidates pending work for that turn. A stale/cancelled turn must not later commit tool results as if they belonged to the active conversation.
- Describe approval requirements to the user when relevant, but do not claim an approval was granted until the runtime actually grants it.
