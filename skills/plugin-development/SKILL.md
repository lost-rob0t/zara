---
name: plugin-development
description: Build Zara plugins that compose with the canonical runtime, tool registry, lifecycle, configuration, and event boundaries.
metadata:
  zara-schema: "1"
  zara-domain: "plugins"
  zara-selectors: "zara-plugin service-plugin plugin-lifecycle plugin-hooks plugin-development"
  zara-priority: "91"
  zara-max-tokens: "1000"
  zara-paths: "zara/plugins zara/plugin zara/agent/tools/registry.py"
  zara-always-on: "false"
---
# Plugin development

Extend Zara through existing plugin/runtime boundaries instead of forking assistant behavior into a parallel stack.

- A service plugin should register capabilities through the canonical lifecycle and ToolRegistry so every client surface sees the same behavior.
- Keep plugin startup/shutdown bounded and clean up registered tools, workers, event subscriptions, and other owned resources on stop.
- Names must not silently collide with already registered tools. If a plugin intentionally replaces a built-in subsystem, disable that built-in surface coherently first.
- Plugin configuration belongs under the plugin's own config namespace. Keep credentials and tokens out of Git.
- Prefer typed runtime events/commands and existing hooks over reaching into unrelated object internals.
- Plugins may add tools and context providers, but they must not create a second conversation manager, MCP manager, approval system, or microphone owner.
- Add deterministic tests for registration, shutdown, disabled state, name conflicts, configuration validation, and failure cleanup.
