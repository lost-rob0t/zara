---
name: desktop-app-control
description: Launch and control configured desktop applications through Zara's canonical command-routing path.
metadata:
  zara-schema: "1"
  zara-domain: "desktop"
  zara-selectors: "desktop application app-launch launch-app window-control desktop-app"
  zara-priority: "84"
  zara-max-tokens: "800"
  zara-paths: "zara/wake.py modules kb"
  zara-always-on: "false"
---
# Desktop application control

For deterministic app and desktop commands, prefer Zara's existing command router rather than improvising a new launcher.

- Route ordinary launch/open/start commands through `query_prolog` using `command_loop:handle_command("<exact user text>")` when that tool is present.
- Preserve the exact user command text so configured aliases and application mappings can resolve normally.
- Direct service tools are allowed to win when they are more specific than the legacy command router.
- Do not claim an application opened merely because an intent matched. Relay the execution result from the command path.
- Do not use arbitrary shell execution as the first choice when Zara already has a deterministic configured app mapping.
- Keep user application mappings and machine-specific configuration outside committed source when they contain local or private values.
