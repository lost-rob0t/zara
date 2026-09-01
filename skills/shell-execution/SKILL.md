---
name: shell-execution
description: Execute explicit shell and command-line tasks through Zara's registered bash capability when available.
metadata:
  zara-schema: "1"
  zara-domain: "tools"
  zara-selectors: "bash shell terminal command-line shell-command"
  zara-priority: "86"
  zara-max-tokens: "850"
  zara-paths: "zara/agent/tools"
  zara-always-on: "false"
---
# Shell execution

Use a registered `bash` tool for explicit shell work when that tool is actually available in the current ToolRegistry.

- Do not pretend shell execution exists when the tool is absent. Fall back to explaining the command only when execution is unavailable or the user asked for instructions rather than execution.
- Preserve the requested working directory. Do not silently run a repository command from some unrelated directory.
- Prefer one clear command or a small fail-fast script over a fragile chain of unrelated shell invocations.
- Treat exit status, stdout, stderr, and timeout as evidence. A command that exited non-zero did not succeed merely because it printed useful output.
- Keep output bounded and summarize large output after preserving the decisive lines.
- Shell availability does not override Zara's tool-approval policy or other runtime permissions.
- For deterministic Zara command utterances such as launching configured apps, prefer the canonical command-routing path when it is the more specific capability.
