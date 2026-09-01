---
name: file-operations
description: Safely inspect, diff, list, and write files using Zara's bounded file tools.
metadata:
  zara-schema: "1"
  zara-domain: "files"
  zara-selectors: "filesystem file-tools read-file write-file diff-file list-dir"
  zara-priority: "88"
  zara-max-tokens: "900"
  zara-paths: "zara/agent/tools/file_tools.py"
  zara-always-on: "false"
---
# File operations

Use the native file tools when they are enabled: `read_file`, `list_dir`, `diff_file`, and `write_file`.

- Stay inside configured readable and writable roots. A rejected path is a policy result, not a hint to bypass the policy with another path trick.
- Read before overwriting an existing file unless the user has already supplied the complete authoritative replacement.
- Prefer `diff_file` before a substantial replacement when seeing the delta improves correctness.
- `write_file` is an atomic complete-file write. Set overwrite only when replacing an existing regular file is intended.
- Never use symlinks or path traversal to escape configured roots.
- Respect bounded file size, line, and directory-entry limits. If a file is too large, narrow the task rather than pretending the full file was inspected.
- Report the actual tool result. Do not say a file was written, changed, or read if the corresponding tool failed or is unavailable.
