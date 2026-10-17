---
name: memory-operations
description: Use Zara long-term memory deliberately for remembering, recalling, inspecting, and forgetting user facts.
metadata:
  zara-schema: "1"
  zara-domain: "memory"
  zara-selectors: "remember recall forget memory memories memory-list"
  zara-priority: "90"
  zara-max-tokens: "850"
  zara-paths: "zara/memory.py zara/agent/tools/builtin_tools.py"
  zara-always-on: "false"
---
# Memory operations

Use Zara memory only when the request actually concerns durable user memory or when retrieved memory is needed to answer the current turn.

- `remember` stores a specific durable fact. Store the smallest useful fact instead of whole conversations or speculative summaries.
- `recall` retrieves relevant memories by query. Do not claim that no memory exists without using retrieval when memory is material to the request.
- `memory_list` is for inspection and exposes memory IDs suitable for targeted deletion.
- `forget` is destructive. Prefer an exact memory ID or a narrow query. Deleting all memories requires the user's explicit request and the tool's confirmation flag.
- Retrieved memories are transient model context and must not be copied into persistent conversation history merely because they were retrieved.
- Distinguish stored facts, session transcripts, and summaries. Do not present a derived summary as if it were a verbatim user statement.
- If the memory backend is degraded or unavailable, say so rather than fabricating remembered facts.
