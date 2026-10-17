---
name: observability-debugging
description: Diagnose Zara runtime failures from typed events, logs, test evidence, health state, and exact-head behavior.
metadata:
  zara-schema: "1"
  zara-domain: "debugging"
  zara-selectors: "debug debugging logs runtime-failure health-check trace latency"
  zara-priority: "83"
  zara-max-tokens: "900"
  zara-paths: "zara/runtime zara/latency.py scripts/test-all.sh"
  zara-always-on: "false"
---
# Observability and debugging

Debug Zara from evidence rather than from guessed control flow.

- Correlate a user turn with its turn ID/conversation ID and typed runtime events before blaming an unrelated subsystem.
- Distinguish model failure, tool failure, approval rejection, cancellation, timeout, plugin failure, MCP failure, and deterministic command failure.
- Preserve exact error messages and decisive log lines while avoiding unnecessary secret-bearing payload dumps.
- For latency work, separate request start, first token, first sentence, final token, tool time, and voice playback boundaries where those metrics exist.
- Prefer deterministic focused tests that reproduce the failure, then run the full repository gate before declaring the fix complete.
- A green run for an older commit is stale evidence. Verification must correspond to the exact candidate head being evaluated.
