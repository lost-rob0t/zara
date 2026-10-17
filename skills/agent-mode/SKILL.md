---
name: agent-mode
description: Use Zara agent-mode for recurring tasks, proactive questions, autonomous actions, and background task lifecycle.
metadata:
  zara-schema: "1"
  zara-domain: "agent-mode"
  zara-selectors: "agent-mode recurring recurring-task periodic proactive random-questions background-task autonomous"
  zara-priority: "94"
  zara-max-tokens: "950"
  zara-paths: "zara/agent_mode zara/plugins"
  zara-always-on: "false"
---
# Agent mode

Agent mode owns work that must happen again later or autonomously rather than as a one-shot todo.

- Use `schedule_recurring_task` for recurring, periodic, interval-based, or autonomous background work when that service tool is registered.
- Use `list_recurring_tasks` and `cancel_recurring_task` to inspect or stop recurring work.
- Use `set_random_questions` only when the user explicitly wants proactive/random questions enabled, disabled, or adjusted.
- Use `agent_mode_status` for status questions when available.
- Do not turn a recurring agent task into an ordinary todo just because the request contains words such as "schedule" or "task".
- Do not turn a one-time todo/reminder into a recurring autonomous job.
- Agent-mode hooks receive the current personality/context at execution time. Persist references or fingerprints when required, not raw secret-bearing prompt state.
- Barge-in, TTS, and microphone ownership remain voice-runtime concerns; do not create a second audio capture path for autonomous speech.
