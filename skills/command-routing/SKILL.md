---
name: command-routing
description: Route deterministic Zara commands through the existing Prolog command pipeline while preserving agent fallback.
metadata:
  zara-schema: "1"
  zara-domain: "routing"
  zara-language: "prolog"
  zara-selectors: "command-router command-routing prolog-command handle-command deterministic-command"
  zara-priority: "96"
  zara-max-tokens: "900"
  zara-paths: "main.pl modules/command_loop.pl modules kb/intents.pl"
  zara-always-on: "false"
---
# Command routing

Preserve Zara's Prolog-first deterministic command path.

- For ordinary command utterances, call `query_prolog` once with `command_loop:handle_command("<exact user text>")` when the Prolog tool is available.
- Explicit service capabilities are more specific and may bypass the generic command router: for example recurring agent tasks or direct TTS requests when their service tools are registered.
- Conversational questions and ordinary chat should remain agent conversation. Do not route free-form conversation through Prolog "just in case".
- If deterministic resolution fails or returns no useful match, fall back to the agent path rather than inventing a command result.
- Use module-qualified Prolog predicates for structured inspection. Because `query_once` returns one solution, use `findall/3` when multiple results are required.
- Side-effecting command predicates should only run when the user actually asked for the action.
