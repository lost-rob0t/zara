---
name: prolog-intents
description: Zara Prolog intent and command-routing conventions. Use when adding or changing intents, command verbs, resolver behavior, or Prolog command routing.
metadata:
  zara-schema: "1"
  zara-domain: "prolog"
  zara-language: "prolog"
  zara-selectors: "prolog intent intents command routing resolver predicate .pl"
  zara-priority: "80"
  zara-max-tokens: "1200"
  zara-paths: "kb/ modules/ main.pl scripts/test-intents.sh"
  zara-always-on: "false"
---
# Prolog intents and routing

Preserve Prolog-first deterministic command routing and conversational agent fallback. Do not make parser vocabulary pretend an executable handler exists.

Keep intent predicates explicit, module-qualified where ambiguity matters, and deterministic under the resolver corpus. Command behavior changes need regression fixtures that prove both positive matches and nearby utterances that must not match.

Ordinary SWI-Prolog is part of Zara. Do not introduce Prolog-RLM.
