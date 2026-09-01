---
name: python-prolog
description: Zara Python to Prolog boundary and result-contract rules. Use when changing pyswip calls, PrologEngine, command results, cross-language errors, or Python/Prolog integration.
metadata:
  zara-schema: "1"
  zara-domain: "integration"
  zara-selectors: "python prolog boundary pyswip prologengine result contract integration"
  zara-priority: "95"
  zara-max-tokens: "1400"
  zara-paths: "zara/prolog_engine.py zara/agent/ kb/ modules/"
  zara-dependencies: "python-runtime prolog-modules"
  zara-always-on: "false"
---
# Python and Prolog boundary

Keep the cross-language boundary narrow and typed in behavior even when pyswip returns Python dictionaries/lists. Normalize success, no-match, and error results before higher layers consume them.

Python owns process/runtime lifecycle and external side effects. Prolog owns deterministic symbolic routing and predicate logic. Do not duplicate one side's state machine on the other side.

Escape/query values safely instead of interpolating untrusted text into executable Prolog source. Preserve exact command text only through the existing command-loop contract designed for it.

Every boundary change needs regression tests on both sides: Python result handling and Prolog predicate/resolver behavior.
