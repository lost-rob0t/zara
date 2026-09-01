---
name: prolog-modules
description: Zara SWI-Prolog module and predicate conventions. Use when changing Prolog modules, predicates, knowledge-base files, module imports, or deterministic Prolog tests.
metadata:
  zara-schema: "1"
  zara-domain: "prolog"
  zara-language: "prolog"
  zara-selectors: "prolog module modules predicate predicates swi swipl knowledge-base kb .pl"
  zara-priority: "70"
  zara-max-tokens: "1200"
  zara-paths: "kb/ modules/ main.pl"
  zara-always-on: "false"
---
# Prolog modules and predicates

Keep module ownership explicit and imports acyclic. Prefer small predicates with clear input/output modes and deterministic behavior where command routing depends on them.

Preserve the installed-resource layout used by main.pl and setup/Nix packaging. A module that works only from the source checkout is not shipped.

Test module loading plus success, failure, and malformed-input cases with SWI-Prolog. Keep Python orchestration out of Prolog when the behavior belongs to runtime lifecycle or external I/O ownership.
