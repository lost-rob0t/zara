---
name: python-runtime
description: Zara Python runtime architecture and conventions. Use when changing the Python runtime, daemon, agent lifecycle, voice runtime, concurrency, or Python modules.
metadata:
  zara-schema: "1"
  zara-domain: "python"
  zara-language: "python"
  zara-selectors: "python runtime daemon lifecycle concurrency agent voice .py"
  zara-priority: "80"
  zara-max-tokens: "1200"
  zara-paths: "zara/ t/ scripts/"
  zara-always-on: "false"
---
# Python runtime

Keep one canonical runtime path. Reuse RuntimeHost, TurnCoordinator, typed runtime events/commands, AgentManager, and existing service-plugin lifecycle instead of creating parallel managers.

Treat ownership and cancellation as part of the API. Long-lived services need bounded queues/workers, explicit startup/shutdown, and deterministic failure propagation. A cancelled or stale turn must not mutate newer state.

Use Nix-owned dependencies and repository configuration surfaces. Do not hide runtime dependencies in ad-hoc install steps.

Add deterministic tests for lifecycle, cancellation, invalid configuration, degraded providers, and packaging when behavior crosses those boundaries.
