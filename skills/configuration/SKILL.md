---
name: configuration
description: Change Zara configuration safely using validated TOML, XDG paths, plugin namespaces, and secret-safe overrides.
metadata:
  zara-schema: "1"
  zara-domain: "configuration"
  zara-selectors: "config.toml zara-config configuration xdg-config config-file"
  zara-priority: "87"
  zara-max-tokens: "900"
  zara-paths: "zara/config.py docs/configuration.org docs/config"
  zara-always-on: "false"
---
# Configuration

Treat Zara configuration as a validated runtime contract.

- Prefer the normal XDG configuration location and Zara's config loader instead of hard-coded machine-specific paths in source.
- Keep secrets, access tokens, API keys, and private endpoints out of Git. Use supported environment/config secret locations instead.
- Preserve typed values: booleans must remain booleans, numeric bounds must be respected, and list-valued settings must remain lists.
- New configuration sections need defaults, validation, and tests for invalid values as well as the happy path.
- Plugin-specific settings belong in that plugin's namespace so one plugin cannot accidentally read another plugin's secrets.
- Runtime feature toggles should disable the whole owned surface coherently rather than hiding one entrypoint while leaving tools/intents active.
- When changing config semantics, update the documented copyable configuration examples together with tests.
