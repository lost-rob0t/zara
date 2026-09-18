# Changelog

## Unreleased

### Fixed

- Android local output-policy checks now use a runtime-neutral code splitter, avoiding Trealla's atom-only `split/4` builtin so the Prolog policy can run on both supported native engines.
