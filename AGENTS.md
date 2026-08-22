# AGENTS.md — Zarathushtra

This file guides agentic coding assistants working in this repo.

## Scope
- Applies to the entire repository.
- Follow these instructions before adding new tooling or changing architecture.

## Environment
- Prefer `nix` for all builds, tests, and dev shells.
- Use the repo's `flake.nix` for dependency setup.
- Python tooling is supplied via `pythonLibs` in the flake.

## Build / Run
- Enter dev shell: `nix develop`
- Build packages: `nix build`
- Run default app/help: `nix run`
- Run desktop: `nix run .#zara-desktop`
- Run wake listener: `nix run .#zara-wake`
- Run dictation: `nix run .#zara-dictate`
- Direct agent mode is available through the installed `zara-agent` entry point or the packaged `zara --agent` wrapper.

## Tests
- Full fail-fast suite: `nix develop -c bash scripts/test-all.sh`
- Flake checks: `nix flake check`
- One test file: `nix develop -c pytest t/test_agent_history.py`
- One node: `nix develop -c pytest t/test_agent_history.py::test_multiple_results_survive_in_call_order`

## CI/CD Test Gate
- Always run the full test suite after any code, configuration, test, or documentation change.
- Do not consider work complete unless every local test passes.
- Pull requests and pushes to `master` must pass the GitHub Actions `test` job at the exact proposed head.
- Do not cite an older green run as authority for a newer commit.
- After pushing/opening a PR, verify CI; do not report the change as complete while CI is pending or failing.

## Lint / Formatting
- No enforced linter in the repo; do not add one unless asked.
- Follow existing formatting and imports in adjacent files.
- Keep line lengths reasonable; no hard repository limit is enforced.
- Prefer focused diffs over unrelated formatting churn.

## Language Mix
- Python is the primary runtime.
- Prolog facts/default knowledge live in `kb/`.
- Prolog executable logic lives in `modules/`.
- Keep Prolog logic in Prolog; Python accesses it via `PrologEngine`.

## Python Style
- Follow existing import grouping: stdlib → third-party → local.
- Prefer explicit, descriptive names.
- Use dataclasses/small classes when state requires them.
- Avoid inline comments unless requested or they explain a genuinely non-obvious invariant.
- Keep functions focused and return early on error/unexpected state.
- Avoid global state unless required by config/runtime initialization.
- Keep async boundaries explicit; do not block async loops.
- Favor type hints on public methods/data structures.
- Guard `None` explicitly.
- Match adjacent string formatting; f-strings are preferred.

## Prolog Style
- Keep predicates in their owning module (`kb/*` facts, `modules/*` logic).
- Preserve predicate naming/arity conventions.
- Canonical intent mappings belong in `kb/intents.pl`.

## Intent Resolution & Prolog
- All normal Python Prolog access goes through `zara/prolog_engine.py`.
- Use `PrologEngine.resolve_intent()` / `query_once()`; no raw `pyswip.Prolog()` in application code.
- Prefer double-quoted Prolog strings for user text.
- Keep user-derived values behind the constrained Prolog term serializers.
- Keep intent mappings, missing-slot checks, and pending-question prompts aligned.
- Plain Prolog is part of Zara. Do not reintroduce a `prolog-rlm` runtime backend/dependency.

## Agent System & Tools
- The canonical conversational backend is LangGraph.
- Built-in tools live in `zara/agent/tools/`.
- Third-party tools should enter the existing LangChain `ToolRegistry` path.
- Avoid parallel custom agent/tool registries.
- MCP tools and plugin tools join the same registry; preserve collision checks and stable identities.
- `context_ids`/backend voice controls/tool approval hooks in the runtime interface are not proof those capabilities are implemented. Check the concrete backend before documenting or depending on them.

## System Prompt
- System prompt is configured under `[agent].system_prompt`.
- If the value is a filepath, it may be read from disk by the agent loader.
- If empty, Zara uses its default prompt.
- Keep tool guidance concise; use tools when needed rather than reflexively.

## Wake Loop
- Wake flow attempts deterministic Prolog routing before open-ended agent fallback where applicable.
- Maintain conversation history through the existing conversation/agent managers.
- Preserve latency trace identity across wake, STT, routing, model/tool, TTS and cancellation stages.
- Structured latency metrics must never contain transcript text, prompts, credentials or audio bytes.
- Keep microphone/VAD capture shared across STT backends instead of forking a new wake implementation per model.

## Memory
- Use `MemoryManager` for semantic/session memory operations.
- Keep persistent-memory failure explicit (`local_fallback` is not durable persistence).
- Keep Chroma optional with graceful process-local fallback.
- Destructive all-memory deletion must retain explicit confirmation semantics.

## Imports & Dependencies
- Do not add non-Nix dependencies unless requested.
- If a dependency is required, update `flake.nix` and the appropriate `pythonLibs`/package definition.
- Keep `setup.py` best-effort packaging metadata aligned where relevant; it is not the authoritative system dependency definition.

## Error Handling
- Prefer explicit errors over silent fallback when the requested behavior cannot be honored.
- For user-facing failures, return a short actionable message.
- Tool/provider failures should not crash the whole conversational loop.
- Do not serialize provider/tool errors into conversation history as fake assistant responses.

## Documentation Contract
Current documentation is mapped from `docs/index.org`.

When user-visible behavior changes:
- update `docs/features.org` so shipped/not-implemented status stays accurate;
- update the owning subsystem page (`docs/voice.org`, `docs/cli.org`, `wiki/mcp.org`, `wiki/plugins.org`, `docs/pets.md`, etc.);
- update `docs/configuration.org` and the relevant literate config under `docs/config/` when settings change;
- update `README.org` only when the project-level summary/fast path changes;
- add/update cross-links rather than duplicating long explanations into multiple pages.

Status language matters:
- **Shipped** means present on `master` and expected to be covered by the regression gate.
- **Not implemented** means an interface/flag/design exists without a working current path.
- Draft PR/research/roadmap material must not be written in current docs as shipped behavior.
- `docs/desktop-copilot-design.md` is design/research history; `docs/features.org` and `docs/architecture.org` own present-tense desktop claims.
- Roadmap work lives in issue #1. Zara's roadmap explicitly excludes resurrecting Prolog-RLM runtime integration.

The literate config files are explanatory examples, not alternate runtime sources of defaults:
- `docs/config/main.org` explains `zara/config.py` defaults/validation.
- `docs/config/prolog.org` explains Prolog configuration owned by `kb/config.pl` / the config loader.
- `docs/config/mcp.org` explains MCP configuration owned by `zara/mcp/config.py`.

## Desktop / Runtime Boundary
- Qt widgets/controllers issue application commands and consume provider-neutral runtime events.
- UI code must not reach directly into `AgentManager`, concrete Prolog objects, microphone internals, or remote tool sessions.
- Plugins receive the bounded `PluginRuntime` facade, not concrete `RuntimeHost` internals.
- Keep GUI work on Qt's thread and runtime work behind `QtRuntimeBridge` / `RuntimeHost`.

## MCP
- Use the official MCP client path in `zara/mcp/`; do not build a second MCP/tool registry.
- Supported configured transports are stdio and Streamable HTTP.
- Treat stdio servers as local executables running with Zara's user privileges.
- Redact configured environment/header secrets.
- Never automatically replay a failed effectful MCP tool call after reconnect.
- Managed MCP config must remain owner-only.

## Plugins
- Importing a plugin must not start resources.
- Service lifecycle is owned by `RuntimeHost` (`start(runtime)` / `stop()`).
- Use bounded event subscriptions and managed workers.
- Do not give plugins Qt/backend/Prolog/microphone/Pet internals or a parallel event bus.
- Isolate plugin failure from the rest of Zara and avoid leaking private plugin config into diagnostics.

## Files to Respect
- `docs/index.org`: documentation map.
- `docs/features.org`: current feature/status ledger.
- `docs/architecture.org`: current runtime ownership/boundaries.
- `docs/configuration.org` + `docs/config/`: config docs.
- issue #1: roadmap/future work.
- `flake.nix`: supported dependency/package/check definition.

## Cursor / Copilot Rules
- No `.cursor/rules/`, `.cursorrules`, or `.github/copilot-instructions.md` are the repository authority; this file is.

## Do Not Do
- Do not add new linters/formatters without request.
- Do not change API providers/models without approval.
- Do not move Prolog logic into Python for convenience.
- Do not add non-Nix dependencies without approval.
- Do not create a second agent/tool/runtime stack for a UI, plugin, or protocol.
- Do not document a draft PR or interface placeholder as a shipped feature.

## Logging Conventions
- Prefer module-level loggers with `logging.getLogger(__name__)`.
- Use concise subsystem context.
- Log useful boundary/state metadata, not secrets or giant payloads.
- Preserve voice timing/privacy constraints.
- Use `info` for normal flow, `warning` for recoverable issues, `error` for failures.

## Debugging Tips
- Debug voice in pipeline order: input → VAD → STT → routing → model/tool → TTS.
- Check exact Prolog goals/results for deterministic routing issues.
- If agent tools loop, inspect tool-call IDs and validated history groups.
- For MCP, start with `zara mcp status` / `inspect` and isolate the failing server.
- For desktop issues, verify runtime events/receipts before reaching into widget code.

## Tests Location
- Python tests live in `t/`.
- Shell/integration/smoke checks live in `scripts/test-*.sh`.
- The final authority is still the full test command under **Tests**.
