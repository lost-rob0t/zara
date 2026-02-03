# AGENTS.md — Zarathushtra

This file guides agentic coding assistants working in this repo.

## Scope
- Applies to the entire repository.
- Follow these instructions before adding new tooling or changing architecture.

## Environment
- Prefer `nix` for all builds, tests, and dev shells.
- Use the repo’s `flake.nix` for dependency setup.
- Python tooling is supplied via `pythonLibs` in the flake.

## Build / Run
- Enter dev shell:
  - `nix develop`
- Build all packages:
  - `nix build`
- Run default app (CLI):
  - `nix run`
- Run wake listener app:
  - `nix run .#zara-wake`
- Run console mode:
  - `nix run .#zara-console`
- Run dictation mode:
  - `nix run .#zara-dictate`

## Tests
- Run full test suite:
  - `nix develop -c pytest`
- Run a single test file:
  - `nix develop -c pytest t/test_agent.py`
- Run a single test by node id:
  - `nix develop -c pytest t/test_agent.py::TestAgent::test_basic_flow`

## Lint / Formatting
- No enforced linter in the repo; do not add one unless asked.
- Follow existing formatting and imports in adjacent files.
- Keep line lengths reasonable (match surrounding style; no hard limit enforced).
- Prefer small, focused diffs over broad formatting changes.

## Language Mix
- Python is the primary runtime.
- Prolog lives in `kb/` and `modules/`.
- Keep Prolog logic in Prolog; Python calls via `PrologEngine`.

## Python Style
- Follow existing import grouping (stdlib → third‑party → local).
- Prefer explicit, descriptive names (avoid one‑letter variables).
- Use dataclasses or small classes when state is required.
- Avoid inline comments unless the user requests them.
- Keep functions focused and short; prefer helper methods.
- Return early for error handling and unexpected states.
- Avoid global state unless required by config initialization.
- Keep async boundaries explicit and avoid blocking calls in async loops.
- Favor type hints on public methods and data structures.
- Use `Optional[...]` and guard `None` values explicitly.
- Keep string formatting consistent with adjacent code (f-strings preferred).

## Prolog Style
- Keep predicates in the correct module (`kb/*` for facts, `modules/*` for logic).
- Preserve existing predicate naming and arity conventions.
- When adding intents, update `kb/intents.pl` (knowledge base) only.

## Intent Resolution & Prolog
- All Prolog access should go through `zara/prolog_engine.py`.
- Use `PrologEngine.resolve_intent()` and `query_once()`; no raw `pyswip.Prolog()`.
- Prefer double‑quoted Prolog strings for user input.
- Log Prolog intent queries when debugging.
- Keep intent mappings in sync: update `kb/intents.pl`, `modules/intent_resolver.pl` missing-slot checks, and any pending-question prompts together when adding intents.

## Agent System & Tools
- The agent uses LangChain tools (no custom tool registry APIs).
- Built-in tools live in `zara/agent/tools/builtin_tools.py`.
- Third‑party tools should be imported as LangChain tools directly.
- Plugin loader expects tools to be `langchain_core.tools.BaseTool` instances.
- Avoid custom tool registries; prefer LangChain `tool` or `StructuredTool`.

## System Prompt
- System prompt is configured in `config.toml` under `[agent].system_prompt`.
- If the value is a filepath, it should be read from disk.
- If empty, the agent uses the default Zarathustra prompt.
- Tool guidance should be concise: call tools only when needed.

## Wake Loop
- The wake flow should attempt Prolog resolution first.
- Only fall back to LLM conversation mode when Prolog fails or returns `ask`.
- Maintain conversation history via `ConversationManager`.
- Log voice input before sending to the agent for debugging.

## Memory
- Use `MemoryManager` for session storage and summaries.
- Summaries should be stored when the conversation ends or times out.
- ChromaDB is optional; keep a graceful in‑memory fallback.

## Imports & Dependencies
- Do not add non‑Nix dependencies unless requested.
- If a new dependency is required, update `flake.nix`.
- If you add a dependency, ensure `pythonLibs` includes it.

## Error Handling
- Prefer explicit error messages over silent failures.
- For user‑facing failures, return a short, actionable message.
- For tool failures, keep the LLM response from crashing.

## Files to Respect
- `spec_zara_inital_fixes.org` describes the system design.
- Keep the implementation aligned with that spec.

## Cursor / Copilot Rules
- No `.cursor/rules/`, `.cursorrules`, or `.github/copilot-instructions.md` found.

## Do Not Do
- Do not add new linters or formatters.
- Do not change API providers or models without approval.
- Do not move Prolog logic into Python.
- Do not add non‑Nix dependencies without approval.
- Do not add inline comments unless asked.

## Logging Conventions
- Prefer module-level loggers via `logging.getLogger(__name__)`.
- Use concise prefixes for subsystems (e.g., `[AgentManager]`, `[AgentNode]`).
- Log inputs at boundaries (wake → agent, Prolog queries) with repr for clarity.
- Avoid logging secrets (API keys, raw credentials) or large payloads.
- Use `info` for normal flow, `warning` for recoverable issues, `error` for failures.

## Debugging Tips
- Enable logging at the wake loop and agent boundary.
- Capture the exact `user_input` and message list sent to the LLM.
- Keep Prolog query logging for intent resolution.
- If the agent loops on tools, check tool call ids in `graph.py` logs.
- For Prolog issues, verify `main.pl` is loaded and predicates exist.

## Single‑File Changes
- Use small edits and avoid large refactors without request.
- When modifying logic, add logging only where needed.

## Tests Location
- Python tests live in `t/`.
- Use `pytest` to run them.

## Structure Notes
- `zara/` contains runtime Python modules.
- `kb/` contains Prolog knowledge base facts.
- `modules/` contains Prolog logic.
- `t/` contains tests.

## Wiki Documentation
- Keep `wiki/` documentation up to date with code changes.
- Update the relevant wiki pages when behaviors, tools, or flows change.

## Additional Notes
- Keep code consistent with existing style.
- Favor minimal, targeted changes.
- Ask before adding new files beyond what is requested.
- Follow repo conventions for notifications, logging, and config defaults.
