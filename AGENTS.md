# AGENTS.md — Zarathushtra

This file guides agentic coding assistants working in this repo.

## Scope
- Applies to the entire repository.
- Follow these instructions before adding new tooling or changing architecture.

## Human voice fixture corpus
- `voice-fixtures.org` is the single Emacs/Org recording workbench. Do not add a second recorder script or standalone `.el` recorder.
- Human voice fixture definitions live as JSON under `t/fixtures/voice/cases/`; real recordings live under `t/fixtures/voice/recordings/`.
- Every declared JSON case is a hard test obligation. `t/test_voice_fixture_manifest.py` must fail until its matching 16 kHz mono signed-16 PCM WAV exists and validates. Never weaken this with `pending`, optional fixtures, skips, xfails, generated silence, or placeholder audio.
- When adding or materially changing dictation, wake/STT, voice commands, transcript normalization, barge-in, multi-turn voice dialogue, or voice E2E behavior, review the corpus and add/update representative JSON cases when human speech can catch regressions text-only tests cannot.
- Creating the JSON before the recording is intentional RED. The human recording is part of acceptance.
- Recordings are accumulated on the single persistent `fixtures/voice-recordings` branch. Use the first block in `voice-fixtures.org`; it creates/opens `~/git/worktrees/zara-voice-recordings` and switches the Org buffer to that worktree instead of changing the normal/master checkout.
- Record only synthetic/test speech safe for the public repository. Never record credentials, private conversations, secrets, or incidental personal data.

## Environment
- Prefer `nix` for all builds, tests, and dev shells.
- Use the repo’s `flake.nix` for dependency setup.
- Python tooling is supplied via `pythonLibs` in the flake.

## Build / Run
- Enter dev shell:
  - `nix develop`
- Build all packages:
  - `nix build`
- Run default app (CLI; prints help with no args):
  - `nix run`
- Run wake listener app:
  - `nix run .#zara-wake`
- Run console mode:
  - `nix run .#zara-console`
- Run dictation mode:
  - `nix run .#zara-dictate`
- Run agent conversation mode (no `nix run` app defined; use the entrypoint):
  - `nix develop -c zara-agent` after `pip install -e .`, or run `nix build` and call the `zara` wrapper with `--agent`

## Tests
- Run full test suite (single command, ZARA-021 gate):
  - `nix develop -c bash scripts/test-all.sh`
- Run the same suite as flake checks (pytest, scripts, syntax, Prolog load, wrappers):
  - `nix flake check`
- Run a single test file:
  - `nix develop -c pytest t/test_agent_history.py`
- Run a single test by node id:
  - `nix develop -c pytest t/test_agent_history.py::test_multiple_results_survive_in_call_order`

## TDD & Coverage Contract

All behavior-changing work is test-driven by default. Tests are not paperwork to backfill after the implementation is already emotionally attached to itself.

For each behavior or regression:

1. Write or update a deterministic test that expresses the required behavior before changing production logic.
2. Run the focused test and prove it fails for the expected reason.
3. Make the smallest coherent production change that satisfies the test.
4. Run the focused test until green.
5. Refactor only while the test remains green.
6. Repeat for the next behavior, then run the broader suite.

Exceptions are limited to changes needed to create or repair the test harness itself. When that happens, keep the harness change minimal and document why ordinary red-green TDD was impossible.

Maximize meaningful coverage across all changed and adjacent behavior. Coverage means exercising distinct behavior and failure paths, not merely causing a line counter to blink.

Prioritize tests for:

- happy paths and realistic end-to-end flows;
- invalid input, empty values, limits, and boundary conditions;
- startup, degraded, restart, shutdown, and cleanup transitions;
- cancellation, stale work, retries, races, and timeout behavior;
- queue/resource limits and failure recovery;
- security, authorization, identity, and isolation boundaries where relevant;
- persistence and migration behavior;
- packaging, console scripts, Nix wrappers, and installed-resource behavior;
- regressions for every bug fixed.

Do not game coverage. Never weaken assertions, add meaningless execution-only tests, exclude relevant code, or preserve untested reachable branches solely to improve a percentage. If changed reachable behavior remains untested, keep adding useful tests until the practical coverage ceiling is reached or document the specific reason a path cannot be deterministically exercised.

## CI/CD Test Gate
- Always run the focused red/green TDD cycle before the full test suite for behavior changes.
- Always run the full test suite after any code, configuration, test, or documentation change.
- Inspect changed-code coverage gaps and add meaningful tests before considering implementation complete.
- Do not consider work complete unless every local test passes.
- Pull requests and pushes to `master` must pass the GitHub Actions `test` job.
- After pushing or opening a pull request, verify the CI test job passes for the exact candidate SHA; do not report the change as complete while CI is pending or failing.

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

## Long-Horizon Tasks
- Long-horizon execution is owned by `RuntimeHost` via `zara/tasks/runner.py`; do not fork the conversational loop or add a second approval channel.
- Task state changes must go through `zara/tasks/store.py` transitions; never graft machine task state onto `TodoStore`.
- Task tools are gated by `[tasks].enabled` (default false) and share the existing tool approval policy. See `wiki/long-horizon-tasks.org`.

## External service-plugin configuration

- External service plugins own their private configuration under
  `$XDG_CONFIG_HOME/zarathushtra/plugins/<plugin-name>/` (falling back to
  `~/.config/zarathushtra/plugins/<plugin-name>/`). Do not move plugin-owned
  settings into Zara's root configuration or expose them through diagnostics.
- Zara service-plugin discovery still uses public Python entry files in the
  configured `[modules].search_paths` (the default is
  `~/.zarathushtra/plugins/`). An installed entry may load implementation and
  dependency files from its private XDG plugin directory.
- `zara-discord` is an external `ServicePlugin`: its Discord token is a
  bootstrap secret supplied through `ZARA_DISCORD_TOKEN` or its mode-0600
  `token` file; its guild access and channel policy are Discord-owned state
  managed by slash commands. Do not add that token or policy to Zara source,
  fixtures, logs, or commits.

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
- Preserve the latency trace ID across wake, STT, routing, LLM, TTS, and cancellation stages.
- Structured latency metrics must never contain transcript text, prompts, credentials, or audio bytes.

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
- The roadmap lives in issue #1 (ZARA-000) and `wiki/`.
- Keep the implementation aligned with that roadmap and the wiki pages.

## Cursor / Copilot Rules
- No `.cursor/rules/`, `.cursorrules`, or `.github/copilot-instructions.md` found.

## RAGE Work Protocol

When the user requests RAGE, use the repository's GitHub Issues as the work queue. Do not manufacture an implementation target from prose when an issue or epic already defines the work.

### Issue consumer

1. Read the relevant epic/roadmap issue and its open children.
2. Select the first open issue whose declared dependencies are satisfied, honoring explicit priority/order. Regression blockers outrank later feature work when the roadmap says so.
3. Record the consumed issue number and exact starting commit in `rage/<work-log>.org` before research, design, tests, or implementation commits.
4. One RAGE iteration works one consumed issue. An epic is a queue/container unless the epic explicitly says it is itself an atomic implementation issue.
5. When an issue passes its gate and is merged/closed, consume the next eligible issue on the next iteration. Never silently skip an eligible blocking issue.

### RAGE iteration

Every iteration is ordered and evidence-driven:

1. **Research** — investigate deeply enough to challenge the issue's inherited design. Read current code and tests, relevant project history/issues, authoritative upstream documentation, known failure modes, and serious alternatives. Research must be capable of changing the plan.
2. **Architecture / Design** — derive the implementation from the research. Record decisions, rejected alternatives, invariants, threat/failure analysis, migration/compatibility constraints, acceptance criteria, and the exact verification gate. Map every acceptance criterion to tests before implementation.
3. **TDD / Generate / Implement** — write the next failing test first, prove the expected red result, implement the smallest coherent change, rerun to green, then refactor. Repeat behavior by behavior. Maximize meaningful branch/path/failure coverage for the consumed issue.
4. **Evaluate** — run focused tests plus the repository's complete gate at the exact candidate head. Inspect changed-code coverage gaps and add useful tests for reachable behavior. GitHub Actions for an older SHA is stale evidence and cannot authorize merge.
5. **Outcome** — if the gate passes, merge and record the merge SHA. If failures falsify the design or reveal that the attempt is structurally wrong, preserve the failure evidence in the Org log, discard the failed implementation attempt, and start a new RAGE iteration from research/design. Do not endlessly patch a disproven architecture until CI becomes green by exhaustion.

### Work log

- Keep an append-only Org-mode log under `rage/`.
- The log must state the exact immutable RAGE start commit and consumed GitHub issue.
- Record iteration boundaries, research sources/findings, design decisions, planned tests, significant red/green evidence, implementation commits, coverage gaps addressed, gate commands/results, CI run/SHA, failures, discarded attempts, PR, and final merge SHA.
- A retry gets a new iteration section. Failed work stays visible as evidence.

### Repository-local skills

- Zara-local reusable agent procedures belong under `skills/`.
- Read the relevant local skill before executing that procedure.
- The RAGE skill supplements this file; it does not override repository gates or GitHub issue ordering.

## Do Not Do
- Do not add new linters or formatters.
- Do not change API providers or models without approval.
- Do not move Prolog logic into Python.
- Do not add non‑Nix dependencies without approval.
- Do not add inline comments unless asked.
- Do not implement behavior first and backfill tests later when ordinary TDD is possible.
- Do not game coverage metrics.

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
- Use small edits and avoid large refactors without request, except where an explicit RAGE design justifies the larger change.
- When modifying logic, add logging only where needed.

## Tests Location
- Python tests live in `t/`.
- Shell integration and smoke tests live in `scripts/test-*.sh`.
- Use the full test suite command under **Tests** for final verification.
- New behavior belongs behind tests first; regressions require a reproducing test before the fix.

## Structure Notes
- `zara/` contains runtime Python modules.
- `kb/` contains Prolog knowledge base facts.
- `modules/` contains Prolog logic.
- `t/` contains tests.
- `rage/` contains append-only RAGE work evidence.
- `skills/` contains Zara-local agent skills.

## Wiki Documentation
- Keep `wiki/` documentation up to date with code changes.
- Update the relevant wiki pages when behaviors, tools, or flows change.

## Additional Notes
- Keep code consistent with existing style.
- Favor minimal, targeted changes outside an approved RAGE design.
- Ask before adding new files beyond what is requested, except RAGE logs/skills/design/test artifacts required by this protocol.
- Follow repo conventions for notifications, logging, and config defaults.
