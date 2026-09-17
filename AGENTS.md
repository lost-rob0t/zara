# AGENTS.md — Zara / Zarathushtra

This file is the repository-wide working model for coding agents. Read it before changing architecture, runtime behavior, plugins, Prolog, Android, or release machinery. More specific `AGENTS.md` files below a directory add local constraints and win when they are stricter.

## Scope and source of truth

- This repository owns Zara Core, the desktop/runtime implementation, the canonical semantic/runtime contracts, the Android and Wear clients under `android/`, and the compatibility surfaces that external plugins consume.
- Public/provider plugins published through the separate `lost-rob0t/zara-plugins` registry live in that repository unless the implementation is intrinsically Zara Core or Android-specific.
- The roadmap and current GitHub issues/PRs are authoritative for work ownership. Read the relevant issue/epic and search open PRs before creating a second implementation.
- Do not infer architecture from an old issue when current code, tests, a newer issue, or an active owning PR supersedes it.
- Prefer extending an existing contract over creating a parallel runtime, registry, scheduler, approval channel, memory store, provider abstraction, or Android authority plane.
- `master` is protected. Work on a branch and use a pull request. Exact-head CI is part of the acceptance evidence.

## Repository ownership model

Use this split when deciding where work belongs.

### Zara Core (`lost-rob0t/zara`)

Core owns:

- `RuntimeHost` and process/runtime lifecycle;
- conversation, wake, agent, memory, long-horizon task, approval, and policy integration;
- canonical Prolog/semantic contracts and intent resolution;
- built-in LangChain-facing tools and typed runtime/device action contracts;
- plugin loading/hosting contracts and compatibility tests;
- Android/Wear Kotlin, Compose, platform permissions, intents, services, device adapters, Data Layer, and UI;
- cross-platform interfaces required by both desktop and Android;
- release/versioning/packaging for Zara itself.

### `lost-rob0t/zara-plugins`

That repository owns independently installable/public integration implementations such as provider adapters and service plugins when they do not require a Core runtime change. Examples include calendar/cloud/home/comms providers implemented behind existing Zara plugin contracts.

If a plugin reveals a missing Core primitive, make the smallest Core contract change here and consume it from the plugin. Do not copy Core internals into the plugin repository.

### Android plugin work

“Android plugin” does not mean “start another Zara runtime in an APK.” Android integrations must use the Zara-owned Android host/capability contracts. Native platform mechanics stay in this repo. A separately packaged capability APK is valid only when the current Android plugin contract/issue explicitly calls for one; it still delegates policy/authority to Zara rather than inventing its own agent loop.

## Runtime architecture

Think in layers rather than one giant “tool registry”:

1. **Semantic input** — text/voice/UI is normalized and resolved by the existing Prolog/semantic path where applicable.
2. **Policy/authority** — existing Zara policy, approval, principal, and Android authority decisions determine whether an effect may run.
3. **Typed effect/capability** — execution crosses a closed typed boundary (`DeviceActionArguments`, plugin tools/commands, task transition, etc.). Callers do not supply arbitrary implementation strings such as Android component names or provider URLs.
4. **Adapter/provider** — a bounded adapter performs the real platform/provider operation.
5. **Observed result** — report what was actually observed. A provider acknowledgement, launched Intent, or queued action is not proof of downstream completion.

Do not let an LLM or Prolog term become direct OS/network authority. Prolog may decide *what* should happen; reviewed code owns *how* the effect is executed and bounded.

## Agent system and tools

- The conversational agent consumes LangChain `BaseTool`/`StructuredTool` surfaces.
- Built-in agent tools live under `zara/agent/tools/` (including `builtin_tools.py`), but that is not the entire Zara execution architecture.
- Runtime/service plugins can expose tools through the Plugin API while participating in Zara lifecycle, policy, approval, events, and typed commands.
- Do not create a second generic tool registry. Adapt new functionality to the existing LangChain/plugin/runtime contracts.
- Third-party libraries may expose LangChain tools directly when that is genuinely sufficient; provider credentials and principal ownership still stay outside tool arguments.

## Plugin working model

Zara supports plugin behavior through existing Core contracts. Before adding or changing a plugin, inspect `zara/plugins`, current plugin tests, and the corresponding plugin repo implementation.

- `ServicePlugin` is for lifecycle-managed integrations/services. It is hosted by Zara; it does not own a replacement runtime.
- Plugin metadata/API versions are compatibility contracts. Keep metadata, registry/package declarations, tests, and installed behavior aligned.
- External service plugins own private config under `$XDG_CONFIG_HOME/zarathushtra/plugins/<plugin-name>/` (fallback `~/.config/zarathushtra/plugins/<plugin-name>/`).
- Public entry/discovery files use configured `[modules].search_paths` (default `~/.zarathushtra/plugins/`). An entry may load implementation/dependencies from its private XDG directory.
- Never put provider/API secrets in tool arguments, diagnostic output, fixtures, Git, Nix store paths, or screenshots.
- For principal-bearing providers, bind credentials/account identity outside caller-controlled inputs and regression-test isolation between principals.
- Mutating provider plugins should independently read back state where the provider supports it. A `2xx`/accepted response alone is not `verified=true`.
- Plugin background work uses Zara lifecycle/worker ownership. Do not spawn unmanaged immortal threads or a competing scheduler.
- When a public plugin requires a Core change, add compatibility/consumer tests here and update the plugin against the reviewed contract.

`zara-discord` is an external `ServicePlugin`: its token is supplied through `ZARA_DISCORD_TOKEN` or its mode-0600 token file. Discord-owned guild/channel policy stays in Discord state and must not be copied into Zara config, logs, fixtures, or source.

## Android / Wear working model

Read `android/AGENTS.md` plus relevant Android design/wiki files before Android work.

- Android is a Zara client/runtime surface, not an independent architecture fork.
- Reuse canonical Core semantics, typed actions, task/todo data, memory/project context, and policy decisions wherever the platform allows.
- Native Android-only mechanics (Intents, `CalendarContract`, Accessibility, Shizuku/root, notifications, IME, package visibility, Compose, Wear APIs) belong in `android/` behind typed Zara interfaces.
- Prefer public/documented Android contracts. Private activities, receivers, undocumented extras, or OEM internals require an explicit issue/design and must not be smuggled in as ordinary “open app” behavior.
- Remote callers may request typed actions; they must not inject raw `Intent` actions, package/component names, shell commands, URIs outside an allowlisted contract, or accessibility operations.
- A user-mediated handoff to another app must be reported as a handoff, not as completion of work inside that app.
- Platform permissions are requested at the narrowest legitimate boundary and failures map to typed unavailable/permission/failure results.
- Visual behavior needs real implementation-backed UI plus the Android screenshot/visual gate where the local instructions require it. Do not ship dead buttons or mock settings for unavailable behavior.
- Wear consumes canonical projections/state; do not invent a watch-side scheduler or independent source of truth.

## Prolog and semantic resolution

- Prolog lives in `kb/` and `modules/`; Android also carries reviewed Prolog sources where the Android runtime contract requires them.
- Keep reasoning/policy/semantic logic in Prolog when it belongs there. Python/Kotlin executes bounded effects.
- Desktop Prolog access goes through `zara/prolog_engine.py`; do not instantiate raw `pyswip.Prolog()` in feature code.
- Use `PrologEngine.resolve_intent()` / `query_once()` and preserve the existing engine lifecycle.
- Prefer double-quoted Prolog strings for user text and validate/escape at the boundary.
- There is no “update `kb/intents.pl` only” rule. When adding or changing a portable intent, keep every affected semantic layer in sync: knowledge/facts, resolver/missing-slot logic, pending-question prompts, codecs/typed effects, Android projection when applicable, and tests.
- Prolog configuration is executable Prolog where that subsystem intentionally exposes it, but physical effects still cross reviewed authority/capability boundaries.
- Do not move Prolog reasoning into ad-hoc Python/Kotlin conditionals merely to avoid updating the semantic contract.

## Long-horizon tasks and human todos

- Long-horizon machine execution is owned by `RuntimeHost` via `zara/tasks/runner.py`.
- State transitions go through `zara/tasks/store.py`; do not graft machine task state onto the human todo store.
- Task tools are gated by `[tasks].enabled` and share Zara approval policy. See `wiki/long-horizon-tasks.org`.
- Human todos/Org/gpt-todos are a separate user-facing planning source. Android/Wear project canonical state; they do not create another scheduler.

## Wake / conversation flow

- The wake path attempts deterministic/Prolog resolution first where the semantic contract applies.
- Escalate to LLM conversation when symbolic resolution cannot answer or explicitly asks for LLM help.
- Preserve conversation history via `ConversationManager` and memory through the existing memory subsystem.
- Preserve latency trace identity through wake → STT → routing → LLM → TTS/cancellation.
- Structured telemetry must not contain transcript text, prompts, credentials, audio bytes, or other secret/private payloads unless the explicit telemetry contract says otherwise.

## Memory

- Use the existing `MemoryManager`/memory contracts for session storage and summaries.
- Preserve graceful fallback behavior when optional vector storage is unavailable.
- Plugin/project/symbolic memory extensions must integrate with the canonical memory contract rather than create isolated stores with incompatible identity/lifecycle semantics.

## Environment

- Prefer Nix for builds, tests, and dev shells.
- Use the repository `flake.nix` for dependency setup.
- Python dependencies available to the project belong in `pythonLibs`/Nix packaging rather than an undocumented ambient environment.

## Build / run

- Dev shell: `nix develop`
- Build packages: `nix build`
- Default CLI: `nix run`
- Wake listener: `nix run .#zara-wake`
- Console: `nix run .#zara-console`
- Dictation: `nix run .#zara-dictate`
- Agent entrypoint: `nix develop -c zara-agent` after editable install when required, or use the built Zara wrapper with `--agent`.

Follow Android-local build/release instructions for `android/`; do not replace the repository release process with an ad-hoc Gradle artifact.

## Tests

- Full repository gate: `nix develop -c bash scripts/test-all.sh`
- Flake checks: `nix flake check`
- Focused Python file: `nix develop -c pytest t/<test_file>.py`
- Focused Python node: `nix develop -c pytest t/<test_file>.py::<test_name>`
- Android/Wear work also runs the focused Gradle/JVM/instrumentation/contract gates required by `android/AGENTS.md`, the issue, and CI.

## TDD and coverage contract

Behavior-changing work is test-driven by default.

1. Write/update a deterministic test for the next behavior.
2. Prove the focused test fails for the expected reason.
3. Make the smallest coherent production change.
4. Run the focused test to green.
5. Refactor while green.
6. Repeat, then run the broader/full gate.

Exceptions are limited to repairing/creating the test harness itself; document why normal red-green was impossible.

Cover meaningful behavior, especially:

- realistic happy-path flows;
- malformed/empty/boundary input;
- startup/degraded/restart/shutdown/cleanup;
- cancellation, stale work, retry, races, timeout;
- queue/resource bounds and recovery;
- authorization, principal, secret, and isolation boundaries;
- persistence/migration/upgrade behavior;
- packaging/install/resource behavior;
- provider acknowledgement versus observed postcondition;
- every fixed regression.

Never weaken assertions, add execution-only tests, exclude relevant reachable code, or leave known branches untested merely to improve a number.

## CI/CD gate

- Run focused red/green before the full suite for behavior changes.
- Run the full applicable test gate after code/config/test/docs changes.
- Inspect changed-code coverage gaps.
- A PR is not complete while the exact candidate SHA has pending/failing required CI.
- Older green runs are stale evidence.
- Do not merge around a red gate. Fix the cause or document a genuine external/hardware acceptance requirement without pretending CI proved it.

## Python style

- Existing import order: stdlib → third-party → local.
- Prefer explicit descriptive names and small focused functions/classes.
- Use type hints on public boundaries.
- Keep async boundaries explicit; do not block event loops.
- Return early on invalid/error states.
- Avoid global mutable state unless required by established config initialization.
- Match adjacent formatting; no repository-wide formatter migration unless requested.
- Avoid explanatory inline comments unless they add necessary invariant/protocol context; do not narrate obvious code.

## Prolog style

- Put facts/knowledge in the established KB location and reusable reasoning in the appropriate module.
- Preserve predicate naming/arity conventions and executable-config conventions of the subsystem.
- Validate any effect term against a closed codec/allowlist before execution.
- Tests must cover malformed terms and fan-out/authority bypass attempts for effect-producing predicates.

## Provider/integration security

For calendar, home, comms, OAuth, or similar provider work:

- credentials are host/plugin-owned, never LLM/caller-supplied fields;
- caller IDs are data, not URLs/paths/headers;
- confine HTTP to configured provider origins and reject credential-bearing cross-origin redirects;
- bound request/response sizes, timeouts, pagination, retries, and backoff;
- classify ambiguous mutation failures rather than blindly retrying non-idempotent writes;
- use provider version/ETag/idempotency primitives when available;
- verify writes by independent observation when possible;
- test two-principal isolation and secret-safe errors/logs;
- do not substitute browser/shell scraping for an existing maintained provider API without an explicit design decision.

## Error handling and logging

- Prefer explicit typed/actionable errors over silent fallbacks.
- Tool/provider failures must not crash the agent loop.
- Use `logging.getLogger(__name__)` and concise subsystem context.
- Never log API keys, bearer/refresh tokens, passwords, raw auth headers, or large private payloads.
- Boundary logging may include safe identifiers/shape metadata when useful; avoid raw prompts/transcripts by default.

## Dependencies

- Do not add a dependency merely to avoid a small standards-compliant implementation when stdlib/existing libraries are sufficient.
- If a new dependency is required, update Nix/flake packaging and installed-resource tests together.
- Do not rely on undeclared local packages.

## Documentation

- Keep relevant `wiki/` pages synchronized with behavior/contracts.
- For cross-repo plugin changes, document which side owns the interface and link the matching issue/PR.
- Use repository-local `skills/` for reusable Zara-specific agent procedures; read the relevant skill before executing it.
- Android releases use `skills/zara-android-release/SKILL.md`.

## RAGE work protocol

When the user requests RAGE, GitHub Issues are the work queue. Do not manufacture a new target when an issue/epic already owns the work.

### Issue consumer

1. Read the relevant epic/roadmap and open children.
2. Check open PRs/workers for ownership collisions.
3. Choose the first eligible issue according to declared dependencies/priority.
4. Record the consumed issue and exact immutable start commit in `rage/<work-log>.org` before implementation work.
5. One RAGE iteration consumes one atomic issue; an epic is normally a queue/container.

### Iteration

1. **Research** — current code/tests/history plus authoritative upstream docs and alternatives.
2. **Architecture/design** — invariants, rejected alternatives, threat/failure analysis, compatibility/migration constraints, acceptance criteria mapped to tests.
3. **TDD/implement** — repeated focused red → minimal implementation → green cycles.
4. **Evaluate** — focused tests, full applicable gate, changed-code coverage, exact-head CI.
5. **Outcome** — merge/record only when gate passes; if evidence falsifies the design, preserve it and start a new iteration instead of patching around a broken architecture.

Keep the Org log append-only with start SHA, issue, sources, decisions, test evidence, commits, gate results, CI SHA/run, failures/discarded attempts, PR, and merge SHA.

## Structure notes

- `zara/` — Python runtime/core modules.
- `kb/` — Prolog knowledge/facts.
- `modules/` — Prolog logic/resolution.
- `android/` — Android/Wear clients and native adapters; see its local `AGENTS.md`.
- `t/` — Python tests.
- `scripts/` — integration/smoke/gate scripts.
- `wiki/` — architecture/behavior docs.
- `rage/` — append-only RAGE evidence.
- `skills/` — Zara-local agent procedures.

## Do not do

- Do not create a second runtime, scheduler, plugin registry, approval channel, memory system, Prolog engine, or Android authority plane for convenience.
- Do not commit directly to protected `master`.
- Do not implement an issue already owned by an active PR/worker unless explicitly taking it over.
- Do not let callers/LLMs inject raw Android intents/components or arbitrary provider origins through a typed capability.
- Do not claim a handoff/HTTP acknowledgement equals completed downstream behavior.
- Do not move Prolog reasoning into Python/Kotlin without an architectural reason.
- Do not add non-Nix/undeclared dependencies without updating packaging.
- Do not add a new linter/formatter unless requested.
- Do not backfill tests after implementation when ordinary TDD is possible.
- Do not game coverage or weaken security/approval tests to make CI green.
- Do not expose credentials in source, fixtures, logs, diagnostics, generated docs, or screenshots.

Keep changes small and targeted unless an approved issue/RAGE design justifies a broader refactor.