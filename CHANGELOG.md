# Zara Changelog

This is the canonical user-facing changelog for Zara. Entries describe behavior that has landed on `master`; workers update this file in the same change that lands user-visible behavior or release-facing fixes.

## Unreleased

### Added

- Assistant runtimes are backend-neutral: Desktop and Android discover installed runtimes, expose Prolog-RLM as an optional runtime when present, and keep model/provider reasoning inside Prolog-RLM when selected.

### Fixed

- Android Runtime settings now project `embedded-local` health and model version from the canonical local-AI lifecycle instead of showing a synthetic `builtin / ready` row; unknown local-model state fails closed as unavailable rather than inventing readiness.
- Desktop Prolog-RLM discovery and runtime configuration now require a literal loopback address (`127.0.0.1` or `::1`) instead of trusting the `localhost` hostname, keeping context and turn traffic on an explicit local transport boundary.
- Wear Voice no longer requests direct Internet access; the focused watch voice shell stays network-free and leaves runtime transport to the shared Wear/phone authority path.
- Wear Voice now uses Zara's canonical version name and Android versionCode instead of shipping stale module-local package metadata.
- Plugin capability compositions now re-check a tool's live approval policy immediately before invocation, preventing approval-policy changes after registration from bypassing approval.
- Android Prolog-RLM turns keep cancellation responsive while generation is running, discard stale replies after runtime switches, and fall back to the embedded runtime after rediscovery detects a dead sidecar.
- Android Prolog-RLM chat now distinguishes user cancellation, stale-runtime fencing, ordinary turn failure, and actual sidecar unavailability; only runtime unavailability triggers sidecar rediscovery, and chat/diagnostics do not echo raw runtime error detail.
- Desktop Runtime settings now render discovery-driven health, locality, capabilities, profiles, selection state, and a bounded diagnostic reason for installed-but-unselectable runtimes without adding them to the selectable runtime list.
- Runtime settings now ignore saved provider/model selections that become incompatible or unavailable, and clear dependent stale picks instead of silently keeping invalid choices.
- Desktop Prolog-RLM turns now preserve a runtime-returned cancellation as a typed cancelled result and discard late completions after local cancellation as well as replies from an older runtime generation after the runtime is stopped or restarted, preventing stale output from publishing into the active turn.
- Prolog-RLM runtime transport failures now surface as bounded typed Zara runtime errors without exposing raw sidecar or provider error details.
- Desktop Prolog-RLM startup now reports an incompatible discovered `ZARA-RUNTIME` protocol as a typed protocol error instead of collapsing it into a generic unavailable-runtime failure.
- Desktop Prolog-RLM turns now forward bounded host-owned context handles when the discovered runtime advertises support, so Prolog-RLM receives the intended Zara context without a second Python inference path.

## 0.2.2-alpha

### Added

- Android now uses a simplified three-menu application shell with tabbed Settings and adaptive navigation.
- Android Projects provide persistent project contexts and project-scoped chat conversations.
- Android Local mode now records bounded, redacted runtime diagnostics that can be copied, shared, or cleared from Diagnostics.
- Release CI now requires exact-source Android and Desktop screenshot evidence plus deeper adversarial regression gates.
- Zara's strict version context separates the currently built version from the active release target and validates immutable release provenance.

### Fixed

- Android Local chat now falls back to the verified on-device model when symbolic/Prolog resolution fails, while explicit Prolog commands still report their own failures.
- Android launcher entries use distinct task identities so opening Automation or Watch Setup cannot reuse another Zara launcher surface's task state.
- Android audio-focus release failures keep the focus lease retryable instead of silently losing ownership.
- Android Assistant qualification and lifecycle handling were replayed onto the current runtime/navigation foundation.
- Android emulator release evidence is more deterministic through explicit host-runtime and SDK provisioning checks.
- Release-note sections render consistently on Desktop and Android without extra blank lines between a subsection heading and its first item.
- Immutable alpha publication now keys off canonical version readiness plus actual GitHub release absence instead of push-event changed-file metadata, so protected-branch promotion merges cannot silently skip APK publication.
- The mutable `android-latest` channel now starts a fresh signed Android/Wear build on every `master` push instead of waiting for the entire repository CI workflow, preserving an exact-SHA APK payload for each successful master build and rolling the direct-download release forward promptly.

### Changed

- The active release line is `0.2.2-alpha`; immutable publication remains gated on exact-head CI, signing, provenance, and version promotion.
- A validated `version.properties` promotion on `master` can publish the matching immutable alpha release, and an existing tag is accepted only when its source SHA and release manifest match the promoted build exactly.
- Mutable `android-latest` remains a rolling test channel and is not treated as an immutable semantic release.

## 0.1.2-alpha

- Previous immutable alpha baseline.
