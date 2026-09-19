# Zara Changelog

This is the canonical user-facing changelog for Zara. Entries describe behavior that has landed on `master`; workers update this file in the same change that lands user-visible behavior or release-facing fixes.

## Unreleased

### Added

- Org automation definitions can now compile from canonical Org heading projections into deterministic, inspectable trigger/condition/action symbol references without granting execution authority or assuming a fixed Org root.
- Optional service plugins can now report a canonical "started but unavailable" state when required configuration or credentials are missing; unavailable plugins expose only a bounded reason code in diagnostics, keep no registered tools or capabilities, and no longer fail startup.
- Android chat now keeps durable multi-conversation history with New chat, pin/unpin, rename, move-to-project actions, full turn restoration, and per-chat lifecycle status indicators.

### Fixed

- Android voice transcripts no longer accept late updates after a final transcript, preventing stale stream events from overwriting a completed transcript.
- Android's embedded Trealla bridge now honors the pinned Trealla `pl_query` success contract, restoring local Prolog queries; Local mode also runs a live query readiness probe before reporting READY and emits richer bounded query diagnostics on failure.
- Wear Voice no longer requests direct Internet access; the focused watch voice shell stays network-free and leaves runtime transport to the shared Wear/phone authority path.
- Wear Voice now uses Zara's canonical version name and Android versionCode instead of shipping stale module-local package metadata.
- Plugin capability compositions now re-check a tool's live approval policy immediately before invocation, preventing approval-policy changes after registration from bypassing approval.
- Cancelling a plugin turn now cooperatively signals active composed tool invocations before stale-result fencing, so opted-in plugin work can stop promptly instead of continuing after cancellation.

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