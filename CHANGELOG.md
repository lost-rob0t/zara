# Zara Changelog

This is the canonical user-facing changelog for Zara. Entries describe behavior that has landed on `master`; workers update this file in the same change that lands user-visible behavior or release-facing fixes.

## Unreleased

### Added

- Authenticated ZARA/1 peers can now attach a bounded `ZaraNode` descriptor to the secure hello handshake; Zara binds that metadata to the existing CURVE/ZAP enrollment and exact principal/session, rejects identity/generation mismatches, and never treats advertised device features as authorization grants.
- The daemon now logs a warning for every denied CURVE/ZAP client authentication, including the presented public key, so unenrolled or mistyped client keys are diagnosable server-side instead of failing silently.
- Added an authenticated browser bridge and packaged Chromium/Firefox extension for reading pages and performing approval-gated browser actions from Zara.
- Portable Zara package profiles can now enable and pin packages independently per app while reusing the same package ABI and each app's existing symbol registry.
- Service plugins can register owner-scoped programmable symbols with deterministic override precedence, introspection, and automatic restoration of previous definitions when a plugin unloads.
- Android now includes a code editor for user-selected workspace roots with revision-fenced voice edits, so late speech results cannot overwrite newer editor changes.
- Android now builds a separate Zara Termux Bridge APK that truthfully reports Termux installation and Run Command permission state without claiming the Linux worker runtime is ready before a real integration probe succeeds.
- Android Git config templates now recognize the canonical dotfiles projection at `.config/zarathushtra/android` when a manifest omits explicit source paths, while keeping the existing manifest, validation, and data-only Prolog security boundary.
- Android launcher actions now persist launchable apps, typed launch intents, and bounded success/failure observations in Zara's private Prolog workspace; secret memory is fingerprinted instead of copied into model recall.
- Org automation definitions can now compile from canonical Org heading projections into deterministic, inspectable trigger/condition/action symbol references without granting execution authority or assuming a fixed Org root.
- Org automation runs now have inert correlated run envelopes with deterministic replay keys and an explicit stale-definition fence, while execution and authorization remain owned by Zara's canonical typed runtime.
- Org automation run receipts can now be rebuilt deterministically from bounded correlated events with contiguous sequencing, explicit retry backoff, and exactly-one terminal projection semantics without storing raw provider/tool payloads.
- Optional service plugins can now report a canonical "started but unavailable" state when required configuration or credentials are missing; unavailable plugins expose only a bounded reason code in diagnostics, keep no registered tools or capabilities, and no longer fail startup.
- Android chat now keeps durable multi-conversation history with New chat, pin/unpin, rename, move-to-project actions, full turn restoration, and per-chat lifecycle status indicators.
- Human todos can now use ordinary Org files as canonical storage, with configurable roots or gpt-todos checkouts, stable Org IDs, schedule/deadline/repeater metadata, and round-trip preservation of unrelated Org properties and prose.

### Fixed

- Android Auto chat now prefers an authenticated desktop/server session when one is connected instead of silently answering locally first.
- Android emulator CI now proves the installed APK can complete both a real embedded Local Prolog turn and an authenticated CURVE/ZARA/1 text turn against the stock Zara server.

- Zara Code APKs now pass cryptographic/package validation, CI installs and launches the editor on an Android emulator, and rolling `android-latest` publication rechecks the stable signing certificate before publishing.
- Python runtime descriptors now reject schema-invalid boolean and enum-shaped scalar values at construction, keeping `ZARA-RUNTIME/1` host state aligned with the shared wire contract before discovery or selection.
- Runtime descriptor protocol text is now bounded consistently by the shared schema and host validators, preventing incompatible overlong protocol identifiers from passing wire validation.
- Android Local AI now rejects new model work as soon as its runtime begins shutting down and drops stale backend callbacks after close, preventing shutdown races from hanging requests or reviving stopped local state.
- Android strict Local Assistant voice now uses only the on-device recognizer and non-network TTS, and cancels and fences stale local recognition, model, and speech work when the Assistant session is hidden, cancelled, restarted, or shut down.
- Android Assistant voice now refuses Remote capture unless enrollment is ready, the connected transport belongs to the current runtime generation, and the authenticated session ID is nonblank; Auto falls back to Local when that remote authority is stale or incomplete.
- Programmable package symbol, kind, and owner identifiers now reject control characters, whitespace, and non-ASCII tokens before registry mutation, keeping the portable namespace deterministic across Python, Prolog, and native app adapters.
- Portable package-profile versions now reject path-shaped, control-character, and non-ASCII tokens before any registry mutation, keeping package pins deterministic across host platforms.
- Org todo discovery now recursively scans configured directory roots, so nested ordinary `.org` files remain visible and editable without flattening a user's workspace or Git layout.
- Android code-editor voice operations now fence cursor and selection changes as editor revisions, so a late transcript cannot apply to a different selection than the one active when listening started.
- Android code-editor voice dictation now preserves spoken indentation, removes speech-only spacing around quoted text and Prolog commas, normalizes grouping punctuation without regex crashes, and builds against the current Compose layout API.
- Android code-editor voice actions now handle backward text selections correctly, so replace, wrap, and explain operations use the selected range instead of rejecting or reversing it.
- Android release-evidence automation now tolerates the short Compose/UIAutomator semantics race where the release-notes title appears before its Continue button, while still failing if the button never becomes available.
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
