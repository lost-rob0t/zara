# Zara Changelog

This is the canonical user-facing changelog for Zara. Entries describe behavior that has landed on `master`; workers update this file in the same change that lands user-visible behavior or release-facing fixes.

## Unreleased

- Fixed a ZARA/1 wire-ordering race where runtime events for fast symbolically-resolved turns could overtake the `turn.accepted` reply, making every Android/Desktop remote turn fail with `protocol.unexpected_message` (expected turn.accepted). Turn events are now held per-route and flushed after the accepted reply, in order.

- Native Emacs integration now exposes the versioned `ZARA-EMACS/1` semantic bridge with opaque buffer/window identities, bounded buffer reads, live command/key introspection, revision-safe edit preview/apply/cancel, ordinary Emacs undo, typed window control, and a closed trusted command-adapter registry. Zara chat remains on the canonical Zara runtime rather than creating an Emacs-local agent loop.

### Fixed

- Desktop pure-symbolic project switches now fence stale clarification, discourse, expert, and verified-fact context before the next turn, matching project-scoped Android semantics without enabling provider or model fallback.
- Native Emacs symbolic replay now validates provider/model hard-zero state before replacing the live transcript, so a rejected replay leaves the visible presentation and cached symbolic status untouched.
- Android remote sessions no longer break after a successful voice turn: the client now decodes the server's `voice.speech.started`/`voice.speech.ended` markers and the legal `turn.cancelled`/`runtime.error`/`runtime.stopped` lifecycle messages it previously rejected as protocol errors, and interleaved text frames no longer kill the voice stream.
- A failed remote frame can no longer leave Android in a fake-connected state: voice pump death and session-desyncing protocol/transport failures now collapse the connection with a typed reason and drive the existing bounded reconnect.
- Android restores a persisted remote session on launch with `session.restore` telemetry instead of silently staying disconnected after process recreation.
- Android chat failures now render one specific, actionable error card (failed subsystem, operation, stable typed code, connection state, recovery, incident id, Retry/Reconnect/Diagnostics actions) instead of umbrella `protocol_error`/`operation_failed`, and no longer duplicate the failure into a second banner.
- The Android chat footer now separates mode, authentication, and transport/protocol truth (`REMOTE • AUTHENTICATED • CONNECTED/RECONNECTING/DISCONNECTED`) instead of showing `REMOTE • AUTHENTICATED • SYMBOLIC` regardless of connection health.

### Added

- Android pure-symbolic conversation mode now preserves deterministic multi-turn clarification and follow-up context across process recreation through Zara's canonical symbolic dialogue and conversation projection, with providers disabled, no credentials required, exact zero provider/model calls, and stale or cancelled completions fenced by the existing generation/CAS boundary.
- Android emits typed, correlation-aware telemetry events (`remote.*`, `protocol.*`, `voice.*`, `session.restore.*`) with monotonic sequences, generation fencing, and metadata-only protocol message records, so no connected-to-disconnected transition is unexplained.
- `ZARA-LOCAL-DIAGNOSTICS/2` incident bundle (text + canonical JSON) with a retained primary-failure block, remote/protocol context, voice pipeline stage states with explicit `not_applicable` semantics, and a correlated ordered timeline; pasting it into a bug report or AI chat identifies the failed subsystem, operation, typed code, last-good step, and correlation ids.
- CI now reproduces the reported remote voice → protocol failure → recovery class end to end: a deterministic failure-injecting ZARA/1 fixture drives the real Android client (JVM matrix: malformed frame, version mismatch, out-of-order, close mid-stream, stale generation after reconnect) and the installed APK on the emulator (text + real voice turn, injected failures, typed UI error, Diagnostics v2, reconnect, second turn, recreation fencing), with evidence retained on success and failure.

### Added

- Native Emacs chat can now keep a stable canonical Zara conversation identity, consume strict `turn.accepted`/`assistant.complete` events, inspect/switch conversation state while idle, and cancel the runtime-minted turn through canonical `CancelTurn`; late or malformed events fail closed without provider/model fallback or an Emacs-owned transcript store.
- Desktop can now select the provider-free `pure_symbolic` conversation execution policy before any daemon/model runtime is constructed; symbolic replies use Zara's canonical Prolog dialogue renderer, report exact zero provider/model usage, and fail closed on unsupported context instead of falling back to a model.
- ZARA-SYNC/1 now defines bounded version vectors, stale-delta fencing, content-addressed block manifests, tombstones, and opaque encrypted revisions, with tiny intermediate blocks rejected while allowing a short final tail block.
- Zara server pairing now supports short-lived QR bootstrap and a single-use 16-letter human pairing-code primitive while keeping long-term client/server trust on authenticated CURVE/ZAP; Android pairing material stays client-owned and desktop paired profiles remain owner-private.
- Authenticated ZARA/1 peers can now attach a bounded `ZaraNode` descriptor to the secure hello handshake; Zara binds that metadata to the existing CURVE/ZAP enrollment and exact principal/session, rejects identity/generation mismatches, and never treats advertised device features as authorization grants.
- A running local Zara server now exposes owner-only live control that can lazily create its durable CURVE identity and idempotently activate an authenticated remote ZARA/1 listener without a daemon restart; pairing clients can consume the returned endpoint and public-key metadata.
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
- Android remote connections now retain the client CURVE secret for the lifetime of the authenticated socket and deliver the first ZARA/1 hello after the asynchronous CURVE handshake completes, so an enrolled device connects to a secure Zara server over real networks instead of silently failing.
- Native Emacs `zara-chat` now enables the canonical conversation-control minor mode by default once `zara-conversation` is loaded, so ordinary send keys use stable conversation identity, strict native events, and canonical cancellation instead of the legacy one-shot request path.
- Explicit secure-TCP Zara servers keep their owner-only live security admin socket under the configured security directory, so existing live enroll/revoke commands continue mutating the running registry while the new local-first bootstrap control socket remains runtime-scoped.
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
