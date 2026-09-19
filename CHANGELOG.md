# Zara Changelog

This is the canonical user-facing changelog for Zara. Entries describe behavior that has landed on `master`; workers update this file in the same change that lands user-visible behavior or release-facing fixes.

## Unreleased

### Added

- Android now includes a code editor for user-selected workspace roots with revision-fenced voice edits, so late speech results cannot overwrite newer editor changes.
- Android Org Daily now consumes an explicitly configured workspace-relative daily path template, date pattern, and timezone through the shared Org-home authority; arbitrary user roots stay intact, invalid or missing Daily configuration fails closed, and simply viewing Daily still does not invent or create a file.
- Desktop Org now has named native Qt launch surfaces for the flagship workbench plus focused Editor, Todo, Sync, and Notebook modes. The shared shell opens only an explicitly selected Org root, recursively discovers ordinary `.org` files without inventing a default layout, and uses revision-fenced source saves so edits made concurrently by Emacs, Git, or another Zara process fail stale instead of being overwritten.
- Android's flagship Org shell now exposes Todo, Org-roam, and Daily surfaces over the shared canonical Org repository/projection stack; it supports the shared workspace or an arbitrary SAF-selected directory, keeps ordinary Org files authoritative, and refuses to guess a Daily path, filename pattern, or timezone when canonical Daily configuration is unavailable.
- Android's shared Org parser/storage/index foundation now honors multiple file-declared `#+TODO` / `#+SEQ_TODO` sequences and caller-supplied fallback TODO states, so custom parallel Org workflows parse, cycle within their declared sequence, and index the canonical corpus without inheriting an operator-specific state machine.
- Android Git config templates now recognize the canonical dotfiles projection at `.config/zarathushtra/android` when a manifest omits explicit source paths, while keeping the existing manifest, validation, and data-only Prolog security boundary.
- Android launcher actions now persist launchable apps, typed launch intents, and bounded success/failure observations in Zara's private Prolog workspace; secret memory is fingerprinted instead of copied into model recall.
- Org automation definitions can now compile from canonical Org heading projections into deterministic, inspectable trigger/condition/action symbol references without granting execution authority or assuming a fixed Org root.
- Org automation runs now have inert correlated run envelopes with deterministic replay keys and an explicit stale-definition fence, while execution and authorization remain owned by Zara's canonical typed runtime.
- Optional service plugins can now report a canonical "started but unavailable" state when required configuration or credentials are missing; unavailable plugins expose only a bounded reason code in diagnostics, keep no registered tools or capabilities, and no longer fail startup.
- Android chat now keeps durable multi-conversation history with New chat, pin/unpin, rename, move-to-project actions, full turn restoration, and per-chat lifecycle status indicators.
- Human todos can now use ordinary Org files as canonical storage, with configurable roots or gpt-todos checkouts, stable Org IDs, schedule/deadline/repeater metadata, and round-trip preservation of unrelated Org properties and prose.

### Fixed

- Android Org Git sync now treats its configured branch as authoritative and fails closed instead of pulling or pushing a different checked-out branch; incomplete JGit push statuses are no longer reported as successful syncs.
- Android Org Daily now anchors its default continuous stream at the configured logical today, so pre-existing future daily files do not appear ahead of today's real file when scrolling backward through canonical dailies.
- Desktop Org source saves now preserve each existing file's permission mode across atomic replacement, so editing from Zara does not silently turn a shared Emacs/Git Org file into a `0600` temp-file mode.
- Org todo discovery now recursively scans configured directory roots, so nested ordinary `.org` files remain visible and editable without flattening a user's workspace or Git layout.
- Android code-editor voice operations now fence cursor and selection changes as editor revisions, so a late transcript cannot apply to a different selection than the one active when listening started.
- Android code-editor voice dictation now preserves spoken indentation, removes speech-only spacing around quoted text and Prolog commas, normalizes grouping punctuation without regex crashes, and builds against the current Compose layout API.
- Android code-editor voice actions now handle backward text selections correctly, so replace, wrap, and explain operations use the selected range instead of rejecting or reversing it.
- Android Org TODO state changes now preserve the source file's existing line endings and terminal-newline shape instead of rewriting unrelated formatting in canonical Org files.
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
