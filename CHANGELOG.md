# Zara Changelog

This is the canonical user-facing changelog for Zara. Entries describe behavior that has landed on `master`; workers update this file in the same change that lands user-visible behavior or release-facing fixes.

## Unreleased

- Keep this section for work that is merged but not yet assigned to a release.

## 0.2.2-alpha

### Added

- Android now uses a simplified three-menu application shell with tabbed Settings and adaptive navigation.
- Android Projects provide persistent project contexts and project-scoped chat conversations.
- Release CI now requires exact-source Android and Desktop screenshot evidence plus deeper adversarial regression gates.
- Zara's strict version context separates the currently built version from the active release target and validates immutable release provenance.

### Fixed

- Android audio-focus release failures keep the focus lease retryable instead of silently losing ownership.
- Android Assistant qualification and lifecycle handling were replayed onto the current runtime/navigation foundation.
- Android emulator release evidence is more deterministic through explicit host-runtime and SDK provisioning checks.

### Changed

- The active release line is `0.2.2-alpha`; immutable publication remains gated on exact-head CI, signing, provenance, and version promotion.
- Mutable `android-latest` remains a rolling test channel and is not treated as an immutable semantic release.

## 0.1.2-alpha

- Previous immutable alpha baseline.
