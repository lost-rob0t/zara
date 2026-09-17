---
name: zara-android-release
description: Version and release Zara Android with SemVer, focused branch PRs, exact-head CI, immutable tags, and verified APK provenance. Use for Android version bumps, release preparation, APK publication, release tags, or release validation in this repository.
---

# Zara Android release

Use the GitHub connector for remote branches, pull requests, Actions state, artifacts, tags, and releases. Use local Git only to prepare and test changes. Never bypass branch protection or substitute an older green run for the exact candidate SHA.

## 1. Establish the release candidate

1. Fetch `master`, confirm the worktree is clean, and record the exact starting SHA.
2. Inspect the latest `v*` tag and `android/app/build.gradle.kts`.
3. Select the next version using SemVer:
   - breaking public contract: increment major;
   - backward-compatible feature: increment minor;
   - backward-compatible fix: increment patch;
   - prerelease: append or advance a valid identifier such as `-alpha`, `-alpha.2`, or `-rc.1`.
4. Increment Android `versionCode` monotonically and set `versionName` to the SemVer value without the leading `v`.
5. Never reuse a published `versionName` or `versionCode`.

Create a focused `release/android-v<version>` branch and pull request. Keep product implementation in its own branch PR; stack it only when an unmerged release-infrastructure change is a real dependency.

## 2. Gate the exact head

Run focused tests first, then the repository gates documented in `AGENTS.md`, including:

```sh
nix develop -c bash scripts/test-all.sh
nix flake check
nix build
nix develop ./android -c bash scripts/test-android-release.sh
```

The release gate must bind evidence to the exact candidate SHA. After every push, use the GitHub connector to verify that all required jobs belong to the current PR head, pass, and have no unresolved review threads. A stale green SHA is a failure, not evidence.

Do not merge unless the PR is mergeable and the exact candidate SHA is green. Opening a PR does not authorize merging it.

## 3. Publish immutably

After the release PR is approved and merged, verify the merge SHA on `master`. Create `v<version>` at that exact merge SHA. Never move or overwrite a release tag. Refuse a tag/version mismatch and refuse to replace an existing release.

The trusted tag or `master` release workflow must produce:

- `zara-android-<version>.apk`;
- `zara-android-<version>.apk.sha256`;
- `zara-android-<version>.manifest.txt` containing source SHA, version name, version code, filename, and SHA-256;
- a verified update-compatible signing certificate.

Pull-request CI must publish `zara-android-debug-<sha>` with a phone APK, checksum, and exact-SHA manifest. Do not relabel a PR artifact as a release asset.

## 4. Verify the distributed APK

Download the GitHub release asset or exact-SHA Actions artifact through the GitHub connector. Verification and hardware acceptance must use that downloaded GitHub artifact, not a local rebuild.

1. Verify the SHA-256 against the adjacent checksum and provenance manifest.
2. Verify embedded source SHA, `versionName`, `versionCode`, and expected filename.
3. Verify the APK signer certificate with `scripts/check-android-apk-signer.sh` for release assets.
4. Confirm the artifact passed the Android/Wear gate and secret inspection at the recorded SHA.
5. Keep real-device install, microphone, Bluetooth route, Assistant-role, side-button, and revocation checks `PENDING` until performed on hardware.

Report the branch, PR, exact head SHA, workflow run, artifact or release URL, SHA-256, signer result, and remaining hardware gates.
