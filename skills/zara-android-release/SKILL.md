---
name: zara-android-release
description: Version and release Zara Android with SemVer, focused branch PRs, exact-head CI, immutable version tags, a rolling latest channel, and verified APK provenance. Use for Android version bumps, release preparation, APK publication, release tags, or release validation in this repository.
---

# Zara Android release

Use the GitHub connector for remote branches, pull requests, Actions state, artifacts, tags, and releases. Use local Git only to prepare and test changes. Never bypass branch protection or substitute an older green run for the exact candidate SHA.

## 0. Resolve strict version context

Run `python scripts/version-context.py --format json` before any version, merge, tag, publication, or updater decision. `version.properties` is the only mutable Zara product-version source. `zara.version` / `android.versionCode` describe the source currently being built; `release.target` / `release.targetAndroidVersionCode` describe the active release line. Never infer the active target from `setup.py`, Gradle literals, a stale prompt, an old tag, or `android-latest`.

Python packaging and both Android apps consume the current values from that file. Versioned release CI validates the same context and may publish a missing one-to-one `v<version>` release from the exact promoted `master` SHA or validate an explicit matching tag event. Ordinary master work never reuses an existing version release. The separate `android-latest` workflow remains the mutable rolling master channel.

## 1. Establish the release candidate

1. Fetch `master`, confirm the worktree is clean, record the exact starting SHA, and resolve the strict version context.
2. Inspect the latest immutable `v*` tag/release and the `android-latest` manifest source SHA.
3. Select the next version using SemVer:
   - breaking public contract: increment major;
   - backward-compatible feature: increment minor;
   - backward-compatible fix: increment patch;
   - prerelease: append or advance a valid identifier such as `-alpha`, `-alpha.2`, or `-rc.1`.
4. Update `release.target` and `release.targetAndroidVersionCode` in `version.properties`; a newer target requires a strictly larger Android code.
5. Promote `zara.version` and `android.versionCode` to the target only on the integrated candidate authorized for release.
6. Never reuse a published version/tag or Android version code.

Keep version promotion on the normal protected merge/review path. The release workflow may mint the missing immutable tag/release only after the promoted exact SHA is fully green; an explicit matching tag is also accepted, but never move or reuse an existing version tag.

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

## 3. Publish immutable versions and rolling latest

After the integrated candidate is approved on `master`, resolve the strict version context again and require `release_ready=true`. The canonical `CHANGELOG.md` must contain one non-empty `## <version>` section. Each version maps one-to-one to `v<version>` and one GitHub release. Never move, overwrite, or reuse a version tag/release; refuse tag/version mismatches.

Before publication, versioned release CI waits for the exact source SHA's full CI to pass, including Android UI acceptance. It then installs and exercises the exact signed release APK it just built on an emulator before any GitHub release is created. The trusted tag or `master` release workflow must then produce:

- `zara-android-<version>.apk`;
- `zara-android-<version>.apk.sha256`;
- `zara-android-<version>.manifest.txt` containing source SHA, version name, version code, filename, and SHA-256;
- a verified update-compatible signing certificate;
- a release-notes asset and GitHub release body generated from that exact version's canonical changelog section.

Pull-request CI must publish `zara-android-debug-<sha>` with a phone APK, checksum, and exact-SHA manifest. Do not relabel a PR artifact as a release asset.

A separate `master`-push workflow owns the intentionally mutable `android-latest` convenience channel. Every `master` push starts an exact-SHA signed Android/Wear build immediately instead of waiting for unrelated repository CI jobs. Each successful build preserves its own exact-SHA Actions payload; the serialized publisher moves only the `android-latest` tag forward and publishes direct-download `zara-latest.apk` and `zara-wear-latest.apk` assets plus checksums and `zara-latest.manifest.txt`. Never treat `android-latest` as immutable release evidence; its manifest source SHA is the authority for the exact build it currently serves.

## 4. Verify the distributed APK

Download the GitHub release asset or exact-SHA Actions artifact through the GitHub connector. Verification and hardware acceptance must use that downloaded GitHub artifact, not a local rebuild.

1. Download the published GitHub release assets again; do not verify only the local pre-upload files.
2. Verify the SHA-256 against the adjacent checksum and provenance manifest.
3. Byte-compare the downloaded APK/manifest/release notes with the gated publication inputs.
4. Verify source SHA, `versionName`, `versionCode`, expected filename, and the update-compatible APK signer.
5. Verify the GitHub release body matches the canonical changelog notes exactly.
6. Confirm the exact source SHA passed the full repository CI and Android/Wear release gate.
7. Keep real-device install, microphone, Bluetooth route, Assistant-role, side-button, and revocation checks `PENDING` until performed on hardware.

Report the branch, PR, exact head SHA, workflow run, artifact or release URL, SHA-256, signer result, and remaining hardware gates.
