# Zara / StarIntel Android ROM lane

This directory is the source of truth for the long-lived `fork/android-rom` branch.

This branch is intentionally divergent from `master`: application work continues on
`master`, while this branch owns the custom Android distribution, LineageOS integration,
system-app prebuilts, device patches, and ROM build policy. Do not open a merge-back PR for
the branch as a whole.

## Base

- Upstream manifest: `LineageOS/android`
- Upstream branch: `lineage-23.2`
- Zara source/release: `lost-rob0t/zara`
- StarIntel Android source/release: `lost-rob0t/starintel-wearos`
- Google services: private build input at `vendor/zara-gapps/product.mk`

Google/GMS binaries are not committed to this public branch. A build that is intended to
ship Google services must provide the private vendor tree through `GAPPS_VENDOR_DIR`.

## What gets baked in

Phone images include these preinstalled system apps:

- Zara (`ai.zara.app`)
- StarIntel Companion (`actor.starintel.wear`)
- StarIntel Quasar (`actor.starintel.quasar`)

The APKs stay signed by their normal update-channel keys so an installed ROM can accept
newer APK updates without a platform-signature mismatch.

## Layout

- `manifest/zara.xml` — local repo manifest that mounts this branch into a Lineage tree.
- `aosp/vendor/extra/product.mk` — product hook inherited by LineageOS.
- `aosp/prebuilts/Android.bp` — Soong definitions for bundled APKs.
- `patches/` — ordered patch series for framework, device, kernel, and vendor changes.
- `scripts/bootstrap.sh` — initialize/sync a Lineage workspace and install private inputs.
- `scripts/sync-prebuilts.sh` — fetch and checksum Zara/StarIntel APKs.
- `scripts/apply-patches.sh` — apply the ROM patch series idempotently.
- `scripts/build.sh` — validate inputs, select a Lineage target, and build an OTA zip.
- `private/` — ignored local inputs; never commit signing keys, GMS blobs, or secrets.

## First build

```bash
git switch fork/android-rom

export ROM_ROOT="$HOME/android/zara-rom"
export GAPPS_VENDOR_DIR="$HOME/.local/share/zara-rom/gapps-vendor"

rom/scripts/bootstrap.sh
rom/scripts/build.sh <device-codename>
```

Use `REPO_JOBS`, `BUILD_JOBS`, `STARINTEL_TAG`, and `GITHUB_TOKEN` as environment
variables when needed. Secrets stay in the environment/keyring/CI secret store; they do
not belong in this branch.

## Patch policy

Each patch must be a normal `git format-patch` file under `patches/<project-path>/`.
Add it to `patches/series` as:

```text
frameworks/base patches/frameworks-base/0001-example.patch
kernel/google/gs-common patches/kernel-google-gs-common/0001-example.patch
```

`apply-patches.sh` applies patches with `git am --3way`, skips patches whose commit is
already present, and aborts on drift. Rebase the patch itself when upstream changes rather
than adding shell mutations to the build.

## Device policy

Keep device-specific work isolated by source project/codename. The ROM layer is device
agnostic; device trees, vendor blobs, kernels, and monitor-mode or hardware patches belong
in the patch queue or dedicated repos referenced by a local manifest extension.
