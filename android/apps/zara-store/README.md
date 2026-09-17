# Zara Store

`ai.zara.store` is Zara's first-party Android app and plugin store. It consumes standard F-Droid repositories and adds a narrow Zara metadata layer for source provenance, plugin protocol compatibility, capabilities, and runtime readiness.

Tracked by #1015 under the Android app-suite epic #1000.

## Foundation slice

This initial module deliberately implements the trust/state contract before APK installation authority:

- pins the reusable Apache-2.0 F-Droid `download`, `index`, and `database` libraries at `0.2.0`;
- accepts ordinary F-Droid package identities without Zara metadata;
- only applies Zara metadata when package name, version code, APK SHA-256, and signer SHA-256 match;
- embeds the exact Zara source commit in `BuildConfig.SOURCE_SHA`;
- separates `downloaded`, `verified`, and observed `installed` state;
- separates Android-plugin `enabled`, `trusted`, `permissionReady`, and `runtimeReady` state;
- only requests network access. This slice does **not** request `REQUEST_INSTALL_PACKAGES` and cannot install APKs yet.

The next installer slice must use Android's supported PackageInstaller flow, require the appropriate per-source user authorization on stock Android, and confirm package/version/signer through PackageManager before reporting `installed`.

## Repository model

The public Zara repository remains a normal F-Droid repository so existing F-Droid clients can consume it:

```text
/fdroid/repo/
```

Zara-specific semantics are a companion catalog bound to the same exact package bytes and signer identity:

```text
/zara/catalog-v1.json
/zara/catalog-v1.json.sig
/zara/source-manifest.json
```

The companion catalog can add Zara semantics. It cannot convert a different or unverified APK into a trusted Zara package.

## F-Droid upstream boundary

F-Droid's client libraries are intentionally reused instead of reimplementing repository downloading, index parsing/verification, and persistence. Their public APIs are currently marked alpha upstream, so the Zara adapter around them must remain narrow and version-pinned.

Useful upstream references:

- <https://github.com/f-droid/fdroidclient/blob/master/libs/README.md>
- <https://f-droid.org/docs/Setup_an_F-Droid_App_Repo/>
- <https://f-droid.org/en/docs/Whitelabel_Builds/>
- <https://f-droid.org/docs/Signing_Process/>
- <https://f-droid.org/docs/Reproducible_Builds/>

The F-Droid 2.0 client line removed support for the historical F-Droid Privileged Extension. Zara's future-ROM design therefore does not depend on that obsolete client path. A future Zara ROM should expose a minimal reviewed Zara system installer adapter while keeping this same public F-Droid repository and Zara catalog format.

## Product modes

### Stock Android / Samsung

Zara Store is an ordinary app. Repository browsing and verification need no elevated authority. Installation/update/remove must use supported Android package-management flows and report interaction requirements honestly.

### Future Zara ROM

The same store UI, catalog, package identities, and plugin trust model remain. Only the installer backend changes to a separately reviewed system component. The UI process should not become a broad privileged app.

## Build

From the Android project root, using the repository's pinned toolchain:

```sh
./gradlew :zara-store:test :zara-store:assembleDebug
```

The build embeds the immutable source SHA supplied by `ZARA_SOURCE_SHA` or resolved from Git.
