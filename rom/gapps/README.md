# Google services build input

The ROM lane requires Google services to be present in the final image, but proprietary
Google binaries are not stored in this public branch.

Provide a private source tree and point `GAPPS_VENDOR_DIR` at it before bootstrap:

```text
gapps-vendor/
├── product.mk
├── Android.bp / Android.mk / product fragments
└── proprietary files owned by the local build environment
```

`rom/scripts/bootstrap.sh` mirrors that directory to `vendor/zara-gapps/`.
`vendor/extra/product.mk` then inherits `vendor/zara-gapps/product.mk`.
`rom/scripts/build.sh` refuses to build if the product file is absent.

This makes Google Play services/GApps a required build-time input without checking
redistribution-restricted APKs or credentials into git.

Keep any downloader credentials in environment variables, an OS keyring/wallet, or CI
secret storage. Do not add tokens, keys, service-account files, signing material, or
checked-in `.env` files to this branch.
