# Local Android SDK libraries

Do not commit vendor AARs here.

## Samsung Health Data SDK

Zara's Samsung Health adapter targets **Samsung Health Data SDK v1.1.0**. Download the SDK from Samsung Developer and copy exactly one file matching:

```text
samsung-health-data-api-*.aar
```

into this directory.

The Android build detects the AAR at configuration time. Without it, `BuildConfig.HAS_SAMSUNG_HEALTH_SDK` is `false`, the vendor source set is not compiled, and Zara uses the typed unavailable gateway. This keeps ordinary CI and source builds independent of a redistributed Samsung binary.

With the AAR present, `src/samsungHealthSdk/java` is compiled and Zara enables the real `SamsungHealthSdkGateway`.

Samsung's current platform requirements are Android 10/API 29+, Java 17+, and Samsung Health 6.30.2+. The SDK does not support emulators. Public distribution also requires Samsung to register the app package and release-signing SHA-256; Samsung Health developer mode is for development/testing only.

The plugin is read-only in this slice. Data access still requires the user's per-data-type consent inside Samsung Health.
