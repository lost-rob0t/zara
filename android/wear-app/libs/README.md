# Samsung Health Sensor SDK

Place exactly one official `samsung-health-sensor-api-*.aar` here to compile the
optional Wear OS gateway. The AAR is vendor-supplied, is not redistributed by
Zara, and remains ignored by Git.

Without it the watch APK builds normally and reports `SDK_MISSING`; that generic
APK is not a Zara Health watch build. A release build must inject the approved
Samsung Health Sensor SDK v1.4.1 artifact through protected operator input and
verify its hash, version, package name, and signing certificate.
