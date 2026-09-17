# Android third-party notices

Zara's phone-to-watch sideload transport uses open-source Android ADB components.

## WatchPush

Portions of the ADB identity, transfer, and wireless-debugging workflow were adapted from WatchPush:

- Project: `GPTmadeit/WatchPush`
- License: GNU General Public License v3.0
- Upstream files consulted include `AdbKeys.kt`, `WatchAdb.kt`, `AdbTransfer.kt`, `AdbDiscovery.kt`, and `MainViewModel.kt`.

Zara itself is distributed under GPL-3.0, so these adaptations remain under the repository's GPL-3.0 terms.

## libadb-android

The Android phone client uses `com.github.MuntashirAkon:libadb-android` to implement ADB pairing, TLS connections, and streams from inside the app. See the upstream project for its license and notices.

## Conscrypt and Bouncy Castle

The watch installer also depends on Conscrypt and Bouncy Castle for TLS and certificate support. Their respective upstream licenses continue to apply.
