# Android UI acceptance harness

`device_acceptance.py` drives the installed production app through UIAutomator. It uses the same Compose navigation state and user-visible actions as a person; it does not invoke a test-only navigation shortcut.

Compose exposes stable `resource-id` values to UIAutomator for primary menu entries (`zara.menu.<menu>`), route tabs (`zara.route.<route>`), the menu control (`zara.navigation.open-menu`), selected menu (`zara.menu.active.<menu>`), destination (`zara.screen.<route>`), and drawer state (`zara.navigation.drawer-state`). The destination tag identifies the visible route; the selected-menu tag and drawer's `content-desc` identify the other navigation state explicitly. Feature scenarios should select controls by these IDs where labels can repeat, then assert all three state values. Visible text remains useful for checking user-facing content and accessibility; it is not a substitute for asserting the destination.

Each `capture(name)` writes `name.png`, `name.hierarchy.xml`, and a bounded app-process log (`name.logcat.txt`) when the app process is available. `manifest.json` ties each evidence set to its scenario ID and records source SHA, installed package/version identity, device profile, and pass/fail outcome. Capture is attempted on the failure path too; hierarchy or log collection errors are recorded instead of hiding the original test failure.

Run against an explicitly selected emulator:

```sh
python android/integration/device_acceptance.py \
  --serial emulator-5554 \
  --source-sha "$(git rev-parse HEAD)" \
  --output android/app/build/reports/device
```

For hosted runs, download the `android-ui-evidence` artifact from that exact GitHub Actions run. Open `manifest.json` to find the scenario and its files; PNGs show the rendered result, XML exposes the active semantic tree, and the corresponding log files retain app-side diagnostics. A successful build or screenshot by itself does not establish feature behavior: scenarios must assert the production UI state and outcome they claim.
