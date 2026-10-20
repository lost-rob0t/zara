"""Contracts for the canonical Android Settings > Plugins surface."""

from pathlib import Path
import xml.etree.ElementTree as ET

ROOT = Path(__file__).resolve().parents[1]
MAIN = ROOT / "android/app/src/main"
KOTLIN = MAIN / "java/ai/zara/app"
ANDROID = "{http://schemas.android.com/apk/res/android}"


def test_plugins_is_real_settings_surface_not_a_gated_primary_route():
    shell = (KOTLIN / "ui/ZaraApp.kt").read_text()
    navigation = (KOTLIN / "ui/AppNavigation.kt").read_text()
    assert 'Plugins(AppMenu.Settings, "Plugins")' in navigation
    assert "AppSurface.Plugins -> PluginSettingsSurface(padding)" in shell
    assert 'Plugins("Plugins", "⬡", "#655")' not in shell


def test_catalog_is_projection_only_and_does_not_scan_android_packages():
    projection = (KOTLIN / "plugins/AndroidPluginCatalogProjection.kt").read_text()
    surface = (KOTLIN / "ui/PluginSettingsSurface.kt").read_text()
    combined = projection + surface
    assert "ZARA-ANDROID-PLUGIN/1" in projection
    assert "UnavailableAndroidPluginCatalogProjectionSource" in projection
    assert "getInstalledPackages" not in combined
    assert "queryIntentServices" not in combined
    assert "setApplicationEnabledSetting" not in combined
    assert "Capability visibility does not grant execution authority" in surface


def test_ready_is_not_inferred_from_install_or_enablement():
    projection = (KOTLIN / "plugins/AndroidPluginCatalogProjection.kt").read_text()
    surface = (KOTLIN / "ui/PluginSettingsSurface.kt").read_text()
    installer = (KOTLIN / "plugins/PluginApkInstaller.kt").read_text()
    assert "health != AndroidPluginCatalogHealth.READY || (trusted && enabled)" in projection
    assert "Installed, trusted, enabled, and ready are separate states" in surface
    assert "Trust, enablement, and capabilities were not granted" in installer


def test_apk_install_callback_is_private_and_same_process():
    manifest = ET.parse(MAIN / "AndroidManifest.xml").getroot().find("application")
    receiver = next(
        item
        for item in manifest.findall("receiver")
        if item.get(ANDROID + "name") == ".plugins.PluginInstallReceiver"
    )
    activity = next(
        item
        for item in manifest.findall("activity")
        if item.get(ANDROID + "name") == ".MainActivity"
    )
    assert receiver.get(ANDROID + "exported") == "false"
    assert receiver.get(ANDROID + "process") == activity.get(ANDROID + "process")
    assert not receiver.findall("intent-filter")


def test_install_flow_is_explicit_and_does_not_gain_broad_package_visibility():
    surface = (KOTLIN / "ui/PluginSettingsSurface.kt").read_text()
    installer = (KOTLIN / "plugins/PluginApkInstaller.kt").read_text()
    manifest = ET.parse(MAIN / "AndroidManifest.xml").getroot()
    assert "ActivityResultContracts.OpenDocument()" in surface
    assert "Settings.ACTION_MANAGE_UNKNOWN_APP_SOURCES" in surface
    assert "setRequireUserAction(PackageInstaller.SessionParams.USER_ACTION_REQUIRED)" in installer
    assert "PluginApkSecurity.copyVerified(input, output, candidate.sha256)" in installer
    assert ".setData(Uri.parse(PluginApkSecurity.callbackIdentity(sessionId, nonce)))" in installer
    permissions = {
        item.get(ANDROID + "name") for item in manifest.findall("uses-permission")
    }
    assert "android.permission.REQUEST_INSTALL_PACKAGES" in permissions
    assert "android.permission.QUERY_ALL_PACKAGES" not in permissions
    assert "android.permission.MANAGE_EXTERNAL_STORAGE" not in permissions


def test_plugin_picker_owns_labeled_click_semantics_without_wrapper():
    surface = (KOTLIN / "ui/PluginSettingsSurface.kt").read_text()
    picker = surface.split("private fun PluginPickerAction", 1)[1].split(
        "@Composable\nprivate fun PluginDigestRow", 1
    )[0]
    visual_owner, labeled_owner = picker.split('Text(\n            "Choose APK",', 1)
    assert ".clickable(" not in visual_owner
    assert ".clickable(" in labeled_owner
    assert "role = Role.Button" in labeled_owner
    assert "onClick = onActivate" in labeled_owner
    assert "PrimaryAction(" not in picker
    assert ".semantics(" not in picker
    assert "tokens.primary" in visual_owner
    assert "tokens.accent" not in visual_owner


def test_standalone_apk_policy_rejects_unsafe_archive_metadata_before_review():
    security = (KOTLIN / "plugins/PluginApkSecurity.kt").read_text()
    installer = (KOTLIN / "plugins/PluginApkInstaller.kt").read_text()
    assert 'packagePattern = Regex("[A-Za-z][A-Za-z0-9_]*(\\\\.[A-Za-z][A-Za-z0-9_]*)+")' in security
    assert "versionCode >= 0" in security
    assert "versionName.none(Char::isISOControl)" in security
    assert "minSdk in 1..sdkInt" in security
    assert "!hasSplits" in security
    assert "info.applicationInfo?.minSdkVersion ?: 1" in installer
    assert "!info.splitNames.isNullOrEmpty()" in installer


def test_device_evidence_matrix_captures_hashed_plugin_text_twins():
    acceptance = (ROOT / "android/integration/device_acceptance.py").read_text()
    assert '"Plugins",' in acceptance
    assert 'required_actions=("Choose APK",) if tab == "Plugins" else ()' in acceptance
    assert '"text_twin_file": twin_path.name' in acceptance
    assert '"text_twin_sha256": hashlib.sha256(twin_data).hexdigest()' in acceptance
    assert 'json.dumps(twin, sort_keys=True' in acceptance
    assert 'device.reveal("Choose APK")' in acceptance
    assert 'device.tap("Choose APK")' in acceptance
    assert 'device.await_contains("Enter the publisher\'s 64-character SHA-256.")' in acceptance
    assert (
        'device.capture(\n'
        '        "settings-plugins-narrow-large-font",\n'
        '        required_actions=("Choose APK",),\n'
        '    )'
        in acceptance
    )
    assert '"plugins-narrow-large-font", target_width_dp=320, font_scale=2.00' in acceptance
    assert (
        'device.capture(\n'
        '        "settings-plugins-install-narrow-large-font",\n'
        '        required_actions=("Choose APK",),\n'
        '    )'
        in acceptance
    )


def test_top_bar_runtime_status_is_not_color_only():
    shell = (KOTLIN / "ui/ZaraApp.kt").read_text()
    assert '"Local server status: ${localState.phase.name.lowercase()}"' in shell
    assert '"Remote connection status: ${connectionLabel(state.server)}"' in shell
    assert "semantics { contentDescription = description }" in shell
