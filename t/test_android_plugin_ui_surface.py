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
    permissions = {
        item.get(ANDROID + "name") for item in manifest.findall("uses-permission")
    }
    assert "android.permission.REQUEST_INSTALL_PACKAGES" in permissions
    assert "android.permission.QUERY_ALL_PACKAGES" not in permissions
    assert "android.permission.MANAGE_EXTERNAL_STORAGE" not in permissions


def test_device_evidence_matrix_already_captures_plugins_settings_tab():
    acceptance = (ROOT / "android/integration/device_acceptance.py").read_text()
    assert '"Plugins",' in acceptance
    assert 'device.capture(f"settings-{tab.lower()}")' in acceptance
