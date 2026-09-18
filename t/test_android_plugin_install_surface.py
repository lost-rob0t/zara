"""Supplement executable Kotlin tests with Android/Compose wiring contracts."""
from pathlib import Path
import xml.etree.ElementTree as ET

ROOT = Path(__file__).resolve().parents[1]
MAIN = ROOT / "android/app/src/main"
KOTLIN = MAIN / "java/ai/zara/app"
ANDROID = "{http://schemas.android.com/apk/res/android}"


def test_plugin_route_is_canonical_settings_plugins():
    source = (KOTLIN / "ui/ZaraApp.kt").read_text()
    navigation = (KOTLIN / "ui/AppNavigation.kt").read_text()
    assert 'Plugins(AppMenu.Settings, "Plugins")' in navigation
    assert "AppSurface.Plugins -> PluginInstallSurface(padding)" in source
    assert "AppSurface.Plugins -> GatedSurface" not in source


def test_plugin_surface_does_not_create_a_second_settings_tab_system():
    surface = (KOTLIN / "ui/PluginInstallSettings.kt").read_text()
    assert "internal fun PluginInstallSurface(padding: PaddingValues)" in surface
    assert "SettingsTabBar" not in surface
    assert "enum class SettingsTab" not in surface


def test_install_callback_is_private_and_in_the_ui_process():
    app = ET.parse(MAIN / "AndroidManifest.xml").getroot().find("application")
    receiver = next(item for item in app.findall("receiver")
                    if item.get(ANDROID + "name") == ".plugins.PluginInstallReceiver")
    activity = next(item for item in app.findall("activity")
                    if item.get(ANDROID + "name") == ".MainActivity")
    assert receiver.get(ANDROID + "exported") == "false"
    assert receiver.get(ANDROID + "process") == activity.get(ANDROID + "process")
    assert not receiver.findall("intent-filter")


def test_installer_is_application_owned_and_observer_is_released():
    application = (KOTLIN / "ZaraApplication.kt").read_text()
    surface = (KOTLIN / "ui/PluginInstallSettings.kt").read_text()
    assert "val pluginInstaller: PluginApkInstaller by lazy" in application
    assert "as ZaraApplication).pluginInstaller" in surface
    assert "onDispose { subscription.close() }" in surface
    assert "PluginApkInstaller(context)" not in surface


def test_install_uses_a_file_picker_and_native_confirmation():
    surface = (KOTLIN / "ui/PluginInstallSettings.kt").read_text()
    installer = (KOTLIN / "plugins/PluginApkInstaller.kt").read_text()
    assert "ActivityResultContracts.OpenDocument()" in surface
    assert "ActivityResultContracts.StartActivityForResult()" in surface
    assert "Settings.ACTION_MANAGE_UNKNOWN_APP_SOURCES" in surface
    assert "setRequireUserAction(PackageInstaller.SessionParams.USER_ACTION_REQUIRED)" in installer
    assert "PluginApkSecurity.copyVerified(input, output, candidate.sha256)" in installer
    assert "Intent(context, PluginInstallReceiver::class.java)" in installer
    assert "PackageInstaller.STATUS_SUCCESS ->" in installer


def test_installer_does_not_request_broad_storage_or_package_visibility():
    manifest = ET.parse(MAIN / "AndroidManifest.xml").getroot()
    tools = "{http://schemas.android.com/tools}"
    permissions = {item.get(ANDROID + "name") for item in manifest.findall("uses-permission")
                   if item.get(tools + "node") != "remove"}
    assert "android.permission.REQUEST_INSTALL_PACKAGES" in permissions
    assert not permissions.intersection({"android.permission.MANAGE_EXTERNAL_STORAGE",
                                         "android.permission.READ_EXTERNAL_STORAGE",
                                         "android.permission.QUERY_ALL_PACKAGES"})
