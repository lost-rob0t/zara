"""Source/manifest contracts; these do not replace Android UI/device tests."""

from pathlib import Path
import xml.etree.ElementTree as ET


ROOT = Path(__file__).resolve().parents[1]
MAIN = ROOT / "android/app/src/main"
ANDROID = "{http://schemas.android.com/apk/res/android}"


def test_plugin_provider_is_private_and_scoped_to_apk_cache():
    manifest = ET.parse(MAIN / "AndroidManifest.xml").getroot()
    providers = [
        provider for provider in manifest.findall("application/provider")
        if provider.get(ANDROID + "name") == ".plugins.PluginApkFileProvider"
    ]
    assert len(providers) == 1
    provider = providers[0]
    assert provider.get(ANDROID + "exported") == "false"
    assert provider.get(ANDROID + "grantUriPermissions") == "true"
    assert provider.get(ANDROID + "authorities") == "${applicationId}.plugin-apks"
    metadata = provider.find("meta-data")
    assert metadata is not None
    assert metadata.get(ANDROID + "resource") == "@xml/plugin_apk_paths"
    roots = list(ET.parse(MAIN / "res/xml/plugin_apk_paths.xml").getroot())
    assert [(root.tag, root.get("path")) for root in roots] == [
        ("cache-path", "plugin-install/")
    ]


def test_only_scoped_installer_visibility_is_added():
    manifest = ET.parse(MAIN / "AndroidManifest.xml").getroot()
    queries = manifest.findall("queries/intent")
    assert any(
        intent.find("action") is not None
        and intent.find("action").get(ANDROID + "name") == "android.intent.action.INSTALL_PACKAGE"
        and intent.find("data") is not None
        and intent.find("data").get(ANDROID + "mimeType") == "application/vnd.android.package-archive"
        for intent in queries
    )
    permissions = {item.get(ANDROID + "name") for item in manifest.findall("uses-permission")}
    assert "android.permission.REQUEST_INSTALL_PACKAGES" in permissions
    assert "android.permission.QUERY_ALL_PACKAGES" not in permissions
    assert "android.permission.MANAGE_EXTERNAL_STORAGE" not in permissions


def test_plugin_route_uses_canonical_three_menu_settings_tab():
    app = (MAIN / "java/ai/zara/app/ui/ZaraApp.kt").read_text()
    navigation = (MAIN / "java/ai/zara/app/ui/AppNavigation.kt").read_text()
    assert 'Plugins(AppMenu.Settings, "Plugins")' in navigation
    assert "AppSurface.Plugins -> PluginSettingsSurface(pluginInstallUi, padding)" in app
    assert "AppSurface.Plugins -> GatedSurface" not in app


def test_install_callbacks_outlive_route_switching():
    app = (MAIN / "java/ai/zara/app/ui/ZaraApp.kt").read_text()
    assert app.index("val pluginInstallUi = rememberPluginInstallUi()") < app.index("when (selected)")
    ui = (MAIN / "java/ai/zara/app/ui/PluginSettings.kt").read_text()
    assert "internal enum class SettingsTab" not in ui
    assert "SettingsTabLayout" not in ui
    assert "mutableStateOf<StagedPluginApk?>(null)" in ui
    assert "Activity.RESULT_OK && completed != null" in ui
    assert "Activity.RESULT_CANCELED" in ui


def test_handoff_rechecks_reviewed_file_and_grants_read_only_to_system_installer():
    source = (MAIN / "java/ai/zara/app/plugins/PluginApkInstaller.kt").read_text()
    verify = source.index("actual.sha256 == candidate.sha256")
    share = source.index("val uri = FileProvider.getUriForFile")
    assert verify < share
    assert "identity == candidate.identity" in source
    assert "PackageManager.MATCH_SYSTEM_ONLY" in source
    assert "Intent.EXTRA_RETURN_RESULT, true" in source
    assert "Intent.FLAG_GRANT_READ_URI_PERMISSION" in source
    assert "FLAG_GRANT_WRITE_URI_PERMISSION" not in source
    assert "context.revokeUriPermission" in source
    assert "withContext(NonCancellable + Dispatchers.IO)" in source
