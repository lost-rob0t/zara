from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
APP = ROOT / "android/app/src/main/java/ai/zara/app/ui/ZaraApp.kt"
NAV = ROOT / "android/app/src/main/java/ai/zara/app/ui/AppNavigation.kt"


def test_all_routed_feature_tabs_are_real_surfaces():
    app = APP.read_text()
    assert "AppSurface.Scheduled -> ScheduledSurface(" in app
    assert "AppSurface.Plugins -> PluginInstallSurface(padding)" in app
    assert "AppSurface.Scheduled -> GatedSurface" not in app
    assert "AppSurface.Plugins -> GatedSurface" not in app


def test_plugins_and_scheduled_stay_inside_canonical_three_menu_navigation():
    nav = NAV.read_text()
    assert 'Scheduled(AppMenu.Workspace, "Scheduled")' in nav
    assert 'Plugins(AppMenu.Settings, "Plugins")' in nav
    assert 'Chat("Chat"' in nav
    assert 'Workspace("Workspace"' in nav
    assert 'Settings("Settings"' in nav
