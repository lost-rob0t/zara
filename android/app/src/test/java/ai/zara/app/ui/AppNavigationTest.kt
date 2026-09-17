package ai.zara.app.ui

import org.junit.Test

class AppNavigationTest {
    @Test
    fun pluginInstallationIsNotADrawerDestination() {
        val destinations = drawerSurfaces()
        check(AppSurface.Plugins !in destinations)
        check(destinations == AppSurface.entries.filter { it != AppSurface.Plugins })
        check(destinations.count { it == AppSurface.Settings } == 1)
    }

    @Test
    fun aRestoredPluginRouteOpensTheSettingsPluginTab() {
        check(displaySurface(AppSurface.Plugins) == AppSurface.Settings)
        check(initialSettingsTab(AppSurface.Plugins) == SettingsTab.Plugins)
    }

    @Test
    fun otherRoutesAndDefaultSettingsStayUnchanged() {
        drawerSurfaces().forEach { destination ->
            check(displaySurface(destination) == destination)
            check(initialSettingsTab(destination) == SettingsTab.General)
        }
    }

    @Test
    fun settingsTabsKeepGeneralFirstAndPluginsSecondary() {
        check(SettingsTab.entries == listOf(SettingsTab.General, SettingsTab.Plugins))
    }
}
