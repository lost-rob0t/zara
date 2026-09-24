package ai.zara.app.ui

import org.junit.Test

class AppNavigationPluginInstallTest {
    @Test
    fun pluginsIsASettingsRouteNotAPrimaryMenu() {
        check(AppRoute.Plugins.menu == AppMenu.Settings)
        check(AppRoute.Plugins in routesFor(AppMenu.Settings))
        check(AppMenu.entries == listOf(AppMenu.Chat, AppMenu.Workspace, AppMenu.Settings))
    }

    @Test
    fun selectingPluginsPreservesCanonicalSettingsNavigation() {
        val navigation = AppNavigation().selectRoute(AppRoute.Plugins)
        check(navigation.menu == AppMenu.Settings)
        check(navigation.route == AppRoute.Plugins)
        check(navigation.settings == AppRoute.Plugins)
    }

    @Test
    fun restoredPluginsRouteSurvivesSavedState() {
        val navigation = AppNavigation().selectRoute(AppRoute.Plugins)
        check(AppNavigation.restore(navigation.save()) == navigation)
    }
}
