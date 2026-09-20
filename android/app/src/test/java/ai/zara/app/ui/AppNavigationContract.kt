package ai.zara.app.ui

object AppNavigationContract {
    fun threeMenus() {
        check(AppMenu.entries.map { it.label } == listOf("Chat", "Workspace", "Settings"))
    }

    fun completeRouteInventory() {
        check(routesFor(AppMenu.Chat) == listOf(AppRoute.Chat, AppRoute.Voice))
        check(routesFor(AppMenu.Workspace) == listOf(AppRoute.Logic, AppRoute.Projects, AppRoute.Scheduled))
        check(routesFor(AppMenu.Settings) == listOf(
            AppRoute.Runtime, AppRoute.ModelApis, AppRoute.Connection, AppRoute.Permissions, AppRoute.Appearance,
            AppRoute.Plugins, AppRoute.Updates, AppRoute.Diagnostics, AppRoute.About,
        ))
        check(AppMenu.entries.flatMap(::routesFor).toSet() == AppRoute.entries.toSet())
        check(AppMenu.entries.flatMap(::routesFor).size == AppRoute.entries.size)
    }

    fun everyRouteCanBeSelected() {
        for (start in AppRoute.entries) {
            for (destination in AppRoute.entries) {
                val state = AppNavigation().selectRoute(start).selectRoute(destination)
                check(state.route == destination)
                check(state.menu == destination.menu)
            }
        }
    }

    fun menuSelectionsAreIndependent() {
        val state = AppNavigation().selectRoute(AppRoute.Voice)
            .selectRoute(AppRoute.Scheduled).selectRoute(AppRoute.Permissions)
        check(state.selectMenu(AppMenu.Chat).route == AppRoute.Voice)
        check(state.selectMenu(AppMenu.Workspace).route == AppRoute.Scheduled)
        check(state.selectMenu(AppMenu.Settings).route == AppRoute.Permissions)
        check(state.selectMenu(AppMenu.Settings) == state)
    }

    fun savedStateRoundTrips() {
        for (chat in routesFor(AppMenu.Chat)) {
            for (workspace in routesFor(AppMenu.Workspace)) {
                for (settings in routesFor(AppMenu.Settings)) {
                    for (menu in AppMenu.entries) {
                        val state = AppNavigation(menu, chat, workspace, settings)
                        check(AppNavigation.restore(state.save()) == state)
                    }
                }
            }
        }
    }

    fun corruptStateFallsBackWithoutCrossMenuRoutes() {
        check(AppNavigation.restore(emptyList()) == AppNavigation())
        check(AppNavigation.restore(listOf("removed", "unknown", "unknown", "unknown")) == AppNavigation())
        val restored = AppNavigation.restore(listOf("Settings", "Updates", "Chat", "Voice"))
        check(restored.menu == AppMenu.Settings)
        check(restored.chat == AppRoute.Chat)
        check(restored.workspace == AppRoute.Logic)
        check(restored.settings == AppRoute.Runtime)
        check(AppNavigation.restore(listOf("Settings", "Voice", "Scheduled", "Updates")).route == AppRoute.Updates)
    }

    fun invalidConstructionIsRejected() {
        check(runCatching { AppNavigation(chat = AppRoute.Logic) }.exceptionOrNull() is IllegalArgumentException)
        check(runCatching { AppNavigation(workspace = AppRoute.Voice) }.exceptionOrNull() is IllegalArgumentException)
        check(runCatching { AppNavigation(settings = AppRoute.Chat) }.exceptionOrNull() is IllegalArgumentException)
    }

    fun backReturnsThroughMenuRootThenChat() {
        check(AppNavigation().back() == null)
        check(AppNavigation().selectRoute(AppRoute.Voice).back()?.route == AppRoute.Chat)
        check(AppNavigation().selectRoute(AppRoute.Updates).back()?.route == AppRoute.Runtime)
        check(AppNavigation().selectRoute(AppRoute.Scheduled).back()?.route == AppRoute.Logic)
        for (route in AppRoute.entries) {
            var state: AppNavigation? = AppNavigation().selectRoute(route)
            var steps = 0
            while (state != null && steps < 4) {
                state = state.back()
                steps += 1
            }
            check(state == null) { "Back loop from $route" }
        }
    }

    fun railBreakpointUsesAvailableWindowWidth() {
        check(!usesNavigationRail(0f))
        check(!usesNavigationRail(320f))
        check(!usesNavigationRail(599.99f))
        check(usesNavigationRail(600f))
        check(usesNavigationRail(840f))
        check(!usesNavigationRail(Float.NaN))
        check(!usesNavigationRail(Float.POSITIVE_INFINITY))
    }

    fun savedKeysAreStableNames() {
        val state = AppNavigation().selectRoute(AppRoute.Permissions)
        check(state.save() == listOf("Settings", "Chat", "Logic", "Permissions"))
    }

    @JvmStatic
    fun main(args: Array<String>) {
        threeMenus()
        completeRouteInventory()
        everyRouteCanBeSelected()
        menuSelectionsAreIndependent()
        savedStateRoundTrips()
        corruptStateFallsBackWithoutCrossMenuRoutes()
        invalidConstructionIsRejected()
        backReturnsThroughMenuRootThenChat()
        railBreakpointUsesAvailableWindowWidth()
        savedKeysAreStableNames()
        println("PASS: 10 navigation contracts; model APIs included in the settings route inventory")
    }
}
