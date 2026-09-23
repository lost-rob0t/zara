package ai.zara.app.ui

import ai.zara.app.widget.WidgetNavigationRequest

enum class AppMenu(val label: String, val glyph: String) {
    Chat("Chat", "⌂"),
    Workspace("Workspace", "λ"),
    Settings("Settings", "⚙"),
}

enum class AppRoute(val menu: AppMenu, val label: String) {
    Chat(AppMenu.Chat, "Chat"),
    Voice(AppMenu.Chat, "Voice"),
    Logic(AppMenu.Workspace, "Logic"),
    Projects(AppMenu.Workspace, "Projects"),
    Scheduled(AppMenu.Workspace, "Scheduled"),
    Runtime(AppMenu.Settings, "Runtime"),
    Connection(AppMenu.Settings, "Connection"),
    Permissions(AppMenu.Settings, "Permissions"),
    Appearance(AppMenu.Settings, "Appearance"),
    Plugins(AppMenu.Settings, "Plugins"),
    Updates(AppMenu.Settings, "Updates"),
    Diagnostics(AppMenu.Settings, "Diagnostics"),
    About(AppMenu.Settings, "About"),
}

fun routesFor(menu: AppMenu): List<AppRoute> = AppRoute.entries.filter { it.menu == menu }

fun usesNavigationRail(availableWidthDp: Float): Boolean =
    availableWidthDp.isFinite() && availableWidthDp >= 600f

private fun pendingWidgetRoute(menu: AppMenu): AppRoute? =
    WidgetNavigationRequest.peek()?.takeIf { it.menu == menu }

data class AppNavigation(
    val menu: AppMenu = WidgetNavigationRequest.peek()?.menu ?: AppMenu.Chat,
    val chat: AppRoute = pendingWidgetRoute(AppMenu.Chat) ?: AppRoute.Chat,
    val workspace: AppRoute = pendingWidgetRoute(AppMenu.Workspace) ?: AppRoute.Logic,
    val settings: AppRoute = pendingWidgetRoute(AppMenu.Settings) ?: AppRoute.Runtime,
) {
    init {
        require(chat.menu == AppMenu.Chat)
        require(workspace.menu == AppMenu.Workspace)
        require(settings.menu == AppMenu.Settings)
        WidgetNavigationRequest.peek()?.let { requested ->
            if (route == requested) WidgetNavigationRequest.consume(requested)
        }
    }

    val route: AppRoute
        get() = when (menu) {
            AppMenu.Chat -> chat
            AppMenu.Workspace -> workspace
            AppMenu.Settings -> settings
        }

    fun selectMenu(destination: AppMenu): AppNavigation = copy(menu = destination)

    fun selectRoute(destination: AppRoute): AppNavigation = when (destination.menu) {
        AppMenu.Chat -> copy(menu = AppMenu.Chat, chat = destination)
        AppMenu.Workspace -> copy(menu = AppMenu.Workspace, workspace = destination)
        AppMenu.Settings -> copy(menu = AppMenu.Settings, settings = destination)
    }

    fun back(): AppNavigation? {
        val root = routesFor(menu).first()
        if (route != root) return selectRoute(root)
        if (menu != AppMenu.Chat) return selectRoute(AppRoute.Chat)
        return null
    }

    fun save(): List<String> = listOf(menu.name, chat.name, workspace.name, settings.name)

    companion object {
        fun restore(values: List<String>): AppNavigation {
            fun routeAt(index: Int, menu: AppMenu): AppRoute =
                routesFor(menu).firstOrNull { it.name == values.getOrNull(index) }
                    ?: routesFor(menu).first()

            return AppNavigation(
                menu = AppMenu.entries.firstOrNull { it.name == values.getOrNull(0) } ?: AppMenu.Chat,
                chat = routeAt(1, AppMenu.Chat),
                workspace = routeAt(2, AppMenu.Workspace),
                settings = routeAt(3, AppMenu.Settings),
            )
        }
    }
}
