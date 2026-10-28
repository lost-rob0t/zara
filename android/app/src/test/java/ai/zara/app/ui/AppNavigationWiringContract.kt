package ai.zara.app.ui

import java.io.File

object AppNavigationWiringContract {
    private fun app() = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
    private fun bars() = File("src/main/java/ai/zara/app/ui/ZaraNavigationBars.kt").readText()

    fun stateIsSavedPerRoute() {
        val source = app()
        check(source.contains("rememberSaveable(stateSaver = AppNavigationSaver)"))
        check(source.contains("rememberSaveableStateHolder()"))
        check(source.contains("SaveableStateProvider(navigation.route.name)"))
        check(source.contains("navigation.selectMenu(destination)"))
        check(source.contains("navigation.selectRoute(destination)"))
    }

    fun drawerAndRailShareExactlyThreeMenus() {
        val drawer = app().substringAfter("private fun ZaraDrawer(")
            .substringBefore("private fun DrawerDividerLabel(")
        check(drawer.contains("AppMenu.entries.forEach"))
        check(!drawer.contains("AppSurface.entries"))
        check(drawer.contains("verticalScroll(rememberScrollState())"))
        check(bars().contains("AppMenu.entries.forEach"))
        check(bars().contains("NavigationRailItem("))
        check(!app().contains("NavigationBarItem("))
    }

    fun tabsAreScrollableAndLabeled() {
        val source = bars()
        check(source.contains("if (navigation.menu == AppMenu.Settings) return"))
        check(source.contains("ScrollableTabRow("))
        check(source.contains("routesFor(navigation.menu)"))
        check(source.contains("selected = route == navigation.route"))
        check(source.contains("Text(route.label"))
        check(source.contains("heightIn(min = 48.dp)"))
    }

    fun settingsSectionsDoNotRemainOneLongForm() {
        val settings = app().substringAfter("private fun SettingsSurface(")
            .substringBefore("private fun DiagnosticsSurface(")
        check(settings.contains("when (section)"))
        val overview = settings.substringAfter("AppRoute.Settings -> {").substringBefore("AppRoute.Runtime -> {")
        val runtime = settings.substringAfter("AppRoute.Runtime -> {").substringBefore("AppRoute.Permissions -> {")
        val permissions = settings.substringAfter("AppRoute.Permissions -> {").substringBefore("AppRoute.Connection -> {")
        val connection = settings.substringAfter("AppRoute.Connection -> {").substringBefore("AppRoute.Updates -> {")
        val updates = settings.substringAfter("AppRoute.Updates -> {").substringBefore("else ->")
        check(overview.contains("SettingsOverviewContent("))
        check(runtime.contains("LOCAL RUNTIME"))
        check(runtime.contains("LocalModelSettingsCard("))
        check(runtime.contains("onSetLocalEmbeddingEnabled"))
        check(!runtime.contains("IDENTITY"))
        check(permissions.contains("ASSISTANT"))
        check(permissions.contains("onRequestMicrophonePermission"))
        check(connection.contains("IDENTITY"))
        check(connection.contains("ConnectionControls("))
        check(connection.contains("onReplaceServerPin"))
        check(updates.contains("SELF UPDATE"))
        check(!updates.contains("IDENTITY"))
    }

    fun adaptiveLayoutAndImeInsetsAreWired() {
        val source = app()
        check(source.contains("BoxWithConstraints("))
        check(source.contains("usesNavigationRail(maxWidth.value)"))
        check(source.contains("if (showRail)"))
        check(source.contains("ZaraNavigationRail("))
        check(source.contains("consumeWindowInsets(padding)"))
        check(source.contains("imePadding()"))
        check(source.contains("!drawerState.isOpen && navigation.back() != null"))
        check(source.contains("contentDescription = \"Open navigation menu\""))
    }

    @JvmStatic
    fun main(args: Array<String>) {
        stateIsSavedPerRoute()
        drawerAndRailShareExactlyThreeMenus()
        tabsAreScrollableAndLabeled()
        settingsSectionsDoNotRemainOneLongForm()
        adaptiveLayoutAndImeInsetsAreWired()
        println("PASS: 5 source-wiring contracts (not Android rendering tests)")
    }
}
