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
        check(source.contains("ScrollableTabRow("))
        check(source.contains("routesFor(navigation.menu)"))
        check(source.contains("selected = route == navigation.route"))
        check(source.contains("Text(route.label"))
        check(source.contains("heightIn(min = 48.dp)"))
    }

    fun settingsSectionsDoNotRemainOneLongForm() {
        val settings = app().substringAfter("private fun SettingsSurface(")
            .substringBefore("private fun DiagnosticsSurface(")
        check(settings.contains("when (section)")) { "Settings route switch missing" }
        check(settings.contains("AppRoute.Runtime -> {")) { "Runtime settings section missing" }
        check(settings.contains("AppRoute.RemoteApis -> {")) { "Remote APIs settings section missing" }
        check(settings.contains("AppRoute.Permissions -> {")) { "Permissions settings section missing" }
        check(settings.contains("AppRoute.Connection -> {")) { "Connection settings section missing" }
        check(settings.contains("AppRoute.Updates -> {")) { "Updates settings section missing" }

        val runtime = settings.substringAfter("AppRoute.Runtime -> {")
            .substringBefore("AppRoute.RemoteApis -> {")
        val remoteApis = settings.substringAfter("AppRoute.RemoteApis -> {")
            .substringBefore("AppRoute.Permissions -> {")
        val permissions = settings.substringAfter("AppRoute.Permissions -> {")
            .substringBefore("AppRoute.Connection -> {")
        val connection = settings.substringAfter("AppRoute.Connection -> {")
            .substringBefore("AppRoute.Updates -> {")
        val updates = settings.substringAfter("AppRoute.Updates -> {")

        check(runtime.contains("LOCAL ZARA SERVER")) { "Runtime lost local server controls" }
        check(runtime.contains("onSetLocalEmbeddingEnabled")) { "Runtime lost embedding controls" }
        check(!runtime.contains("IDENTITY")) { "Runtime leaked connection identity controls" }

        check(remoteApis.contains("REMOTE API STATUS")) { "Remote APIs status card missing" }
        check(remoteApis.contains("OpenRouter")) { "OpenRouter preset missing" }
        check(remoteApis.contains("RemoteProviderCard(")) { "Modern provider cards missing" }
        check(remoteApis.contains("Switch(")) { "Remote API enable switch missing" }
        check(remoteApis.contains("Generic OpenAI-compatible")) { "Generic OpenAI-compatible preset missing" }
        check(remoteApis.contains("Z.AI Coding Plan")) { "Z.AI preset missing" }
        check(remoteApis.contains("Android Keystore")) { "Remote API secret boundary is not visible" }
        check(remoteApis.contains("onSaveRemoteApi")) { "Remote API save callback missing" }
        check(remoteApis.contains("onClearRemoteApiKey")) { "Remote API key-clear callback missing" }

        check(permissions.contains("ASSISTANT")) { "Permissions lost Assistant controls" }
        check(permissions.contains("onRequestMicrophonePermission")) { "Permissions lost microphone controls" }

        check(connection.contains("IDENTITY")) { "Connection lost identity controls" }
        check(connection.contains("ConnectionControls(")) { "Connection controls missing" }
        check(connection.contains("onReplaceServerPin")) { "Server pin replacement missing" }

        check(updates.contains("SELF UPDATE")) { "Updates section missing" }
        check(!updates.substringBefore("else -> error").contains("IDENTITY")) {
            "Updates leaked connection identity controls"
        }
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
