package ai.zara.app.widget

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class AndroidWidgetContractTest {
    @Test
    fun `manifest exposes three resizeable home screen widget providers`() {
        val manifest = File("src/main/AndroidManifest.xml").readText()
        listOf(
            "ZaraAssistantWidgetProvider",
            "ZaraRuntimeWidgetProvider",
            "ZaraActionsWidgetProvider",
        ).forEach { provider -> assertTrue("missing $provider", manifest.contains(provider)) }
        assertTrue(manifest.contains("android.appwidget.action.APPWIDGET_UPDATE"))

        listOf("assistant", "runtime", "actions").forEach { name ->
            val info = File("src/main/res/xml/zara_${name}_widget_info.xml").readText()
            assertTrue(info.contains("resizeMode=\"horizontal|vertical\""))
            assertTrue(info.contains("widgetCategory=\"home_screen\""))
            assertTrue(info.contains("initialLayout=\"@layout/widget_zara\""))
        }
    }

    @Test
    fun `widget clicks use durable activity ingress and canonical AppNavigation`() {
        val provider = File("src/main/java/ai/zara/app/widget/ZaraWidgetProvider.kt").readText()
        val bridge = File("src/main/java/ai/zara/app/widget/WidgetNavigationRequest.kt").readText()
        val activity = File("src/main/java/ai/zara/app/MainActivity.kt").readText()
        val shell = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val navigation = File("src/main/java/ai/zara/app/ui/AppNavigation.kt").readText()

        assertTrue(provider.contains("WidgetRoute"))
        assertTrue(provider.contains("FLAG_IMMUTABLE"))
        assertTrue(provider.contains("PendingIntent.getBroadcast"))
        assertTrue(provider.contains("WidgetRouteReceiver::class.java"))
        assertFalse("widget provider must not bypass the bounded ingress", provider.contains("PendingIntent.getActivity"))

        assertTrue(bridge.contains("WidgetRoute.entries"))
        assertTrue(bridge.contains("MainActivity::class.java"))
        assertTrue(bridge.contains("Intent.FLAG_ACTIVITY_SINGLE_TOP"))
        assertFalse("widget launch must not destroy the current task", bridge.contains("Intent.FLAG_ACTIVITY_CLEAR_TASK"))
        assertFalse("route ingress must survive process death instead of using process-local memory", bridge.contains("AtomicReference"))
        assertFalse("route ingress must not use a process-local request singleton", bridge.contains("WidgetNavigationRequest.request"))

        assertTrue(activity.contains("consumeWidgetRoute(intent)"))
        assertTrue(activity.contains("override fun onNewIntent(intent: Intent)"))
        assertTrue(shell.contains("LaunchedEffect(widgetRouteIngress)"))
        assertTrue(shell.contains("navigation = navigation.selectRoute(requested)"))
        assertTrue(navigation.contains("selectRoute"))
        assertFalse("widgets must not create a second navigation owner", provider.contains("requestedSurface"))
        assertFalse("widgets must not create a second AppNavigation", bridge.contains("AppNavigation("))
    }

    @Test
    fun `canonical runtime observers persist and refresh widget truth`() {
        val activity = File("src/main/java/ai/zara/app/MainActivity.kt").readText()
        val runtime = File("src/main/java/ai/zara/app/widget/WidgetRuntimeSnapshot.kt").readText()

        assertTrue(activity.contains("WidgetRuntimeSnapshotStore"))
        assertTrue(activity.contains("persistWidgetRuntimeSnapshot"))
        assertTrue(activity.contains("appSession.setStateObserver"))
        assertTrue(activity.contains("appSession.setLocalServerObserver"))
        assertTrue(activity.contains("ZaraWidgetUpdater.refreshAll"))
        assertTrue(runtime.contains("freshnessMillis"))
        assertTrue(runtime.contains("STALE"))
        assertTrue(runtime.contains("LOCAL UNKNOWN"))
        assertTrue(runtime.contains("UNKNOWN"))
    }

    @Test
    fun `widget routes are the current canonical route whitelist`() {
        val style = File("src/main/java/ai/zara/app/widget/WidgetStyle.kt").readText()
        listOf(
            "CHAT(\"chat\", AppRoute.Chat)",
            "VOICE(\"voice\", AppRoute.Voice)",
            "LOGIC(\"logic\", AppRoute.Logic)",
            "PROJECTS(\"projects\", AppRoute.Projects)",
            "SCHEDULED(\"scheduled\", AppRoute.Scheduled)",
            "RUNTIME(\"runtime\", AppRoute.Runtime)",
            "CONNECTION(\"connection\", AppRoute.Connection)",
            "PERMISSIONS(\"permissions\", AppRoute.Permissions)",
            "APPEARANCE(\"appearance\", AppRoute.Appearance)",
            "PLUGINS(\"plugins\", AppRoute.Plugins)",
            "UPDATES(\"updates\", AppRoute.Updates)",
            "DIAGNOSTICS(\"diagnostics\", AppRoute.Diagnostics)",
            "ABOUT(\"about\", AppRoute.About)",
        ).forEach { contract -> assertTrue("missing route $contract", style.contains(contract)) }
        assertFalse("legacy parallel themes route must not survive", style.contains("THEMES(\"themes\""))
        assertFalse("legacy parallel remote route must not survive", style.contains("REMOTE(\"remote\""))
        assertTrue("legacy themes atom may only be normalized to Appearance", style.contains("\"themes\" -> APPEARANCE"))
        assertTrue("legacy remote atom may only be normalized to Runtime", style.contains("\"remote\" -> RUNTIME"))
    }

    @Test
    fun `appearance owns widget style import export reset and live refresh`() {
        val shell = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val appearance = File("src/main/java/ai/zara/app/ui/WidgetAppearanceControls.kt").readText()

        assertTrue(shell.contains("WidgetAppearanceControls(selected)"))
        assertTrue(appearance.contains("ActivityResultContracts.OpenDocument"))
        assertTrue(appearance.contains("ActivityResultContracts.CreateDocument"))
        assertTrue(appearance.contains("WidgetStyleEnvironment.import"))
        assertTrue(appearance.contains("WidgetStyleEnvironment.export"))
        assertTrue(appearance.contains("WidgetStyleEnvironment.reset"))
        assertTrue(appearance.contains("ZaraWidgetUpdater.refreshAll"))
        assertTrue(appearance.contains("Import .pl"))
        assertTrue(appearance.contains("Export .pl"))
        assertTrue(appearance.contains("Reset widget style"))
        assertTrue(appearance.indexOf("WidgetStyleEnvironment.import") < appearance.indexOf("ZaraWidgetUpdater.refreshAll"))
    }
}
