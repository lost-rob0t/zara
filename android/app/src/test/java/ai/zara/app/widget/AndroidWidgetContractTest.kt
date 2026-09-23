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
    fun `widget clicks enter the canonical AppNavigation through a bounded bridge`() {
        val provider = File("src/main/java/ai/zara/app/widget/ZaraWidgetProvider.kt").readText()
        val bridge = File("src/main/java/ai/zara/app/widget/WidgetNavigationRequest.kt").readText()
        val navigation = File("src/main/java/ai/zara/app/ui/AppNavigation.kt").readText()
        val style = File("src/main/assets/prolog/widget_styles.pl").readText()

        assertTrue(provider.contains("WidgetRoute"))
        assertTrue(provider.contains("FLAG_IMMUTABLE"))
        assertTrue(provider.contains("WidgetRouteReceiver"))
        assertTrue(bridge.contains("AppRoute"))
        assertTrue(bridge.contains("WidgetRoute.entries"))
        assertTrue(bridge.contains("MainActivity::class.java"))
        assertTrue(navigation.contains("WidgetNavigationRequest"))
        assertTrue(navigation.contains("selectRoute"))
        assertFalse("widgets must not create a second navigation owner", provider.contains("requestedSurface"))
        assertFalse("widgets must not create a second AppNavigation", bridge.contains("AppNavigation("))

        assertTrue(style.contains("zara_widget_stylesheet(1)."))
        assertTrue(style.contains("widget_color("))
        assertTrue(style.contains("widget_metric("))
        assertTrue(style.contains("widget_text("))
        assertTrue(style.contains("widget_flag("))
        assertTrue(style.contains("widget_action("))
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
    }

    @Test
    fun `appearance owns widget style import export reset and live refresh`() {
        val activity = File("src/main/java/ai/zara/app/MainActivity.kt").readText()
        val shell = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()

        assertTrue(activity.contains("ActivityResultContracts.OpenDocument"))
        assertTrue(activity.contains("ActivityResultContracts.CreateDocument"))
        assertTrue(activity.contains("WidgetStyleStore"))
        assertTrue(activity.contains("ZaraWidgetUpdater.refreshAll"))
        assertTrue(shell.contains("Import .pl"))
        assertTrue(shell.contains("Export .pl"))
        assertTrue(shell.contains("Reset widget style"))
    }
}
