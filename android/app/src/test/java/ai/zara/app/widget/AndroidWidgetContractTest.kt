package ai.zara.app.widget

import java.io.File
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
    fun `widget clicks are bounded app routes and style is Prolog owned`() {
        val provider = File("src/main/java/ai/zara/app/widget/ZaraWidgetProvider.kt").readText()
        val activity = File("src/main/java/ai/zara/app/MainActivity.kt").readText()
        val shell = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val style = File("src/main/assets/prolog/widget_styles.pl").readText()

        assertTrue(provider.contains("WidgetRoute"))
        assertTrue(provider.contains("PendingIntent.getActivity"))
        assertTrue(activity.contains("WIDGET_ROUTE"))
        assertTrue(shell.contains("requestedSurface"))
        assertTrue(style.contains("zara_widget_stylesheet(1)."))
        assertTrue(style.contains("widget_color("))
        assertTrue(style.contains("widget_metric("))
        assertTrue(style.contains("widget_text("))
        assertTrue(style.contains("widget_flag("))
        assertTrue(style.contains("widget_action("))
    }

    @Test
    fun `themes screen provides import export reset and live widget refresh`() {
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
