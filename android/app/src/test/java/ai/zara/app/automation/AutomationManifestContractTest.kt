package ai.zara.app.automation

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class AutomationManifestContractTest {
    @Test
    fun `automation surface declares platform owned special access`() {
        val manifest = File("src/main/AndroidManifest.xml").readText()
        val automation = manifest
            .substringAfter(".automation.AutomationActivity")
            .substringBefore("</activity>")
        val accessibility = manifest
            .substringAfter(".accessibility.ZaraAccessibilityService")
            .substringBefore("</service>")

        assertTrue(manifest.contains("android.permission.PACKAGE_USAGE_STATS"))
        assertTrue(manifest.contains("android.permission.WRITE_SETTINGS"))
        assertTrue(manifest.contains("android.permission.SYSTEM_ALERT_WINDOW"))
        assertTrue(automation.contains("android:process=\":voice\""))
        assertTrue(accessibility.contains("android:exported=\"true\""))
        assertTrue(accessibility.contains("android.permission.BIND_ACCESSIBILITY_SERVICE"))
        assertTrue(accessibility.contains("android:process=\":voice\""))
    }

    @Test
    fun `reviewed youtube package visibility includes stock and revanced candidates`() {
        val manifest = File("src/main/AndroidManifest.xml").readText()

        assertTrue(manifest.contains("com.google.android.youtube"))
        assertTrue(manifest.contains("app.revanced.android.youtube"))
        assertTrue(manifest.contains("app.rvx.android.youtube"))
    }

    @Test
    fun `remote device protocol is not silently expanded by local automation`() {
        val codec = File("src/main/java/ai/zara/app/runtime/ZaraCapabilityCodec.kt").readText()

        assertTrue(codec.contains("OpenApp(\"open_app\")"))
        assertTrue(codec.contains("OpenUri(\"open_uri\")"))
        assertFalse(codec.contains("AppSearch(\"app_search\")"))
    }
}
