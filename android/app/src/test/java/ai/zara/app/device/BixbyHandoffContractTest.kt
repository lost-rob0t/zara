package ai.zara.app.device

import ai.zara.app.prolog.AndroidAutomationCatalog
import java.io.File
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class BixbyHandoffContractTest {
    @Test
    fun `bixby is a reviewed Samsung launch alias`() {
        assertTrue("bixby" in AndroidAppAliases.reviewed)
        assertEquals(
            listOf("com.samsung.android.bixby.agent"),
            AndroidAppAliases.packageCandidates(" BIXBY "),
        )
    }

    @Test
    fun `manifest makes the reviewed Bixby package visible`() {
        val manifest = File("src/main/AndroidManifest.xml").readText()

        assertTrue(
            manifest.contains("<package android:name=\"com.samsung.android.bixby.agent\" />"),
        )
    }

    @Test
    fun `Bixby ships as its own Prolog automation source`() {
        val bixby = AndroidAutomationCatalog.examples.single {
            it.fileName == "zara_bixby.pl"
        }

        assertTrue(bixby.source.contains("automation(open_bixby,"))
        assertTrue(bixby.source.contains("open_app(bixby)"))
        assertFalse(
            AndroidAutomationCatalog.examples.single {
                it.fileName == "android_automation.pl"
            }.source.contains("open_bixby"),
        )
    }

    @Test
    fun `automation screen seeds missing catalog sources without replacing existing ones`() {
        val activity = File("src/main/java/ai/zara/app/automation/AutomationActivity.kt").readText()

        assertTrue(activity.contains("AndroidAutomationCatalog.examples.filterNot"))
        assertTrue(activity.contains("source.fileName in existingNames"))
        assertFalse(activity.contains("AndroidAutomationCatalog.examples.single()"))
    }

    @Test
    fun `automation screen exposes Bixby without raw intent input`() {
        val activity = File("src/main/java/ai/zara/app/automation/AutomationActivity.kt").readText()

        assertTrue(activity.contains("onRun(\"open_bixby\")"))
        assertTrue(activity.contains("Text(\"Bixby\")"))
    }
}
