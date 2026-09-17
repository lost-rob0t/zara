package ai.zara.app.device

import ai.zara.app.prolog.AndroidAutomationCatalog
import java.io.File
import org.junit.Assert.assertEquals
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
    fun `seeded Prolog automation can call Bixby through open app`() {
        val source = AndroidAutomationCatalog.examples.single().source

        assertTrue(source.contains("automation(open_bixby,"))
        assertTrue(source.contains("open_app(bixby)"))
    }
}
