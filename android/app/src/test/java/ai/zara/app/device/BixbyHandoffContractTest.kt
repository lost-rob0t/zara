package ai.zara.app.device

import ai.zara.app.prolog.AndroidAutomationCatalog
import ai.zara.app.runtime.LocalQueryResult
import java.io.File
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNull
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
    fun `local semantic Bixby frame executes the reviewed app handoff`() {
        val launcher = FakeAppLauncher(setOf("bixby"))
        val handoff = BixbyLocalHandoff(OpenAppAdapter(launcher))
        val result = LocalQueryResult(
            query = "resolve_frames(...) ",
            terms = listOf(BIXBY_OPEN_FRAME),
            generation = 1,
        )

        assertEquals(DeviceActionResult.Completed, handoff.dispatch(result))
        assertEquals(listOf("bixby"), launcher.launched)
    }

    @Test
    fun `local semantic handoff ignores unrelated frames`() {
        val launcher = FakeAppLauncher(setOf("bixby"))
        val handoff = BixbyLocalHandoff(OpenAppAdapter(launcher))
        val result = LocalQueryResult(
            query = "resolve_frames(...) ",
            terms = listOf(
                "frame(intent(ns(app),name(open)),[slot(name(target),value(ref(kind(app_alias),id(youtube))),origin(utterance))],complete)",
            ),
            generation = 1,
        )

        assertNull(handoff.dispatch(result))
        assertTrue(launcher.launched.isEmpty())
    }

    @Test
    fun `local semantic handoff fails closed on extra frames`() {
        val launcher = FakeAppLauncher(setOf("bixby"))
        val handoff = BixbyLocalHandoff(OpenAppAdapter(launcher))
        val result = LocalQueryResult(
            query = "resolve_frames(...) ",
            terms = listOf(BIXBY_OPEN_FRAME, BIXBY_OPEN_FRAME),
            generation = 1,
        )

        assertNull(handoff.dispatch(result))
        assertTrue(launcher.launched.isEmpty())
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

    @Test
    fun `local chat wires semantic resolution into Bixby handoff`() {
        val session = File("src/main/java/ai/zara/app/AndroidAppSession.kt").readText()

        assertTrue(session.contains("BixbyLocalHandoff"))
        assertTrue(session.contains("bixbyHandoff.dispatch(result)"))
    }

    private class FakeAppLauncher(
        var available: Set<String>,
    ) : AppLauncher {
        val launched = mutableListOf<String>()

        override fun isAvailable(alias: String): Boolean = alias in available

        override fun launch(alias: String) {
            check(alias in available)
            launched += alias
        }
    }

    private companion object {
        const val BIXBY_OPEN_FRAME =
            "frame(intent(ns(app),name(open)),[slot(name(target),value(ref(kind(app_alias),id(bixby))),origin(utterance))],complete)"
    }
}
