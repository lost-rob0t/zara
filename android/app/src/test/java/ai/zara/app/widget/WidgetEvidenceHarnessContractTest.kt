package ai.zara.app.widget

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class WidgetEvidenceHarnessContractTest {
    @Test
    fun `debug evidence harness pins only canonical widget providers and cannot ship in main`() {
        val mainManifest = File("src/main/AndroidManifest.xml").readText()
        val debugManifest = File("src/debug/AndroidManifest.xml")
        val evidenceActivity = File(
            "src/debug/java/ai/zara/app/widget/WidgetEvidenceActivity.kt",
        )

        assertFalse(mainManifest.contains("WidgetEvidenceActivity"))
        assertTrue("debug evidence manifest must exist", debugManifest.isFile)
        assertTrue("debug evidence activity must exist", evidenceActivity.isFile)

        val debugManifestSource = debugManifest.readText()
        val activitySource = evidenceActivity.readText()
        assertTrue(debugManifestSource.contains(".widget.WidgetEvidenceActivity"))
        assertTrue(debugManifestSource.contains("android:exported=\"true\""))
        assertTrue(activitySource.contains("isRequestPinAppWidgetSupported"))
        assertTrue(activitySource.contains("requestPinAppWidget"))
        assertTrue(activitySource.contains("\"assistant\" -> ZaraAssistantWidgetProvider::class.java"))
        assertTrue(activitySource.contains("\"runtime\" -> ZaraRuntimeWidgetProvider::class.java"))
        assertTrue(activitySource.contains("\"actions\" -> ZaraActionsWidgetProvider::class.java"))
        assertFalse("evidence harness must not load arbitrary provider classes", activitySource.contains("Class.forName"))
    }

    @Test
    fun `debug evidence harness can project bounded fresh stale and corrupt runtime states`() {
        val activitySource = File(
            "src/debug/java/ai/zara/app/widget/WidgetEvidenceActivity.kt",
        ).readText()

        assertTrue(activitySource.contains("WidgetRuntimeSnapshotStore"))
        assertTrue(activitySource.contains("ZaraWidgetUpdater.refreshAll"))
        assertTrue(activitySource.contains("\"fresh\""))
        assertTrue(activitySource.contains("\"stale\""))
        assertTrue(activitySource.contains("\"corrupt\""))
        assertTrue(activitySource.contains("DEFAULT_FRESHNESS_MILLIS"))
        assertFalse("evidence harness must not mutate production preferences", activitySource.contains("SharedPreferences"))
    }
}
