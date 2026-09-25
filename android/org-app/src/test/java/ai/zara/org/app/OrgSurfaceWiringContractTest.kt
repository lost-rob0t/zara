package ai.zara.org.app

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class OrgSurfaceWiringContractTest {
    private val source: String
        get() = File("src/main/java/ai/zara/org/app/MainActivity.kt").readText()

    @Test fun `flagship exposes every org system surface over one canonical screen`() {
        assertTrue(source.contains("OrgWorkspaceScreen("))
        assertTrue(source.contains("OrgSurfaceTab(\"Todo\") { model -> TodoSurface(model) }"))
        assertTrue(source.contains("OrgSurfaceTab(\"Roam\") { model -> RoamSurface(model) }"))
        assertTrue(source.contains("OrgSurfaceTab(\"Daily\") { model -> DailySurface(model) }"))
        assertTrue(source.contains("OrgSurfaceTab(\"Reminders\") { model -> RemindersSurface(model) }"))
        assertTrue(source.contains("OrgSurfaceTab(\"Timers\") { model -> TimersSurface(model) }"))
        assertTrue(source.contains("OrgSurfaceTab(\"Graph\") { model -> GraphSurface(model) }"))
        assertTrue(source.contains("OrgSurfaceTab(\"Editor\") { model -> EditorSurface(model) }"))
        assertTrue(source.contains("OrgSurfaceTab(\"Home\") { model -> HomeSurface(model) }"))
    }

    @Test fun `surfaces come from the one shared surface library`() {
        assertTrue(source.contains("import ai.zara.org.surfaces.OrgWorkspaceScreen"))
        assertFalse(source.contains("Room.databaseBuilder"))
        assertFalse(source.contains("SQLiteDatabase"))
    }

    @Test fun `daily configuration is never invented by the app`() {
        assertFalse(source.contains("Documents/Notes/org"))
        assertFalse(source.contains("ZoneId.of("))
    }
}
