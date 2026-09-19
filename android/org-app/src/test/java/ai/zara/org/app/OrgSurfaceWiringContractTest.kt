package ai.zara.org.app

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class OrgSurfaceWiringContractTest {
    private val source: String
        get() = File("src/main/java/ai/zara/org/app/MainActivity.kt").readText()

    @Test fun `flagship surfaces consume one canonical projection`() {
        assertTrue(source.contains("OrgSurface { TODO, ROAM, DAILY }"))
        assertTrue(source.contains("OrgHome.open(context)"))
        assertTrue(source.contains("OrgWorkspaceProjector.project(documents, OrgHome.dailySpec(context))"))
        assertTrue(source.contains("OrgSurface.TODO -> TodoSurface(projection.tasks, ::cycle)"))
        assertTrue(source.contains("OrgSurface.ROAM -> RoamSurface(projection.roam)"))
        assertTrue(source.contains("OrgSurface.DAILY -> DailySurface(projection)"))
    }

    @Test fun `todo mutations stay on canonical repository authority`() {
        assertTrue(source.contains("repo.cycleTodo(task)"))
        assertFalse(source.contains("mutableStateListOf<OrgTask>"))
        assertFalse(source.contains("Room.databaseBuilder"))
        assertFalse(source.contains("SQLiteDatabase"))
    }

    @Test fun `roam stays a derived canonical graph`() {
        assertTrue(source.contains("graph.search(query)"))
        assertTrue(source.contains("graph.backlinks(node.id)"))
        assertFalse(source.contains("RoomDatabase"))
        assertFalse(source.contains("roam.db"))
    }

    @Test fun `daily consumes canonical home config and never invents operator layout`() {
        assertTrue(source.contains("OrgHome.dailySpec(context)"))
        assertTrue(source.contains("Daily view is not configured"))
        assertTrue(source.contains("No directory, filename pattern, or timezone is guessed"))
        assertFalse(source.contains("Documents/Notes/org"))
        assertFalse(source.contains("roam/daily/{date}.org"))
        assertFalse(source.contains("ZoneId.of("))
    }
}
