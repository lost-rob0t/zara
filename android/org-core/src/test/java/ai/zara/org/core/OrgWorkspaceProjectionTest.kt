package ai.zara.org.core

import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test
import java.time.Clock
import java.time.Instant
import java.time.ZoneId

class OrgWorkspaceProjectionTest {
    @Test
    fun projectsTodoAndRoamFromSameCanonicalDocuments() {
        val documents = mapOf(
            "notes/project.org" to """
                #+title: Project
                * TODO Ship Android Org :mobile:
                :PROPERTIES:
                :ID: project-node
                :END:
                Links to [[id:daily-node][today]].
            """.trimIndent(),
            "journal/2026_09_18.org" to """
                #+title: Daily
                #+TODO: TODO STRT | DONE
                * STRT Review projection
                :PROPERTIES:
                :ID: daily-node
                :END:
            """.trimIndent(),
        )

        val projection = OrgWorkspaceProjector.project(documents)

        assertEquals(2, projection.tasks.size)
        assertEquals(listOf("STRT", "TODO"), projection.tasks.map { it.state }.sorted())
        assertEquals(setOf("daily-node", "project-node"), projection.roam.nodes.keys)
        assertEquals(1, projection.roam.backlinks("daily-node").size)
        assertTrue(projection.dailies.isEmpty())
        assertNull(projection.today)
        assertNull(projection.todayPath)
    }

    @Test
    fun openTasksRespectFileLocalDoneKeywords() {
        val projection = OrgWorkspaceProjector.project(
            mapOf(
                "custom-workflow.org" to """
                    #+TODO: NEXT BLOCKED | SHIPPED
                    * NEXT Keep working
                    * SHIPPED Already delivered
                """.trimIndent(),
            ),
        )

        assertEquals(listOf("Keep working"), projection.openTasks.map { it.title })
    }

    @Test
    fun multipleFileLocalTodoSequencesRemainCanonical() {
        val source = """
            #+TODO: TODO(t) | DONE(d)
            #+TODO: REPORT(r) BUG(b) KNOWNCAUSE(k) | FIXED(f)
            #+TODO: | CANCELED(c)
            * TODO General task
            * BUG Reproduce issue
            * FIXED Resolved issue
            * CANCELED Dropped work
        """.trimIndent()

        val document = OrgParser.parse(source, "mixed.org")
        val workflow = OrgParser.todoWorkflow(source)
        val projection = OrgWorkspaceProjector.project(mapOf("mixed.org" to source))

        assertEquals(listOf("TODO", "BUG", "FIXED", "CANCELED"), document.tasks.map { it.state })
        assertEquals(
            listOf("TODO", "DONE", "REPORT", "BUG", "KNOWNCAUSE", "FIXED", "CANCELED"),
            workflow.states,
        )
        assertEquals(setOf("DONE", "FIXED", "CANCELED"), workflow.doneStates)
        assertEquals(listOf("General task", "Reproduce issue"), projection.openTasks.map { it.title })
        assertEquals("FIXED", OrgParser.nextTodoState(source, "KNOWNCAUSE"))
        assertEquals("REPORT", OrgParser.nextTodoState(source, "FIXED"))
    }

    @Test
    fun dailyProjectionUsesExplicitArbitraryWorkspaceTemplate() {
        val spec = OrgDailySpec(
            relativePathTemplate = "knowledge/journal/{date}.org",
            datePattern = "yyyy_MM_dd",
            zoneId = ZoneId.of("America/New_York"),
        )
        val documents = linkedMapOf(
            "knowledge/journal/2026_09_20.org" to "* Existing future daily",
            "knowledge/journal/2026_09_17.org" to "* Yesterday",
            "elsewhere/2026_09_19.org" to "* Not a configured daily",
            "knowledge/journal/2026_09_18.org" to "* Today",
            "knowledge/journal/2026_09_16.org" to "* Older",
        )
        val clock = Clock.fixed(Instant.parse("2026-09-18T16:00:00Z"), spec.zoneId)

        val projection = OrgWorkspaceProjector.project(documents, spec, clock)

        assertEquals(
            listOf(
                "knowledge/journal/2026_09_18.org",
                "knowledge/journal/2026_09_17.org",
                "knowledge/journal/2026_09_16.org",
            ),
            projection.dailies.map { it.path },
        )
        assertEquals("knowledge/journal/2026_09_18.org", projection.todayPath)
        assertEquals("* Today", projection.dailies.first().source)
    }

    @Test
    fun missingTodayIsProjectedWithoutCreatingSyntheticDocument() {
        val spec = OrgDailySpec(
            relativePathTemplate = "roam/daily/{date}.org",
            datePattern = "yyyy-MM-dd",
            zoneId = ZoneId.of("UTC"),
        )
        val documents = mapOf("roam/daily/2026-09-17.org" to "* Yesterday")
        val clock = Clock.fixed(Instant.parse("2026-09-18T12:00:00Z"), spec.zoneId)

        val projection = OrgWorkspaceProjector.project(documents, spec, clock)

        assertEquals("roam/daily/2026-09-18.org", projection.todayPath)
        assertEquals(listOf("roam/daily/2026-09-17.org"), projection.dailies.map { it.path })
        assertEquals(setOf("roam/daily/2026-09-17.org"), documents.keys)
    }

    @Test(expected = IllegalArgumentException::class)
    fun dailyTemplateCannotEscapeWorkspace() {
        OrgDailySpec(
            relativePathTemplate = "../daily/{date}.org",
            datePattern = "yyyy-MM-dd",
            zoneId = ZoneId.of("UTC"),
        )
    }
}
