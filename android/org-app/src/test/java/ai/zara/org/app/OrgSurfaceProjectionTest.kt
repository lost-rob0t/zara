package ai.zara.org.app

import ai.zara.org.core.OrgDailySpec
import ai.zara.org.core.OrgWorkspaceProjector
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test
import java.time.Clock
import java.time.Instant
import java.time.ZoneId

class OrgSurfaceProjectionTest {
    @Test
    fun todoRoamAndDailyConsumeOneCanonicalCorpus() {
        val documents = linkedMapOf(
            "notes/project.org" to """
                #+TODO: NEXT | DONE
                * NEXT Ship Org UI
                :PROPERTIES:
                :ID: project
                :END:
                [[id:daily][Daily]]
            """.trimIndent(),
            "journal/2026-09-19.org" to """
                #+title: Daily
                * DONE Review
                :PROPERTIES:
                :ID: daily
                :END:
            """.trimIndent(),
        )
        val spec = OrgDailySpec(
            relativePathTemplate = "journal/{date}.org",
            datePattern = "yyyy-MM-dd",
            zoneId = ZoneId.of("America/New_York"),
        )
        val clock = Clock.fixed(Instant.parse("2026-09-19T04:30:00Z"), spec.zoneId)

        val projection = OrgWorkspaceProjector.project(documents, spec, clock)

        assertEquals(2, projection.tasks.size)
        assertEquals(setOf("project", "daily"), projection.roam.nodes.keys)
        assertEquals("journal/2026-09-19.org", projection.todayPath)
        assertEquals(listOf("journal/2026-09-19.org"), projection.dailies.map { it.path })
        assertEquals(1, projection.roam.backlinks("daily").size)
    }

    @Test
    fun missingCanonicalDailyConfigDoesNotInventLayout() {
        val projection = OrgWorkspaceProjector.project(
            mapOf("arbitrary/root/note.org" to "* TODO Keep ordinary Org canonical"),
        )

        assertNull(projection.today)
        assertNull(projection.todayPath)
        assertTrue(projection.dailies.isEmpty())
    }
}
