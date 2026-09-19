package ai.zara.org.core

import java.time.Duration
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Test

class OrgTimerTest {
    @Test
    fun timerTemplatesComeFromCanonicalOpenTasksWithEffort() {
        val source = """
            #+TODO: TODO | ARCHIVED
            * TODO Ramen :timer:
            :PROPERTIES:
            :Effort: 0:03
            :END:
            * TODO Normal work
            :PROPERTIES:
            :Effort: 1:00
            :END:
            * ARCHIVED Old timer :timer:
            :PROPERTIES:
            :Effort: 0:10
            :END:
        """.trimIndent()
        val projection = OrgWorkspaceProjector.project(
            mapOf("timers/custom-workflow.org" to source),
        )

        val timers = OrgTimers.fromProjection(projection)

        assertEquals(1, timers.size)
        assertEquals("Ramen", timers.single().name)
        assertEquals(Duration.ofMinutes(3), timers.single().duration)
    }

    @Test
    fun effortParserRejectsZeroAndMalformedValues() {
        assertNull(OrgTimers.parseEffort("0:00"))
        assertNull(OrgTimers.parseEffort("3 minutes"))
        assertEquals(Duration.ofMinutes(90), OrgTimers.parseEffort("1:30"))
    }
}
