package ai.zara.org.core

import java.time.Duration
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Test

class OrgTimerTest {
    @Test
    fun timerTemplatesComeFromTaggedOrgTasksWithEffort() {
        val source = """
            * TODO Ramen :timer:
            :PROPERTIES:
            :Effort: 0:03
            :END:
            * TODO Normal work
            :PROPERTIES:
            :Effort: 1:00
            :END:
        """.trimIndent()

        val timers = OrgTimers.fromTasks(OrgParser.parse(source, "timers.org").tasks)

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
