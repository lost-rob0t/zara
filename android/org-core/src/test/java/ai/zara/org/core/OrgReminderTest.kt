package ai.zara.org.core

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class OrgReminderTest {
    @Test
    fun parserPreservesExplicitScheduledAndDeadlineTimes() {
        val source = """
            * TODO Timed reminder
            SCHEDULED: <2026-09-19 Sat 07:45>
            DEADLINE: <2026-09-20 Sun 18:30>
        """.trimIndent()

        val task = OrgParser.parse(source, "agenda/reminders.org").tasks.single()

        assertEquals(LocalDate.of(2026, 9, 19), task.scheduled)
        assertEquals(LocalTime.of(7, 45), task.scheduledTime)
        assertEquals(LocalDate.of(2026, 9, 20), task.deadline)
        assertEquals(LocalTime.of(18, 30), task.deadlineTime)
    }

    @Test
    fun remindersUseExplicitTimeOrSharedDefaultFromCanonicalProjection() {
        val source = """
            #+TODO: NEXT | SHIPPED
            * NEXT Explicit
            SCHEDULED: <2026-09-19 Sat 07:45>
            * NEXT All day
            DEADLINE: <2026-09-20 Sun>
        """.trimIndent()
        val projection = OrgWorkspaceProjector.project(
            mapOf("agenda/custom-workflow.org" to source),
        )

        val reminders = OrgReminders.fromProjection(
            projection,
            defaultTime = LocalTime.of(9, 0),
        )

        assertEquals(LocalDateTime.of(2026, 9, 19, 7, 45), reminders[0].whenLocal)
        assertTrue(reminders[0].explicitTime)
        assertEquals(LocalDateTime.of(2026, 9, 20, 9, 0), reminders[1].whenLocal)
        assertFalse(reminders[1].explicitTime)
    }

    @Test
    fun fileLocalDoneStateDoesNotScheduleReminders() {
        val source = """
            #+TODO: NEXT | SHIPPED
            * SHIPPED Already delivered
            SCHEDULED: <2026-09-19 Sat 09:00>
        """.trimIndent()
        val projection = OrgWorkspaceProjector.project(
            mapOf("agenda/custom-workflow.org" to source),
        )

        assertTrue(OrgReminders.fromProjection(projection).isEmpty())
    }
}
