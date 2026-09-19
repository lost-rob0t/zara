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
    fun remindersUseExplicitTimeOrSharedDefault() {
        val tasks = listOf(
            OrgTask(
                path = "agenda/a.org",
                line = 1,
                level = 1,
                state = "TODO",
                title = "Explicit",
                scheduled = LocalDate.of(2026, 9, 19),
                scheduledTime = LocalTime.of(7, 45),
            ),
            OrgTask(
                path = "agenda/b.org",
                line = 2,
                level = 1,
                state = "TODO",
                title = "All day",
                deadline = LocalDate.of(2026, 9, 20),
            ),
        )

        val reminders = OrgReminders.fromTasks(tasks, defaultTime = LocalTime.of(9, 0))

        assertEquals(LocalDateTime.of(2026, 9, 19, 7, 45), reminders[0].whenLocal)
        assertTrue(reminders[0].explicitTime)
        assertEquals(LocalDateTime.of(2026, 9, 20, 9, 0), reminders[1].whenLocal)
        assertFalse(reminders[1].explicitTime)
    }

    @Test
    fun completedTasksDoNotScheduleReminders() {
        val task = OrgTask(
            path = "agenda/done.org",
            line = 1,
            level = 1,
            state = "DONE",
            title = "Done",
            scheduled = LocalDate.of(2026, 9, 19),
        )

        assertTrue(OrgReminders.fromTasks(listOf(task)).isEmpty())
    }
}
