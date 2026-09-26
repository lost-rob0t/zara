package ai.zara.org.core

import java.time.LocalDate
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class OrgCalendarTest {
    @Test
    fun scheduledAndDeadlineRemainDistinctOccurrences() {
        val task = OrgTask(
            path = "agenda/work.org",
            line = 12,
            level = 1,
            state = "TODO",
            title = "Ship Org calendar",
            scheduled = LocalDate.of(2026, 9, 17),
            deadline = LocalDate.of(2026, 9, 19),
            effort = "1:30",
            category = "Work",
        )

        val occurrences = OrgCalendar.occurrences(listOf(task))

        assertEquals(2, occurrences.size)
        assertEquals(OrgOccurrenceKind.SCHEDULED, occurrences[0].kind)
        assertEquals(LocalDate.of(2026, 9, 17), occurrences[0].date)
        assertEquals(OrgOccurrenceKind.DEADLINE, occurrences[1].kind)
        assertEquals(LocalDate.of(2026, 9, 19), occurrences[1].date)
        assertEquals("1:30", occurrences[0].effort)
        assertEquals("Work", occurrences[1].category)
    }

    @Test
    fun rangeProjectionIsInclusiveAndStable() {
        val tasks = listOf(
            OrgTask(
                path = "agenda/a.org",
                line = 1,
                level = 1,
                state = "TODO",
                title = "Before",
                scheduled = LocalDate.of(2026, 9, 16),
            ),
            OrgTask(
                path = "agenda/b.org",
                line = 2,
                level = 1,
                state = "TODO",
                title = "Start",
                scheduled = LocalDate.of(2026, 9, 17),
            ),
            OrgTask(
                path = "agenda/c.org",
                line = 3,
                level = 1,
                state = "TODO",
                title = "End",
                deadline = LocalDate.of(2026, 9, 18),
            ),
            OrgTask(
                path = "agenda/d.org",
                line = 4,
                level = 1,
                state = "TODO",
                title = "After",
                deadline = LocalDate.of(2026, 9, 19),
            ),
        )

        val occurrences = OrgCalendar.between(
            tasks,
            LocalDate.of(2026, 9, 17),
            LocalDate.of(2026, 9, 18),
        )

        assertEquals(listOf("Start", "End"), occurrences.map { it.title })
    }

    @Test
    fun rejectsInvertedRange() {
        val failure = runCatching {
            OrgCalendar.between(
                emptyList(),
                LocalDate.of(2026, 9, 18),
                LocalDate.of(2026, 9, 17),
            )
        }.exceptionOrNull()

        assertTrue(failure is IllegalArgumentException)
    }
}
