package ai.zara.org.reminder

import ai.zara.org.core.OrgReminderKind
import ai.zara.org.core.OrgReminderSpec
import java.time.LocalDateTime
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class ReminderContractTest {
    @Test
    fun stableKeyChangesWhenReminderGenerationChanges() {
        val first = OrgReminderSpec(
            taskPath = "agenda/inbox.org",
            taskLine = 3,
            title = "Call",
            taskState = "TODO",
            kind = OrgReminderKind.SCHEDULED,
            whenLocal = LocalDateTime.of(2026, 9, 19, 8, 0),
            explicitTime = true,
        )
        val moved = first.copy(whenLocal = first.whenLocal.plusHours(1))

        assertTrue(first.stableKey != moved.stableKey)
        assertEquals(first.stableKey, first.copy().stableKey)
    }
}
