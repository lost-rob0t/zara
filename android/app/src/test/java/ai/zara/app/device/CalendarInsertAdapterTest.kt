package ai.zara.app.device

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class CalendarInsertAdapterTest {
    @Test
    fun `valid event launches reviewed calendar handoff`() {
        val launcher = FakeCalendarIntentLauncher(available = true)
        val adapter = CalendarInsertAdapter(launcher)
        val arguments = DeviceActionArguments.CalendarInsert(
            title = "Planning",
            startMillis = 1_795_093_200_000,
            endMillis = 1_795_096_800_000,
            location = "Room 7",
            description = "Review roadmap",
        )

        assertEquals(DeviceActionResult.Completed, adapter.execute(arguments))
        assertEquals(listOf(arguments), launcher.inserted)
    }

    @Test
    fun `unavailable calendar handler is typed unavailable`() {
        val launcher = FakeCalendarIntentLauncher(available = false)
        val result = CalendarInsertAdapter(launcher).execute(valid())

        assertEquals(
            DeviceActionResult.Error(DeviceActionErrorCode.Unavailable),
            result,
        )
        assertTrue(launcher.inserted.isEmpty())
    }

    @Test
    fun `invalid text and time ranges fail before Android launch`() {
        val launcher = FakeCalendarIntentLauncher(available = true)
        val adapter = CalendarInsertAdapter(launcher)
        val invalid = listOf(
            valid().copy(title = "   "),
            valid().copy(title = "x\nraw"),
            valid().copy(title = "x".repeat(513)),
            valid().copy(location = "x".repeat(2_049)),
            valid().copy(description = "x\u0000y"),
            valid().copy(startMillis = -1),
            valid().copy(endMillis = valid().startMillis),
            valid().copy(endMillis = valid().startMillis - 1),
            valid().copy(endMillis = valid().startMillis + 367L * 24 * 60 * 60 * 1000),
        )

        invalid.forEach { arguments ->
            assertEquals(
                DeviceActionResult.Error(DeviceActionErrorCode.InvalidArguments),
                adapter.execute(arguments),
            )
        }
        assertTrue(launcher.inserted.isEmpty())
    }

    @Test
    fun `wrong typed arguments fail closed`() {
        val launcher = FakeCalendarIntentLauncher(available = true)
        val result = CalendarInsertAdapter(launcher).execute(
            DeviceActionArguments.OpenApp("youtube"),
        )

        assertEquals(
            DeviceActionResult.Error(DeviceActionErrorCode.InvalidArguments),
            result,
        )
        assertTrue(launcher.inserted.isEmpty())
    }

    @Test
    fun `permission and launch failures are typed`() {
        val denied = FakeCalendarIntentLauncher(available = true, failure = SecurityException("denied"))
        assertEquals(
            DeviceActionResult.Error(DeviceActionErrorCode.PermissionDenied),
            CalendarInsertAdapter(denied).execute(valid()),
        )

        val failed = FakeCalendarIntentLauncher(available = true, failure = IllegalStateException("boom"))
        assertEquals(
            DeviceActionResult.Error(DeviceActionErrorCode.Failed),
            CalendarInsertAdapter(failed).execute(valid()),
        )
    }

    private fun valid() = DeviceActionArguments.CalendarInsert(
        title = "Planning",
        startMillis = 1_795_093_200_000,
        endMillis = 1_795_096_800_000,
        location = null,
        description = null,
    )

    private class FakeCalendarIntentLauncher(
        private val available: Boolean,
        private val failure: Throwable? = null,
    ) : CalendarIntentLauncher {
        val inserted = mutableListOf<DeviceActionArguments.CalendarInsert>()

        override fun isAvailable(): Boolean = available

        override fun insert(arguments: DeviceActionArguments.CalendarInsert) {
            failure?.let { throw it }
            inserted += arguments
        }
    }
}
