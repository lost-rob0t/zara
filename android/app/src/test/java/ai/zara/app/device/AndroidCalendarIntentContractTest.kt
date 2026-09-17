package ai.zara.app.device

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class AndroidCalendarIntentContractTest {
    @Test
    fun `launcher uses only documented CalendarContract insert surface`() {
        val source = File("src/main/java/ai/zara/app/device/AndroidCalendarIntentLauncher.kt").readText()

        assertTrue(source.contains("Intent(Intent.ACTION_INSERT)"))
        assertTrue(source.contains("CalendarContract.Events.CONTENT_URI"))
        assertTrue(source.contains("CalendarContract.Events.TITLE"))
        assertTrue(source.contains("CalendarContract.Events.EVENT_LOCATION"))
        assertTrue(source.contains("CalendarContract.Events.DESCRIPTION"))
        assertTrue(source.contains("CalendarContract.EXTRA_EVENT_BEGIN_TIME"))
        assertTrue(source.contains("CalendarContract.EXTRA_EVENT_END_TIME"))
        assertTrue(source.contains("resolveActivity"))
        assertFalse(source.contains("setPackage("))
        assertFalse(source.contains("setComponent("))
        assertFalse(source.contains("WRITE_CALENDAR"))
    }

    @Test
    fun `calendar insert adapter is wired into the canonical device registry`() {
        val session = File("src/main/java/ai/zara/app/AndroidAppSession.kt").readText()

        assertTrue(session.contains("CalendarInsertAdapter(AndroidCalendarIntentLauncher(context))"))
        assertFalse(session.contains("CalendarInsertAdapter(AndroidCalendarIntentLauncher(context),"))
    }

    @Test
    fun `device calendar handoff does not add calendar provider permissions`() {
        val manifest = File("src/main/AndroidManifest.xml").readText()

        assertFalse(manifest.contains("android.permission.READ_CALENDAR"))
        assertFalse(manifest.contains("android.permission.WRITE_CALENDAR"))
    }
}
