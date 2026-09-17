package ai.zara.app.ui

import java.io.File
import kotlin.test.Test
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class AndroidScheduledContractTest {
    @Test
    fun scheduledRouteUsesRealSurfaceAndCanonicalAssistantBoundary() {
        val root = File(System.getProperty("user.dir"))
        val app = File(root, "src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val surface = File(root, "src/main/java/ai/zara/app/ui/ScheduledSurface.kt").readText()

        assertTrue(app.contains("AppSurface.Scheduled -> ScheduledSurface("))
        assertTrue(surface.contains("Cron"))
        assertTrue(surface.contains("Create schedule"))
        assertTrue(surface.contains("List schedules"))
        assertTrue(surface.contains("onSendText"))

        assertFalse(surface.contains("WorkManager"))
        assertFalse(surface.contains("AlarmManager"))
        assertFalse(surface.contains("RoomDatabase"))
        assertFalse(surface.contains("SQLite"))
    }
}
