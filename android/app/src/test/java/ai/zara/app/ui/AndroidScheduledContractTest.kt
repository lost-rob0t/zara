package ai.zara.app.ui

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class AndroidScheduledContractTest {
    @Test
    fun scheduledRouteUsesRealSurfaceAndCanonicalAssistantBoundary() {
        val root = File(System.getProperty("user.dir"))
        val app = File(root, "src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val surface = File(root, "src/main/java/ai/zara/app/ui/ScheduledSurface.kt").readText()

        assertTrue(app.contains("AppSurface.Scheduled -> ScheduledSurface("))
        assertTrue(surface.contains("Cron / interval"))
        assertTrue(surface.contains("@every 6h"))
        assertTrue(surface.contains("Create schedule"))
        assertTrue(surface.contains("List schedules"))
        assertTrue(surface.contains("onSendText"))

        assertTrue(surface.contains("RuntimeMode.Local"))
        assertTrue(surface.contains("ServerConnection.Connected"))
        assertTrue(surface.contains("schedulerAvailable"))
        assertTrue(surface.contains("Local mode: schedule controls are disabled"))
        assertTrue(surface.contains("Degraded: no authenticated Zara server is connected"))
        assertTrue(surface.contains("Schedules are not executed on Android"))

        assertFalse(surface.contains("WorkManager"))
        assertFalse(surface.contains("AlarmManager"))
        assertFalse(surface.contains("RoomDatabase"))
        assertFalse(surface.contains("SQLite"))
    }
}
