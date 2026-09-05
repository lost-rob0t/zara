package ai.zara.app.device

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class OpenAppAdapterTest {
    @Test
    fun `adapter advertises only when a reviewed alias is launchable`() {
        val launcher = FakeAppLauncher(setOf("browser"))
        val adapter = OpenAppAdapter(launcher)

        assertTrue(adapter.isAvailable())

        launcher.available = emptySet()
        assertFalse(adapter.isAvailable())
    }

    @Test
    fun `reviewed alias launches without accepting package names`() {
        val launcher = FakeAppLauncher(setOf("browser", "youtube"))
        val adapter = OpenAppAdapter(launcher)

        assertEquals(
            DeviceActionResult.Completed,
            adapter.execute(DeviceActionArguments.OpenApp("YouTube")),
        )
        assertEquals(listOf("youtube"), launcher.launched)
    }

    @Test
    fun `unknown alias and raw package name fail closed`() {
        val launcher = FakeAppLauncher(setOf("browser", "youtube"))
        val adapter = OpenAppAdapter(launcher)

        assertEquals(
            DeviceActionResult.Error(DeviceActionErrorCode.InvalidArguments),
            adapter.execute(DeviceActionArguments.OpenApp("com.android.chrome")),
        )
        assertEquals(
            DeviceActionResult.Error(DeviceActionErrorCode.InvalidArguments),
            adapter.execute(DeviceActionArguments.OpenApp("settings")),
        )
        assertTrue(launcher.launched.isEmpty())
    }

    @Test
    fun `known but unavailable alias reports unavailable`() {
        val launcher = FakeAppLauncher(setOf("browser"))
        val adapter = OpenAppAdapter(launcher)

        assertEquals(
            DeviceActionResult.Error(DeviceActionErrorCode.Unavailable),
            adapter.execute(DeviceActionArguments.OpenApp("youtube")),
        )
        assertTrue(launcher.launched.isEmpty())
    }

    private class FakeAppLauncher(
        var available: Set<String>,
    ) : AppLauncher {
        val launched = mutableListOf<String>()

        override fun isAvailable(alias: String): Boolean = alias in available

        override fun launch(alias: String) {
            check(alias in available)
            launched += alias
        }
    }
}
