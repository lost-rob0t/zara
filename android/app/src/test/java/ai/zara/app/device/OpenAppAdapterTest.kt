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
    fun `wrong typed arguments fail closed`() {
        val launcher = FakeAppLauncher(setOf("browser"))
        val adapter = OpenAppAdapter(launcher)

        assertEquals(
            DeviceActionResult.Error(DeviceActionErrorCode.InvalidArguments),
            adapter.execute(DeviceActionArguments.OpenUri("https://example.com")),
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

    @Test
    fun `platform security rejection reports permission denied`() {
        val launcher = FakeAppLauncher(
            available = setOf("browser"),
            failure = SecurityException("denied"),
        )
        val adapter = OpenAppAdapter(launcher)

        assertEquals(
            DeviceActionResult.Error(DeviceActionErrorCode.PermissionDenied),
            adapter.execute(DeviceActionArguments.OpenApp("browser")),
        )
    }

    @Test
    fun `platform launch failure reports failed without optimistic completion`() {
        val launcher = FakeAppLauncher(
            available = setOf("browser"),
            failure = IllegalStateException("handler disappeared"),
        )
        val adapter = OpenAppAdapter(launcher)

        assertEquals(
            DeviceActionResult.Error(DeviceActionErrorCode.Failed),
            adapter.execute(DeviceActionArguments.OpenApp("browser")),
        )
        assertTrue(launcher.launched.isEmpty())
    }

    private class FakeAppLauncher(
        var available: Set<String>,
        private val failure: Throwable? = null,
    ) : AppLauncher {
        val launched = mutableListOf<String>()

        override fun isAvailable(alias: String): Boolean = alias in available

        override fun launch(alias: String) {
            check(alias in available)
            failure?.let { throw it }
            launched += alias
        }
    }
}
