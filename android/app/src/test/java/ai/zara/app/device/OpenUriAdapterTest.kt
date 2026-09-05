package ai.zara.app.device

import ai.zara.app.runtime.DeviceCapability
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class OpenUriAdapterTest {
    @Test
    fun `availability comes from platform launcher`() {
        assertTrue(OpenUriAdapter(FakeUriLauncher(available = true)).isAvailable())
        assertFalse(OpenUriAdapter(FakeUriLauncher(available = false)).isAvailable())
    }

    @Test
    fun `successful open uses normalized reviewed URI`() {
        val launcher = FakeUriLauncher(available = true)
        val adapter = OpenUriAdapter(launcher)

        val result = adapter.execute(DeviceActionArguments.OpenUri("https://example.com"))

        assertEquals(DeviceActionResult.Completed, result)
        assertEquals(listOf("https://example.com/"), launcher.opened)
        assertEquals(DeviceCapability.OpenUri, adapter.capability)
    }

    @Test
    fun `wrong typed arguments fail without platform invocation`() {
        val launcher = FakeUriLauncher(available = true)
        val adapter = OpenUriAdapter(launcher)

        val result = adapter.execute(DeviceActionArguments.OpenApp("browser"))

        assertEquals(DeviceActionResult.Error(DeviceActionErrorCode.InvalidArguments), result)
        assertEquals(emptyList<String>(), launcher.opened)
    }

    @Test
    fun `unavailable launcher fails without optimistic success`() {
        val launcher = FakeUriLauncher(available = false)
        val adapter = OpenUriAdapter(launcher)

        val result = adapter.execute(DeviceActionArguments.OpenUri("https://example.com"))

        assertEquals(DeviceActionResult.Error(DeviceActionErrorCode.Unavailable), result)
        assertEquals(emptyList<String>(), launcher.opened)
    }

    @Test
    fun `platform launch failure becomes typed failed result`() {
        val launcher = FakeUriLauncher(available = true, failOpen = true)
        val adapter = OpenUriAdapter(launcher)

        val result = adapter.execute(DeviceActionArguments.OpenUri("https://example.com"))

        assertEquals(DeviceActionResult.Error(DeviceActionErrorCode.Failed), result)
    }

    private class FakeUriLauncher(
        private val available: Boolean,
        private val failOpen: Boolean = false,
    ) : UriLauncher {
        val opened = mutableListOf<String>()

        override fun isAvailable(): Boolean = available

        override fun open(uri: String) {
            if (failOpen) throw IllegalStateException("launch failed")
            opened += uri
        }
    }
}
