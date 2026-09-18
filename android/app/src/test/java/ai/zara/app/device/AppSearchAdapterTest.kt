package ai.zara.app.device

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class AppSearchAdapterTest {
    @Test
    fun `youtube and revanced searches remain reviewed aliases`() {
        val launcher = FakeAppSearchLauncher(setOf("youtube", "youtube_revanced"))
        val adapter = AppSearchAdapter(launcher)

        assertEquals(
            DeviceActionResult.Completed,
            adapter.execute(DeviceActionArguments.AppSearch("youtube", "psytrance")),
        )
        assertEquals(
            DeviceActionResult.Completed,
            adapter.execute(DeviceActionArguments.AppSearch("YouTube_ReVanced", "psytrance")),
        )
        assertEquals(
            listOf("youtube" to "psytrance", "youtube_revanced" to "psytrance"),
            launcher.searches,
        )
    }

    @Test
    fun `adapter rejects wrong typed capability arguments`() {
        val launcher = FakeAppSearchLauncher(setOf("youtube"))
        val adapter: DeviceCapabilityAdapter = AppSearchAdapter(launcher)

        assertEquals(DeviceCapability.AppSearch, adapter.capability)
        assertEquals(
            DeviceActionResult.Error(DeviceActionErrorCode.InvalidArguments),
            adapter.execute(DeviceActionArguments.OpenApp("youtube")),
        )
        assertTrue(launcher.searches.isEmpty())
    }

    @Test
    fun `raw package names and unknown aliases fail closed`() {
        val launcher = FakeAppSearchLauncher(setOf("youtube", "youtube_revanced"))
        val adapter = AppSearchAdapter(launcher)

        assertEquals(
            DeviceActionResult.Error(DeviceActionErrorCode.InvalidArguments),
            adapter.execute(DeviceActionArguments.AppSearch("app.revanced.android.youtube", "psytrance")),
        )
        assertEquals(
            DeviceActionResult.Error(DeviceActionErrorCode.InvalidArguments),
            adapter.execute(DeviceActionArguments.AppSearch("browser", "psytrance")),
        )
        assertTrue(launcher.searches.isEmpty())
    }

    @Test
    fun `unavailable alias returns typed unavailable`() {
        val adapter = AppSearchAdapter(FakeAppSearchLauncher(setOf("youtube")))

        assertEquals(
            DeviceActionResult.Error(DeviceActionErrorCode.Unavailable),
            adapter.execute(DeviceActionArguments.AppSearch("youtube_revanced", "psytrance")),
        )
    }

    private class FakeAppSearchLauncher(
        private val available: Set<String>,
    ) : AppSearchLauncher {
        val searches = mutableListOf<Pair<String, String>>()

        override fun isAvailable(alias: String): Boolean = alias in available

        override fun search(alias: String, query: String) {
            check(alias in available)
            searches += alias to query
        }
    }
}
