package ai.zara.app.prolog

import ai.zara.app.accessibility.AccessibilityAutomationAdapter
import ai.zara.app.automation.AdbAutomationKey
import ai.zara.app.automation.AdbAutomationPort
import ai.zara.app.control.AndroidControlAccess
import ai.zara.app.device.AppLauncher
import ai.zara.app.device.AppSearchAdapter
import ai.zara.app.device.AppSearchLauncher
import ai.zara.app.device.DeviceActionResult
import ai.zara.app.device.OpenAppAdapter
import ai.zara.app.device.OpenUriAdapter
import ai.zara.app.device.UriLauncher
import ai.zara.app.runtime.LocalQueryResult
import org.junit.Assert.assertEquals
import org.junit.Test
import java.util.concurrent.CompletableFuture

class AndroidAutomationRunnerTest {
    @Test
    fun `youtube demo executes typed search without special access`() {
        val search = FakeSearchLauncher(setOf("youtube"))
        val runner = runner(
            result = "actions([app_search(youtube, 'psytrance')])",
            search = search,
            accessibilityGranted = false,
        )

        val outcome = runner.run("youtube_psytrance").get()

        assertEquals(
            AndroidAutomationResult.Completed(
                AndroidAutomationPlan(
                    "youtube_psytrance",
                    listOf(AndroidAutomationAction.SearchApp("youtube", "psytrance")),
                )
            ),
            outcome,
        )
        assertEquals(listOf("youtube" to "psytrance"), search.searches)
    }

    @Test
    fun `ADB plan executes only through typed port`() {
        val adb = FakeAdbPort()
        val runner = runner(
            result = "actions([adb_tap(100,200), adb_key(home)])",
            search = FakeSearchLauncher(emptySet()),
            accessibilityGranted = false,
            adb = adb,
        )

        val outcome = runner.run("adb_demo").get()

        assertEquals(
            AndroidAutomationResult.Completed(
                AndroidAutomationPlan(
                    "adb_demo",
                    listOf(
                        AndroidAutomationAction.AdbTap(100, 200),
                        AndroidAutomationAction.AdbKey(AdbAutomationKey.Home),
                    ),
                )
            ),
            outcome,
        )
        assertEquals(listOf("tap:100:200", "key:home"), adb.actions)
    }

    @Test
    fun `accessibility action stops and requests access before side effect`() {
        val runner = runner(
            result = "actions([ui_click(text('Play'))])",
            search = FakeSearchLauncher(emptySet()),
            accessibilityGranted = false,
        )

        val outcome = runner.run("click_play").get()
        val needsAccess = outcome as AndroidAutomationResult.NeedsAccess

        assertEquals(AndroidControlAccess.Accessibility, needsAccess.access)
        assertEquals(0, needsAccess.actionIndex)
    }

    private fun runner(
        result: String,
        search: FakeSearchLauncher,
        accessibilityGranted: Boolean,
        adb: AdbAutomationPort = FakeAdbPort(),
    ): AndroidAutomationRunner = AndroidAutomationRunner(
        queryProlog = { query ->
            CompletableFuture.completedFuture(LocalQueryResult(query, listOf(result), 1))
        },
        openApp = OpenAppAdapter(FakeAppLauncher()),
        openUri = OpenUriAdapter(FakeUriLauncher()),
        appSearch = AppSearchAdapter(search),
        accessibility = AccessibilityAutomationAdapter(),
        adb = adb,
        accessGranted = { access -> access == AndroidControlAccess.Accessibility && accessibilityGranted },
    )

    private class FakeAdbPort : AdbAutomationPort {
        val actions = mutableListOf<String>()

        override fun isAvailable(): Boolean = true

        override fun tap(x: Int, y: Int): DeviceActionResult {
            actions += "tap:$x:$y"
            return DeviceActionResult.Completed
        }

        override fun swipe(
            x1: Int,
            y1: Int,
            x2: Int,
            y2: Int,
            durationMs: Int,
        ): DeviceActionResult {
            actions += "swipe:$x1:$y1:$x2:$y2:$durationMs"
            return DeviceActionResult.Completed
        }

        override fun typeText(text: String): DeviceActionResult {
            actions += "text:$text"
            return DeviceActionResult.Completed
        }

        override fun key(key: AdbAutomationKey): DeviceActionResult {
            actions += "key:${key.name.lowercase()}"
            return DeviceActionResult.Completed
        }

        override fun wait(durationMs: Int): DeviceActionResult {
            actions += "wait:$durationMs"
            return DeviceActionResult.Completed
        }

        override fun screenshotPng(): Result<ByteArray> =
            Result.success(byteArrayOf(0x89.toByte(), 0x50, 0x4E, 0x47))
    }

    private class FakeAppLauncher : AppLauncher {
        override fun isAvailable(alias: String): Boolean = true
        override fun launch(alias: String) = Unit
    }

    private class FakeUriLauncher : UriLauncher {
        override fun isAvailable(): Boolean = true
        override fun open(uri: String) = Unit
    }

    private class FakeSearchLauncher(
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
