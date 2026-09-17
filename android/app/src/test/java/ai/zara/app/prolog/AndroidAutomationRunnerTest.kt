package ai.zara.app.prolog

import ai.zara.app.accessibility.AccessibilityAutomationAdapter
import ai.zara.app.control.AndroidControlAccess
import ai.zara.app.device.AppLauncher
import ai.zara.app.device.AppSearchAdapter
import ai.zara.app.device.AppSearchLauncher
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
    fun `accessibility action stops and requests access before side effect`() {
        val runner = runner(
            result = "actions([ui_click(text('Play'))])",
            search = FakeSearchLauncher(emptySet()),
            accessibilityGranted = false,
        )

        val outcome = runner.run("click_play").get()

        assertEquals(
            AndroidControlAccess.Accessibility,
            (outcome as AndroidAutomationResult.NeedsAccess).access,
        )
        assertEquals(0, outcome.actionIndex)
    }

    private fun runner(
        result: String,
        search: FakeSearchLauncher,
        accessibilityGranted: Boolean,
    ): AndroidAutomationRunner = AndroidAutomationRunner(
        queryProlog = { query ->
            CompletableFuture.completedFuture(LocalQueryResult(query, listOf(result), 1))
        },
        openApp = OpenAppAdapter(FakeAppLauncher()),
        openUri = OpenUriAdapter(FakeUriLauncher()),
        appSearch = AppSearchAdapter(search),
        accessibility = AccessibilityAutomationAdapter(),
        accessGranted = { access -> access == AndroidControlAccess.Accessibility && accessibilityGranted },
    )

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
