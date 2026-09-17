package ai.zara.app.launcher

import ai.zara.app.prolog.AndroidKnowledgeBase
import ai.zara.app.prolog.PrologWorkspace
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder

class LauncherCatalogTest {
    @get:Rule
    val temporary = TemporaryFolder()

    @Test
    fun refreshDedupesAndSortsAppsWhilePersistingDiscoveryToKb() {
        val workspace = PrologWorkspace(temporary.newFolder("launcher-kb"))
        val kb = AndroidKnowledgeBase(workspace, clockMillis = { 100L })
        val player = LauncherAppRecord(
            packageName = "com.example.player",
            activityName = "com.example.player.MainActivity",
            label = "Player",
            profile = "owner",
        )
        val browser = LauncherAppRecord(
            packageName = "org.example.browser",
            activityName = "org.example.browser.Home",
            label = "Browser",
            profile = "owner",
        )
        val catalog = LauncherCatalog(
            source = FakeLauncherAppSource(listOf(player, browser, player)),
            starter = FakeLauncherAppStarter(),
            knowledgeBase = kb,
        )

        val apps = catalog.refresh()

        assertEquals(listOf("Browser", "Player"), apps.map { it.label })
        val source = kb.sources().joinToString("\n") { it.text }
        assertTrue(source.contains("com.example.player"))
        assertTrue(source.contains("org.example.browser"))
    }

    @Test
    fun launchWritesActionAndSuccessObservationBackToKb() {
        val workspace = PrologWorkspace(temporary.newFolder("launcher-success"))
        val kb = AndroidKnowledgeBase(workspace, clockMillis = { 200L })
        val app = LauncherAppRecord(
            packageName = "com.example.player",
            activityName = "com.example.player.MainActivity",
            label = "Player",
            profile = "owner",
        )
        val starter = FakeLauncherAppStarter()
        val catalog = LauncherCatalog(FakeLauncherAppSource(listOf(app)), starter, kb)

        val result = catalog.launch(app)

        assertTrue(result.success)
        assertEquals(listOf(app), starter.started)
        val source = kb.sources().joinToString("\n") { it.text }
        assertTrue(source.contains("android_action(\"launch:com.example.player/com.example.player.MainActivity\""))
        assertTrue(source.contains("android_observation(\"launch:com.example.player/com.example.player.MainActivity\",\"success\""))
    }

    @Test
    fun failedLaunchIsDurablyRememberedWithoutPretendingSuccess() {
        val workspace = PrologWorkspace(temporary.newFolder("launcher-failure"))
        val kb = AndroidKnowledgeBase(workspace, clockMillis = { 300L })
        val app = LauncherAppRecord(
            packageName = "com.example.missing",
            activityName = "com.example.missing.MainActivity",
            label = "Missing",
            profile = "owner",
        )
        val catalog = LauncherCatalog(
            source = FakeLauncherAppSource(listOf(app)),
            starter = FakeLauncherAppStarter(failure = IllegalStateException("activity disappeared")),
            knowledgeBase = kb,
        )

        val result = catalog.launch(app)

        assertFalse(result.success)
        assertTrue(result.error.orEmpty().contains("activity disappeared"))
        val source = kb.sources().joinToString("\n") { it.text }
        assertTrue(source.contains("android_observation(\"launch:com.example.missing/com.example.missing.MainActivity\",\"failure\""))
        assertFalse(source.contains("android_observation(\"launch:com.example.missing/com.example.missing.MainActivity\",\"success\""))
    }

    private class FakeLauncherAppSource(
        private val apps: List<LauncherAppRecord>,
    ) : LauncherAppSource {
        override fun listLaunchableApps(): List<LauncherAppRecord> = apps
    }

    private class FakeLauncherAppStarter(
        private val failure: Throwable? = null,
    ) : LauncherAppStarter {
        val started = mutableListOf<LauncherAppRecord>()

        override fun start(app: LauncherAppRecord) {
            failure?.let { throw it }
            started += app
        }
    }
}
