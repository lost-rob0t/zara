package ai.zara.app.prolog

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder

class AndroidKnowledgeBaseTest {
    @get:Rule
    val temporary = TemporaryFolder()

    @Test
    fun durableFactsDedupeAcrossRestartWhileObservationsAppend() {
        val workspace = PrologWorkspace(temporary.newFolder("android-kb"))
        val first = AndroidKnowledgeBase(workspace, clockMillis = { 1_000L })
        val app = AndroidAppMemory(
            packageName = "com.example.player",
            activityName = "com.example.player.MainActivity",
            label = "Player",
            profile = "owner",
        )
        val action = AndroidActionMemory(
            id = "open_player",
            kind = "launch_app",
            backend = "launcher_apps",
            target = "com.example.player/com.example.player.MainActivity",
            source = "launcher_discovery",
        )

        first.rememberApp(app)
        first.rememberApp(app)
        first.rememberAction(action)
        first.rememberAction(action)
        first.rememberObservation("open_player", "success", "launcher_apps", "started")
        first.rememberObservation("open_player", "success", "launcher_apps", "started")

        val second = AndroidKnowledgeBase(workspace, clockMillis = { 2_000L })
        val source = second.sources().joinToString("\n") { it.text }

        assertEquals(1, source.countOccurrences("android_app(\"com.example.player\""))
        assertEquals(1, source.countOccurrences("android_action(\"open_player\""))
        assertEquals(2, source.countOccurrences("android_observation(\"open_player\""))
        assertTrue(second.modelContext().contains("com.example.player"))
        assertTrue(second.modelContext().contains("open_player"))
    }

    @Test
    fun kbRotatesBoundedShardsAndRecallUsesNewestFactsFirst() {
        val workspace = PrologWorkspace(temporary.newFolder("android-kb-shards"))
        val kb = AndroidKnowledgeBase(
            workspace = workspace,
            clockMillis = { 3_000L },
            shardByteLimit = 420,
        )

        repeat(24) { index ->
            kb.rememberEvent(
                AndroidMemoryEvent(
                    kind = "launcher_event",
                    subject = "app_$index",
                    detail = "opened application number $index with durable context",
                    provenance = "launcher",
                    sensitivity = AndroidMemorySensitivity.PRIVATE,
                )
            )
        }

        assertTrue(kb.sources().size > 1)
        assertTrue(kb.sources().all { it.text.encodeToByteArray().size <= 420 })
        val context = kb.modelContext(limit = 3)
        assertTrue(context.contains("app_23"))
        assertTrue(context.contains("app_22"))
        assertTrue(context.contains("app_21"))
        assertFalse(context.contains("app_0"))
    }

    @Test
    fun secretMemoryStoresOnlyFingerprintAndNeverLeaksIntoRecall() {
        val workspace = PrologWorkspace(temporary.newFolder("android-kb-secret"))
        val kb = AndroidKnowledgeBase(workspace, clockMillis = { 4_000L })
        val secret = "api-token-super-secret"

        kb.rememberEvent(
            AndroidMemoryEvent(
                kind = "credential_seen",
                subject = "provider",
                detail = secret,
                provenance = "local_config",
                sensitivity = AndroidMemorySensitivity.SECRET,
            )
        )

        val persisted = kb.sources().joinToString("\n") { it.text }
        assertFalse(persisted.contains(secret))
        assertTrue(persisted.contains("redacted:"))
        assertFalse(kb.modelContext().contains(secret))
    }

    @Test
    fun hooksAreDurableTypedFactsVisibleToModelRecall() {
        val workspace = PrologWorkspace(temporary.newFolder("android-kb-hooks"))
        val kb = AndroidKnowledgeBase(workspace, clockMillis = { 5_000L })

        kb.rememberHook(
            AndroidHookMemory(
                id = "spotify_open_focus",
                event = "app_opened:com.spotify.music",
                automation = "music_focus_mode",
                source = "user_config",
            )
        )

        val source = kb.sources().joinToString("\n") { it.text }
        assertTrue(source.contains("android_hook(\"spotify_open_focus\""))
        assertTrue(kb.modelContext().contains("music_focus_mode"))
    }

    private fun String.countOccurrences(needle: String): Int {
        if (needle.isEmpty()) return 0
        var count = 0
        var offset = 0
        while (true) {
            val index = indexOf(needle, offset)
            if (index < 0) return count
            count += 1
            offset = index + needle.length
        }
    }
}
