package ai.zara.app.ui

import ai.zara.app.MainActivity
import ai.zara.app.prolog.LocalEmbeddingConfiguration
import android.app.ActivityManager
import android.content.ComponentName
import android.content.Context
import android.content.Intent
import androidx.test.platform.app.InstrumentationRegistry
import java.io.File
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test

class LocalEmbeddingPreferenceStoreInstrumentedTest {
    private lateinit var context: Context
    private val canonicalRelativePath = "zara/preferences/local-embedding.bin"

    @Before
    fun setUp() {
        context = InstrumentationRegistry.getInstrumentation().targetContext
        cleanPreferenceFiles()
    }

    @After
    fun tearDown() {
        cleanPreferenceFiles()
    }

    @Test
    fun appAndVoiceProcessesResolveOneApplicationOwnedPreference() {
        val packageName = context.packageName
        assertEquals(packageName, context.applicationInfo.processName)
        val mainActivity = context.packageManager.getActivityInfo(
            ComponentName(context, MainActivity::class.java),
            0,
        )
        assertEquals("$packageName:voice", mainActivity.processName)

        val appProcessStore = LocalEmbeddingPreferenceStore.create(context)
        assertEquals(
            LocalEmbeddingPreferenceSaveResult.Saved,
            appProcessStore.save(LocalEmbeddingConfiguration(enabled = true)),
        )
        assertTrue(LocalEmbeddingPreferenceStore.create(context).load().configuration.enabled)
        assertEquals("enabled", File(context.filesDir, canonicalRelativePath).readText())
    }

    @Test
    fun voiceProcessColdStartMigratesLegacyPreferenceWithoutCrash() {
        val legacy = File(context.filesDir, "local-embedding.bin")
        legacy.writeText("enabled")
        val canonical = File(context.filesDir, canonicalRelativePath)
        assertFalse(canonical.exists())

        context.startActivity(
            Intent(context, MainActivity::class.java).addFlags(Intent.FLAG_ACTIVITY_NEW_TASK),
        )

        waitUntil("voice process did not migrate the legacy embedding preference") {
            canonical.isFile && canonical.readText().trim() == "enabled"
        }
        waitUntil("voice process did not remain alive after cold start") {
            val processes = context.getSystemService(ActivityManager::class.java).runningAppProcesses
            processes?.any { it.processName == "${context.packageName}:voice" } == true
        }
        assertFalse(legacy.exists())
    }

    @Test
    fun mainThreadStorageFailureIsTypedAndRecoversAfterRepair() {
        val zaraDirectory = File(context.filesDir, "zara")
        assertTrue(zaraDirectory.isDirectory || zaraDirectory.mkdirs())
        val blockedPreferenceDirectory = File(zaraDirectory, "preferences")
        blockedPreferenceDirectory.deleteRecursively()
        blockedPreferenceDirectory.writeText("not-a-directory")
        val store = LocalEmbeddingPreferenceStore.create(context)
        var failed: LocalEmbeddingPreferenceSaveResult? = null

        InstrumentationRegistry.getInstrumentation().runOnMainSync {
            failed = store.save(LocalEmbeddingConfiguration(enabled = true))
        }

        assertTrue(failed is LocalEmbeddingPreferenceSaveResult.Failed)
        assertTrue(blockedPreferenceDirectory.delete())
        var recovered: LocalEmbeddingPreferenceSaveResult? = null
        InstrumentationRegistry.getInstrumentation().runOnMainSync {
            recovered = store.save(LocalEmbeddingConfiguration(enabled = true))
        }
        assertEquals(LocalEmbeddingPreferenceSaveResult.Saved, recovered)
        assertTrue(LocalEmbeddingPreferenceStore.create(context).load().configuration.enabled)
    }

    private fun cleanPreferenceFiles() {
        File(context.filesDir, "local-embedding.bin").delete()
        File(context.filesDir, "zara/preferences").deleteRecursively()
    }

    private fun waitUntil(message: String, predicate: () -> Boolean) {
        val deadline = System.nanoTime() + 10_000_000_000L
        while (System.nanoTime() < deadline) {
            if (runCatching(predicate).getOrDefault(false)) return
            Thread.sleep(50)
        }
        assertTrue(message, predicate())
    }
}
