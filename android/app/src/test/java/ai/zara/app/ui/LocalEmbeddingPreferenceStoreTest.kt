package ai.zara.app.ui

import ai.zara.app.prolog.LocalEmbeddingConfiguration
import java.io.File
import java.nio.file.Files
import java.util.concurrent.CountDownLatch
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test

class LocalEmbeddingPreferenceStoreTest {
    @Test
    fun `existing preference directory does not reproduce mkdirs false crash`() {
        val root = Files.createTempDirectory("zara-embedding-existing-parent").toFile()
        val directory = File(root, "zara/preferences")
        assertTrue(directory.mkdirs())
        assertFalse("File.mkdirs returns false for an existing directory", directory.mkdirs())

        val store = LocalEmbeddingPreferenceStore.forApplicationFiles(root)
        assertEquals(
            LocalEmbeddingPreferenceSaveResult.Saved,
            store.save(LocalEmbeddingConfiguration(enabled = true)),
        )
        assertTrue(store.load().configuration.enabled)
    }

    @Test
    fun `cold start defaults disabled without manufacturing a preference`() {
        val root = Files.createTempDirectory("zara-embedding-cold-start").toFile()
        val loaded = LocalEmbeddingPreferenceStore.forApplicationFiles(root).load()

        assertFalse(loaded.configuration.enabled)
        assertNull(loaded.warning)
        assertFalse(File(root, "zara/preferences/local-embedding.bin").exists())
    }

    @Test
    fun `save creates canonical directory and persists toggle across process instances`() {
        val root = Files.createTempDirectory("zara-embedding-processes").toFile()
        val appProcess = LocalEmbeddingPreferenceStore.forApplicationFiles(root)
        val voiceProcess = LocalEmbeddingPreferenceStore.forApplicationFiles(root)

        assertEquals(
            LocalEmbeddingPreferenceSaveResult.Saved,
            appProcess.save(LocalEmbeddingConfiguration(enabled = true)),
        )
        assertTrue(voiceProcess.load().configuration.enabled)
        assertTrue(File(root, "zara/preferences").isDirectory)

        assertEquals(
            LocalEmbeddingPreferenceSaveResult.Saved,
            voiceProcess.save(LocalEmbeddingConfiguration(enabled = false)),
        )
        assertFalse(LocalEmbeddingPreferenceStore.forApplicationFiles(root).load().configuration.enabled)
    }

    @Test
    fun `legacy root preference migrates once without changing enabled state`() {
        val root = Files.createTempDirectory("zara-embedding-migration").toFile()
        val legacy = File(root, "local-embedding.bin")
        legacy.writeText("enabled")

        val loaded = LocalEmbeddingPreferenceStore.forApplicationFiles(root).load()

        assertTrue(loaded.configuration.enabled)
        assertNull(loaded.warning)
        assertEquals("enabled", File(root, "zara/preferences/local-embedding.bin").readText())
        assertFalse(legacy.exists())
        assertTrue(LocalEmbeddingPreferenceStore.forApplicationFiles(root).load().configuration.enabled)
    }

    @Test
    fun `concurrent process writers use isolated temp files and leave valid state`() {
        val root = Files.createTempDirectory("zara-embedding-concurrent").toFile()
        val appProcess = LocalEmbeddingPreferenceStore.forApplicationFiles(root)
        val voiceProcess = LocalEmbeddingPreferenceStore.forApplicationFiles(root)
        val start = CountDownLatch(1)
        val pool = Executors.newFixedThreadPool(2)

        val appWrite = pool.submit {
            start.await()
            repeat(50) { index ->
                assertEquals(
                    LocalEmbeddingPreferenceSaveResult.Saved,
                    appProcess.save(LocalEmbeddingConfiguration(enabled = index % 2 == 0)),
                )
            }
        }
        val voiceWrite = pool.submit {
            start.await()
            repeat(50) { index ->
                assertEquals(
                    LocalEmbeddingPreferenceSaveResult.Saved,
                    voiceProcess.save(LocalEmbeddingConfiguration(enabled = index % 2 != 0)),
                )
            }
        }

        start.countDown()
        appWrite.get(10, TimeUnit.SECONDS)
        voiceWrite.get(10, TimeUnit.SECONDS)
        pool.shutdownNow()

        val canonical = File(root, "zara/preferences/local-embedding.bin")
        assertTrue(canonical.readText() in setOf("enabled", "disabled"))
        assertNull(LocalEmbeddingPreferenceStore.forApplicationFiles(root).load().warning)
        val leftovers = canonical.parentFile?.listFiles { file ->
            file.name.startsWith(".${canonical.name}.") && file.name.endsWith(".tmp")
        }.orEmpty()
        assertTrue("generated temp siblings must be owned and cleaned per write", leftovers.isEmpty())
    }

    @Test
    fun `storage failure is typed and the same store recovers after directory repair`() {
        val root = Files.createTempDirectory("zara-embedding-recovery").toFile()
        val zara = File(root, "zara")
        assertTrue(zara.mkdirs())
        val blockedDirectory = File(zara, "preferences")
        blockedDirectory.writeText("not-a-directory")
        val store = LocalEmbeddingPreferenceStore.forApplicationFiles(root)

        val failed = store.save(LocalEmbeddingConfiguration(enabled = true))
        assertTrue(failed is LocalEmbeddingPreferenceSaveResult.Failed)
        assertFalse(store.load().configuration.enabled)

        assertTrue(blockedDirectory.delete())
        assertEquals(
            LocalEmbeddingPreferenceSaveResult.Saved,
            store.save(LocalEmbeddingConfiguration(enabled = true)),
        )
        val recovered = store.load()
        assertTrue(recovered.configuration.enabled)
        assertNull(recovered.warning)
    }

    @Test
    fun `corrupt canonical preference degrades explicitly instead of silently disabling`() {
        val root = Files.createTempDirectory("zara-embedding-corrupt").toFile()
        val canonical = File(root, "zara/preferences/local-embedding.bin")
        assertTrue(canonical.parentFile.mkdirs())
        canonical.writeText("maybe")

        val loaded = LocalEmbeddingPreferenceStore.forApplicationFiles(root).load()

        assertFalse(loaded.configuration.enabled)
        assertNotNull(loaded.warning)
        assertTrue(loaded.warning!!.contains("invalid"))
    }
}
