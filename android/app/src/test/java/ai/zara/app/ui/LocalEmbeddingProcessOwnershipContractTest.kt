package ai.zara.app.ui

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class LocalEmbeddingProcessOwnershipContractTest {
    @Test
    fun `embedding preference is application owned across app and voice processes`() {
        val manifest = File("src/main/AndroidManifest.xml").readText()
        val activity = File("src/main/java/ai/zara/app/MainActivity.kt").readText()
        val store = File("src/main/java/ai/zara/app/ui/LocalEmbeddingPreferenceStore.kt").readText()

        assertTrue(manifest.contains("android:name=\".MainActivity\""))
        assertTrue(manifest.contains("android:process=\":voice\""))
        assertTrue(manifest.contains("android:name=\".watch.WatchSetupActivity\""))
        assertTrue(activity.contains("LocalEmbeddingPreferenceStore.create(applicationContext)"))
        assertFalse(activity.contains("LocalEmbeddingPreferenceStore(File(filesDir"))
        assertTrue(store.contains("context.applicationContext ?: context"))
        assertTrue(store.contains("forApplicationFiles(applicationContext.filesDir)"))
        assertTrue(store.contains("zara/preferences/local-embedding.bin"))
        assertTrue(store.contains("local-embedding.bin"))
    }

    @Test
    fun `toggle state changes only after durable save and failures become ui errors`() {
        val activity = File("src/main/java/ai/zara/app/MainActivity.kt").readText()
        val toggle = activity.substringAfter("onSetLocalEmbeddingEnabled = { enabled ->")
            .substringBefore("onScanPairingQr")

        assertTrue(toggle.contains("val nextEmbedding = localEmbedding.copy(enabled = enabled)"))
        assertTrue(toggle.contains("embeddingPreferenceStore.save(nextEmbedding)"))
        assertTrue(toggle.contains("LocalEmbeddingPreferenceSaveResult.Saved -> localEmbedding = nextEmbedding"))
        assertTrue(toggle.contains("is LocalEmbeddingPreferenceSaveResult.Failed ->"))
        assertTrue(toggle.contains("operationError = result.message"))
        assertFalse(toggle.contains("localEmbedding = localEmbedding.copy(enabled = enabled)"))
    }
}
