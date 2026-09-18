package ai.zara.app.ui

import ai.zara.app.runtime.EMBEDDED_LOCAL_RUNTIME_ID
import java.nio.file.Files
import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class AssistantRuntimePreferenceStoreTest {
    @Test
    fun missingPreferenceDefaultsToEmbeddedRuntime() {
        val root = Files.createTempDirectory("zara-assistant-runtime").toFile()
        val store = AssistantRuntimePreferenceStore(root.resolve("runtime.bin"))

        assertEquals(EMBEDDED_LOCAL_RUNTIME_ID, store.load())
    }

    @Test
    fun stableRuntimeIdRoundTripsWithoutEndpointOrProcessIdentity() {
        val root = Files.createTempDirectory("zara-assistant-runtime").toFile()
        val file = root.resolve("runtime.bin")
        val store = AssistantRuntimePreferenceStore(file)

        store.save("prolog-rlm")

        assertEquals("prolog-rlm", store.load())
        assertEquals("v1:prolog-rlm", file.readText())
    }

    @Test
    fun invalidRuntimeIdsFailClosed() {
        val root = Files.createTempDirectory("zara-assistant-runtime").toFile()
        val store = AssistantRuntimePreferenceStore(root.resolve("runtime.bin"))

        assertThrows(IllegalArgumentException::class.java) {
            store.save("http://127.0.0.1:18765")
        }
    }
}
