package ai.zara.app.ui

import ai.zara.app.runtime.RuntimeMode
import java.io.File
import java.nio.file.Files
import org.junit.Assert.assertEquals
import org.junit.Test

class RuntimeModePreferenceStoreTest {
    @Test
    fun `fresh install defaults to local`() {
        val root = Files.createTempDirectory("zara-runtime-mode").toFile()
        val store = RuntimeModePreferenceStore(File(root, "runtime-mode.bin"))

        assertEquals(RuntimeMode.Local, store.load())
    }

    @Test
    fun `legacy remote preference migrates to local once`() {
        val root = Files.createTempDirectory("zara-runtime-mode").toFile()
        val file = File(root, "runtime-mode.bin").apply { writeText("Remote") }
        val store = RuntimeModePreferenceStore(file)

        assertEquals(RuntimeMode.Local, store.load())
    }

    @Test
    fun `versioned symbolic preference remains explicit`() {
        val root = Files.createTempDirectory("zara-runtime-mode").toFile()
        val file = File(root, "runtime-mode.bin")
        val store = RuntimeModePreferenceStore(file)

        store.save(RuntimeMode.Symbolic)

        assertEquals(RuntimeMode.Symbolic, store.load())
        assertEquals("v2:Symbolic", file.readText())
    }

    @Test
    fun `versioned remote preference remains explicit`() {
        val root = Files.createTempDirectory("zara-runtime-mode").toFile()
        val file = File(root, "runtime-mode.bin")
        val store = RuntimeModePreferenceStore(file)

        store.save(RuntimeMode.Remote)

        assertEquals(RuntimeMode.Remote, store.load())
        assertEquals("v2:Remote", file.readText())
    }
}
