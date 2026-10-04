package ai.zara.app.localai

import java.io.File
import java.nio.file.Files
import org.junit.Assert.*
import org.junit.Test

class LocalModelPreferenceStoreTest {
    @Test
    fun selectionSurvivesRestartAndCorruptionDisablesOllama() {
        val file = File(Files.createTempDirectory("local-model").toFile(), "model.bin")
        val store = LocalModelPreferenceStore(file)
        assertEquals(LocalModelSelection(), store.load())
        val selection = LocalModelSelection(LocalModelProvider.OLLAMA, "gemma3:1b")
        store.save(selection)
        assertEquals(selection, LocalModelPreferenceStore(file).load())
        file.writeText("ollama\nhttp://remote-server\n")
        assertEquals(LocalModelSelection(), store.load())
    }

    @Test
    fun invalidModelNamesCannotBePersisted() {
        listOf("", "a\nb", "https://example.com", "a".repeat(257)).forEach { name ->
            assertThrows(IllegalArgumentException::class.java) {
                LocalModelSelection(LocalModelProvider.OLLAMA, name)
            }
        }
    }
}
