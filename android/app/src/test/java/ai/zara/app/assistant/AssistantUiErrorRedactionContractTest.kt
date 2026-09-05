package ai.zara.app.assistant

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class AssistantUiErrorRedactionContractTest {
    @Test
    fun `voice interaction surface never renders raw throwable messages`() {
        val source = File("src/main/java/ai/zara/app/assistant/ZaraVoiceInteractionSession.kt").readText()

        assertTrue(source.contains("UiOperationFailure.summarize(error)"))
        assertFalse(source.contains("rootMessage(error)"))
        assertFalse(source.contains("current.message"))
    }
}
