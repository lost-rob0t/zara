package ai.zara.app.voice

import java.io.File
import org.junit.Assert.assertTrue
import org.junit.Test

class VoiceSessionInvalidationWiringContractTest {
    @Test
    fun `canonical reducer publication invalidates stale voice capture before UI observation`() {
        val session = File("src/main/java/ai/zara/app/AndroidAppSession.kt").readText()

        assertTrue(session.contains("voice.onRuntimeStateChanged(state)"))
        assertTrue(session.indexOf("voice.onRuntimeStateChanged(state)") < session.indexOf("runtimeStateObserver?.invoke(state)"))
    }
}
