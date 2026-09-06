package ai.zara.app.voice

import java.io.File
import org.junit.Assert.assertTrue
import org.junit.Test

class RecorderFailureDiagnosticWiringContractTest {
    @Test
    fun `asynchronous recorder failures reach the redacted diagnostic boundary`() {
        val session = File("src/main/java/ai/zara/app/AndroidAppSession.kt").readText()

        assertTrue(session.contains("onRecorderFailure = ::reportVoiceStreamFailure"))
    }
}
