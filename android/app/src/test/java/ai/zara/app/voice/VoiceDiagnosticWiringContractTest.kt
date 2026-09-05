package ai.zara.app.voice

import java.io.File
import org.junit.Assert.assertTrue
import org.junit.Test

class VoiceDiagnosticWiringContractTest {
    @Test
    fun `app session never exposes raw voice exception messages`() {
        val session = File("src/main/java/ai/zara/app/AndroidAppSession.kt").readText()

        assertTrue(session.contains("VoiceDiagnosticFailure.summarize(error)"))
    }
}
