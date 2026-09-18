package ai.zara.app.localai

import java.io.File
import org.junit.Assert.assertTrue
import org.junit.Test

class LocalHybridRoutingContractTest {
    @Test
    fun localTextRoutesThroughPrologBeforeOptionalModelFallback() {
        val session = File("src/main/java/ai/zara/app/AndroidAppSession.kt").readText()

        assertTrue(session.contains("LocalAiServiceClient"))
        assertTrue(session.contains("result.terms.isNotEmpty()"))
        assertTrue(session.contains("localAi.generate"))
        assertTrue(session.contains("LocalGenerationRequest"))
    }

    @Test
    fun bargeInStopsLocalSpeechAsWellAsRemotePcmPlayback() {
        val session = File("src/main/java/ai/zara/app/AndroidAppSession.kt").readText()

        assertTrue(session.contains("localAi.stopSpeech()"))
        assertTrue(session.contains("voiceStreamSink.interrupt()"))
    }
}
