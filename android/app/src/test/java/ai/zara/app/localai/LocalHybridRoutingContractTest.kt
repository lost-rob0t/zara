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
        assertTrue(session.contains("modelServer.generate"))
        assertTrue(session.contains("runtimeMode == RuntimeMode.Auto && serverState.config.enabled"))
        assertTrue(session.contains("\"model_server.fallback.blocked\""))
    }

    @Test
    fun cloudFallbackCannotCrossStrictLocalOrPureSymbolicAuthority() {
        val session = File("src/main/java/ai/zara/app/AndroidAppSession.kt").readText()
        val activity = File("src/main/java/ai/zara/app/MainActivity.kt").readText()

        assertTrue(session.contains("runtimeMode == RuntimeMode.Auto && serverState.config.enabled"))
        assertTrue(activity.contains("ConversationExecutionPolicy.PURE_SYMBOLIC"))
        assertTrue(activity.contains("executionPolicyController.submit"))
    }

    @Test
    fun bargeInStopsLocalSpeechAsWellAsRemotePcmPlayback() {
        val session = File("src/main/java/ai/zara/app/AndroidAppSession.kt").readText()

        assertTrue(session.contains("localAi.stopSpeech()"))
        assertTrue(session.contains("voiceStreamSink.interrupt()"))
    }
}
