package ai.zara.app.voice

import ai.zara.app.runtime.ZaraWireException
import java.util.concurrent.CompletionException
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Test

class VoiceDiagnosticFailureTest {
    @Test
    fun `wire failure is typed without exposing frame content`() {
        val secret = "token=private-value"
        val summary = VoiceDiagnosticFailure.summarize(ZaraWireException(secret))

        assertEquals("voice_protocol_error", summary)
        assertFalse(summary.contains(secret))
    }

    @Test
    fun `backpressure failure has stable redacted category`() {
        val summary = VoiceDiagnosticFailure.summarize(
            VoiceStreamBackpressureException("stream private-id queue overflow"),
        )

        assertEquals("voice_backpressure", summary)
    }

    @Test
    fun `nested permission failure stays actionable without raw message`() {
        val secret = "permission denied for /private/audio/path"
        val summary = VoiceDiagnosticFailure.summarize(
            CompletionException(SecurityException(secret)),
        )

        assertEquals("voice_permission_denied", summary)
        assertFalse(summary.contains(secret))
    }

    @Test
    fun `unknown runtime failure never exposes endpoint or payload`() {
        val secret = "tcp://private.example:7731 payload=hello"
        val summary = VoiceDiagnosticFailure.summarize(IllegalStateException(secret))

        assertEquals("voice_runtime_error", summary)
        assertFalse(summary.contains(secret))
    }
}
