package ai.zara.app.telemetry

import ai.zara.app.auth.AuthenticationException
import ai.zara.app.runtime.StaleTextSessionException
import ai.zara.app.runtime.TextRequestTimeoutException
import ai.zara.app.runtime.ZaraWireException
import ai.zara.app.voice.VoiceStreamBackpressureException
import java.io.IOException
import org.junit.Assert.assertEquals
import org.junit.Test

class ZaraFailuresTest {
    @Test
    fun wireExceptionKeepsTypedProtocolCode() {
        val failure = ZaraFailures.classify(
            ZaraWireException("unsupported voice stream event type", code = ZaraFailureCodes.PROTOCOL_UNSUPPORTED_MESSAGE),
            ZaraOperation.STREAM,
        )
        assertEquals(ZaraSubsystem.PROTOCOL, failure.subsystem)
        assertEquals(ZaraFailureCodes.PROTOCOL_UNSUPPORTED_MESSAGE, failure.code)
        assertEquals(ZaraRecovery.RETRYABLE, failure.recovery)
    }

    @Test
    fun serverProtocolErrorCarriesServerCodeAndRetryable() {
        val failure = ZaraFailures.classify(
            ZaraWireException(
                "turn failed: turn_budget",
                code = ZaraFailureCodes.PROTOCOL_SERVER_ERROR,
                serverCode = "turn_budget",
                retryable = false,
            ),
            ZaraOperation.SUBMIT,
        )
        assertEquals(ZaraFailureCodes.PROTOCOL_SERVER_ERROR, failure.code)
        assertEquals("turn_budget", failure.serverCode)
        assertEquals(false, failure.retryable)
        assertEquals(ZaraRecovery.REQUIRES_ACTION, failure.recovery)
    }

    @Test
    fun transportFailuresAreRetryable() {
        val timeout = ZaraFailures.classify(TextRequestTimeoutException("timed out"), ZaraOperation.SUBMIT)
        assertEquals(ZaraSubsystem.TRANSPORT, timeout.subsystem)
        assertEquals(ZaraFailureCodes.TRANSPORT_TIMEOUT, timeout.code)
        assertEquals(ZaraRecovery.RETRYABLE, timeout.recovery)

        val closed = ZaraFailures.classify(IOException("socket closed"), ZaraOperation.STREAM)
        assertEquals(ZaraFailureCodes.TRANSPORT_CLOSED, closed.code)
    }

    @Test
    fun staleSessionMapsToStaleGeneration() {
        val failure = ZaraFailures.classify(
            StaleTextSessionException("text request belongs to a stale session"),
            ZaraOperation.SUBMIT,
        )
        assertEquals(ZaraFailureCodes.PROTOCOL_STALE_GENERATION, failure.code)
        assertEquals(ZaraSubsystem.PROTOCOL, failure.subsystem)
    }

    @Test
    fun authRejectionRequiresAction() {
        val failure = ZaraFailures.classify(
            AuthenticationException("enrollment missing"),
            ZaraOperation.CONNECT,
        )
        assertEquals(ZaraSubsystem.AUTH, failure.subsystem)
        assertEquals(ZaraFailureCodes.AUTH_REJECTED, failure.code)
        assertEquals(ZaraRecovery.REQUIRES_ACTION, failure.recovery)
    }

    @Test
    fun voiceOperationFailuresKeepVoiceSubsystem() {
        val failure = ZaraFailures.classify(
            VoiceStreamBackpressureException("stream queue overflow"),
            ZaraOperation.VOICE_TURN,
        )
        assertEquals(ZaraSubsystem.VOICE, failure.subsystem)
        assertEquals(ZaraFailureCodes.VOICE_REMOTE_RESPONSE, failure.code)
    }

    @Test
    fun remoteNotConnectedIsItsOwnTypedCode() {
        val failure = ZaraFailures.classify(RemoteUnavailableException(), ZaraOperation.SUBMIT)
        assertEquals(ZaraFailureCodes.REMOTE_NOT_CONNECTED, failure.code)
        assertEquals(ZaraRecovery.REQUIRES_ACTION, failure.recovery)
    }

    @Test
    fun unmappedFailuresAreExplicitlyUnknownNotOperationFailed() {
        val failure = ZaraFailures.classify(IllegalStateException("boom"), ZaraOperation.SUBMIT)
        assertEquals("unknown", failure.code)
        assertEquals(ZaraRecovery.UNKNOWN, failure.recovery)
        assertEquals("boom", failure.message)
    }

    @Test
    fun messagesAreBounded() {
        val failure = ZaraFailures.classify(
            IllegalStateException("x".repeat(10_000)),
            ZaraOperation.SUBMIT,
        )
        assert(failure.message.length <= 512)
    }
}
