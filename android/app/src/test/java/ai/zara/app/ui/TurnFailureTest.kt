package ai.zara.app.ui

import ai.zara.app.telemetry.ZaraFailure
import ai.zara.app.telemetry.ZaraFailureCodes
import ai.zara.app.telemetry.ZaraOperation
import ai.zara.app.telemetry.ZaraRecovery
import ai.zara.app.telemetry.ZaraSubsystem
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class TurnFailureTest {

    @Test fun `protocol unexpected message renders one specific actionable card`() {
        val failure = TurnFailures.from(
            failure(
                code = ZaraFailureCodes.PROTOCOL_UNEXPECTED_MESSAGE,
                subsystem = ZaraSubsystem.PROTOCOL,
                operation = ZaraOperation.STREAM,
                recovery = ZaraRecovery.RETRYABLE,
            ),
            transportConnected = false,
            incidentId = "diag-abc123",
        )
        assertEquals("Remote protocol failed", failure.title)
        assertTrue(failure.explanation.isNotBlank())
        assertEquals(ZaraFailureCodes.PROTOCOL_UNEXPECTED_MESSAGE, failure.code)
        assertEquals("disconnected", failure.connectionState)
        assertTrue(failure.retryPossible)
        assertTrue(failure.reconnectPossible)
        assertEquals("diag-abc123", failure.incidentId)

        val summary = TurnFailures.renderSummary(failure)
        assertTrue(summary.contains("Code: protocol.unexpected_message"))
        assertTrue(summary.contains("Connection: disconnected"))
        assertTrue(summary.contains("Recovery: retryable"))
        assertTrue(summary.contains("Remote protocol failed"))
    }

    @Test fun `remote unavailable while disconnected is explicit reconnect required`() {
        val failure = TurnFailures.from(
            failure(
                code = ZaraFailureCodes.REMOTE_NOT_CONNECTED,
                subsystem = ZaraSubsystem.TRANSPORT,
                operation = ZaraOperation.SUBMIT,
                recovery = ZaraRecovery.REQUIRES_ACTION,
            ),
            transportConnected = false,
            incidentId = null,
        )
        assertEquals("Remote connection required", failure.title)
        assertFalse(failure.retryPossible)
        assertTrue(failure.reconnectPossible)
        assertEquals("requires_action", failure.recovery)
    }

    @Test fun `transport timeout while still connected offers retry`() {
        val failure = TurnFailures.from(
            failure(
                code = ZaraFailureCodes.TRANSPORT_TIMEOUT,
                subsystem = ZaraSubsystem.TRANSPORT,
                operation = ZaraOperation.SUBMIT,
                recovery = ZaraRecovery.RETRYABLE,
            ),
            transportConnected = true,
            incidentId = null,
        )
        assertEquals("Remote connection lost", failure.title)
        assertTrue(failure.retryPossible)
        assertEquals("connected", failure.connectionState)
    }

    @Test fun `voice failures identify the failed stage`() {
        val failure = TurnFailures.from(
            failure(
                code = ZaraFailureCodes.VOICE_TTS,
                subsystem = ZaraSubsystem.TTS,
                operation = ZaraOperation.SPEAK,
                recovery = ZaraRecovery.RETRYABLE,
            ),
            transportConnected = true,
            incidentId = null,
        )
        assertEquals("Voice playback failed", failure.title)
        assertTrue(failure.retryPossible)
    }

    @Test fun `generic unknown failures stay explicit instead of operation_failed`() {
        val failure = TurnFailures.from(
            failure(
                code = ZaraFailureCodes.UNKNOWN,
                subsystem = ZaraSubsystem.LIFECYCLE,
                operation = ZaraOperation.SUBMIT,
                recovery = ZaraRecovery.UNKNOWN,
            ),
            transportConnected = true,
            incidentId = null,
        )
        assertEquals("Request failed", failure.title)
        assertFalse(TurnFailures.renderSummary(failure).contains("operation_failed"))
    }

    @Test fun `summaries never contain secrets or raw stack traces`() {
        val failure = TurnFailures.from(
            failure(
                code = ZaraFailureCodes.TRANSPORT_CLOSED,
                subsystem = ZaraSubsystem.TRANSPORT,
                operation = ZaraOperation.STREAM,
                recovery = ZaraRecovery.RETRYABLE,
                message = "token=super-secret endpoint tcp://10.0.0.8:6060",
            ),
            transportConnected = false,
            incidentId = "diag-1",
        )
        val summary = TurnFailures.renderSummary(failure)
        assertFalse(summary.contains("super-secret"))
        assertFalse(summary.contains("10.0.0.8"))
        assertFalse(summary.contains("at ai.zara"))
    }

    @Test fun `dedupe keeps most specific failure for the same conversation`() {
        val specific = TurnFailures.from(
            failure(
                code = ZaraFailureCodes.PROTOCOL_UNEXPECTED_MESSAGE,
                subsystem = ZaraSubsystem.PROTOCOL,
                operation = ZaraOperation.STREAM,
                recovery = ZaraRecovery.RETRYABLE,
            ),
            transportConnected = false,
            incidentId = "diag-1",
        )
        val generic = TurnFailures.from(
            failure(
                code = ZaraFailureCodes.UNKNOWN,
                subsystem = ZaraSubsystem.LIFECYCLE,
                operation = ZaraOperation.SUBMIT,
                recovery = ZaraRecovery.UNKNOWN,
            ),
            transportConnected = false,
            incidentId = "diag-1",
        )
        val kept = TurnFailures.mostSpecific(existing = specific, candidate = generic)
        assertEquals(specific, kept)
        assertEquals(
            specific,
            TurnFailures.mostSpecific(existing = null, candidate = specific),
        )
    }

    private fun failure(
        code: String,
        subsystem: ZaraSubsystem,
        operation: ZaraOperation,
        recovery: ZaraRecovery,
        message: String = "injected",
    ): ZaraFailure = ZaraFailure(
        subsystem = subsystem,
        operation = operation,
        phase = null,
        code = code,
        message = message,
        causeClass = "fixture",
        serverCode = null,
        retryable = null,
        recovery = recovery,
        connectionGeneration = 1,
        requestId = null,
        turnId = null,
    )
}
