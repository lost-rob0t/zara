package ai.zara.app.telemetry

import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertNull
import org.junit.Test

class ProtocolIncidentDeduplicationTest {
    @Test fun `only the next matching sparse completion is suppressed`() {
        val tracker = FailureIncidentTracker()
        val rich = failure(7)
        val sparse = rich.copy(connectionGeneration = null, requestId = null, phase = null, protocolEvidence = null)
        tracker.record(rich)
        assertNull(tracker.record(sparse))
        assertNotNull(tracker.record(sparse))
    }

    @Test fun `known newer generation is a distinct attempt with retained evidence`() {
        val tracker = FailureIncidentTracker()
        tracker.record(failure(7))
        val newer = requireNotNull(tracker.record(failure(8)))
        assertEquals(8L, newer.failure.connectionGeneration)
        assertEquals("request-8", newer.failure.requestId)
    }

    @Test fun `different operation must not be suppressed as duplicate`() {
        val tracker = FailureIncidentTracker()
        val rich = failure(7)
        tracker.record(rich)
        val other = rich.copy(
            operation = ZaraOperation.STREAM, connectionGeneration = null,
            requestId = null, phase = null, protocolEvidence = null,
        )
        assertNotNull(tracker.record(other))
    }

    @Test fun `late failure from an older generation cannot overwrite current evidence`() {
        val tracker = FailureIncidentTracker()
        val current = requireNotNull(tracker.record(failure(8)))
        assertNull(tracker.record(failure(7)))
        assertEquals(current, tracker.primary())
    }

    @Test fun `clear removes the retained incident and pending duplicate guard`() {
        val tracker = FailureIncidentTracker()
        val rich = failure(7)
        tracker.record(rich)
        tracker.clear()
        assertNull(tracker.primary())
        assertNotNull(tracker.record(rich.copy(
            connectionGeneration = null, requestId = null, phase = null, protocolEvidence = null,
        )))
    }

    private fun failure(generation: Long): ZaraFailure {
        val evidence = TextTurnProtocolTrace(
            generation, "session-$generation", "request-$generation", 5_000,
        ).snapshot()
        return ZaraFailure(
            subsystem = ZaraSubsystem.PROTOCOL, operation = ZaraOperation.SUBMIT,
            phase = evidence.phase, code = ZaraFailureCodes.PROTOCOL_UNEXPECTED_MESSAGE,
            message = "expected turn.accepted", causeClass = "ai.zara.app.runtime.ZaraWireException",
            serverCode = null, retryable = null, recovery = ZaraRecovery.RETRYABLE,
            connectionGeneration = generation, requestId = evidence.requestId,
            turnId = null, protocolEvidence = evidence,
        )
    }
}
