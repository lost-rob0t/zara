package ai.zara.app.telemetry

import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Test

class FailureIncidentTrackerTest {

    @Test fun `records primary failure with correlation and last good step`() {
        val tracker = FailureIncidentTracker(clock = listOf(1_000L).iterator()::next)
        tracker.noteSuccess("voice.stt.complete")

        val incident = tracker.record(
            failure(
                code = ZaraFailureCodes.PROTOCOL_MALFORMED,
                connectionGeneration = 3,
                turnId = "turn-9",
            ),
        )

        assertEquals(ZaraFailureCodes.PROTOCOL_MALFORMED, incident?.failure?.code)
        assertEquals(3L, incident?.failure?.connectionGeneration)
        assertEquals("turn-9", incident?.failure?.turnId)
        assertEquals("voice.stt.complete", incident?.lastSuccess)
        assertEquals(1_000L, incident?.firstSeenMillis)
        assertEquals(1_000L, incident?.lastSeenMillis)
        assertEquals(incident, tracker.primary())
    }

    @Test fun `newer generation failure supersedes and older generation cannot`() {
        val tracker = FailureIncidentTracker()
        tracker.record(failure(code = ZaraFailureCodes.TRANSPORT_CLOSED, connectionGeneration = 2))

        val stale = tracker.record(
            failure(code = ZaraFailureCodes.PROTOCOL_MALFORMED, connectionGeneration = 1),
        )
        assertNull(stale)
        assertEquals(ZaraFailureCodes.TRANSPORT_CLOSED, tracker.primary()?.failure?.code)

        val fresh = tracker.record(
            failure(code = ZaraFailureCodes.PROTOCOL_UNEXPECTED_MESSAGE, connectionGeneration = 4),
        )
        assertEquals(ZaraFailureCodes.PROTOCOL_UNEXPECTED_MESSAGE, fresh?.failure?.code)
        assertEquals(fresh, tracker.primary())
    }

    @Test fun `incident is retained after later successes until superseded or cleared`() {
        val tracker = FailureIncidentTracker()
        tracker.record(failure(code = ZaraFailureCodes.TRANSPORT_TIMEOUT, connectionGeneration = 1))

        tracker.noteSuccess("remote.reconnect.ready")
        tracker.noteSuccess("text.turn.complete")

        assertEquals(ZaraFailureCodes.TRANSPORT_TIMEOUT, tracker.primary()?.failure?.code)

        tracker.clear()
        assertNull(tracker.primary())
    }

    @Test fun `same failure updates last seen without losing first seen`() {
        val tracker = FailureIncidentTracker(clock = listOf(5L, 6L).iterator()::next)
        val first = tracker.record(failure(code = ZaraFailureCodes.TRANSPORT_CLOSED, connectionGeneration = 2))
        val second = tracker.record(failure(code = ZaraFailureCodes.TRANSPORT_CLOSED, connectionGeneration = 2))

        assertEquals(5L, first?.firstSeenMillis)
        assertEquals(5L, second?.firstSeenMillis)
        assertEquals(6L, second?.lastSeenMillis)
        assertEquals(second, tracker.primary())
    }

    private fun failure(
        code: String,
        connectionGeneration: Long?,
        turnId: String? = null,
    ): ZaraFailure = ZaraFailure(
        subsystem = ZaraFailures.subsystemFor(code, ZaraOperation.SUBMIT),
        operation = ZaraOperation.SUBMIT,
        phase = null,
        code = code,
        message = "injected",
        causeClass = "java.io.IOException",
        serverCode = null,
        retryable = null,
        recovery = ZaraFailures.recoveryFor(code, null),
        connectionGeneration = connectionGeneration,
        requestId = null,
        turnId = turnId,
    )
}
