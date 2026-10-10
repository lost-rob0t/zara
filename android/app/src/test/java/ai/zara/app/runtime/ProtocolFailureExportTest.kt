package ai.zara.app.runtime

import ai.zara.app.diagnostics.DiagnosticsSnapshot
import ai.zara.app.diagnostics.DiagnosticsV2
import ai.zara.app.telemetry.FailureIncidentTracker
import ai.zara.app.telemetry.ZaraFailures
import ai.zara.app.telemetry.ZaraOperation
import java.util.concurrent.CompletionException
import org.junit.Assert.*
import org.junit.Test

class ProtocolFailureExportTest {
    @Test fun exportRetainsFailureEvidenceAfterReconnectAndFutureReclassification() {
        val trace = ProtocolFailureTrace { 0L }
        trace.begin(7, "session-7", "request-7", "awaiting_turn_acceptance", "turn.accepted")
        trace.transmitted("turn.submit", "request-7", "session-7", frames("DO NOT EXPORT REQUEST"))
        trace.received(frames("DO NOT EXPORT RESPONSE"))
        trace.decoded(TextServerMessage.Progress("event-7", "session-7", null, "turn-7", 1, "turn.started"))
        val error = ZaraWireException("expected turn.accepted; received turn.started", code = "protocol.unexpected_message")
        attachProtocolFailureContext(error, trace.snapshot())
        var now = 1_000L
        val tracker = FailureIncidentTracker { now }
        tracker.noteSuccess("remote.connect.ready")
        tracker.record(ZaraFailures.classify(error, ZaraOperation.SUBMIT))
        now = 2_000L
        tracker.noteSuccess("remote.reconnect.ready")
        tracker.record(ZaraFailures.classify(CompletionException(error), ZaraOperation.SUBMIT))
        val incident = requireNotNull(tracker.primary())
        assertEquals(1_000L, incident.firstSeenMillis)
        assertEquals(2_000L, incident.lastSeenMillis)
        assertEquals("remote.connect.ready", incident.lastSuccess)
        val snapshot = DiagnosticsSnapshot(
            version = "test", versionCode = 1, sourceSha = "test-source", runtimeMode = "remote",
            sessionId = "session-8", sessionGeneration = 8, connectionPhase = "connected", enrollmentPhase = "ready",
            incident = incident, remoteContext = null, voiceStages = emptyList(),
            localAiPhase = "not_applicable", localAiNote = "remote-only mode", localAiGeneration = null, localAiModel = null,
            localServerPhase = "ready", localServerGeneration = 1, localServerFailure = null,
            events = emptyList(), diagnosticId = "diag-test", capturedAtMillis = 3_000,
        )
        val bundle = DiagnosticsV2.render(snapshot)
        for (fact in listOf(
            "session_generation=8", "primary_failure.connection_generation=7",
            "primary_failure.context_scope=at_failure", "primary_failure.request_id=request-7",
            "primary_failure.expected_message=turn.accepted", "primary_failure.last_rx_message_type=turn.started",
            "primary_failure.last_tx_message_type=turn.submit", "primary_failure.pending_requests=1",
        )) assertTrue(bundle.text.contains(fact))
        assertTrue(bundle.json.contains("\"failure_protocol_trace\":["))
        assertTrue(bundle.json.contains("\"expected_message\":\"turn.accepted\""))
        assertTrue(bundle.json.contains("\"message_type\":\"turn.started\""))
        assertFalse(bundle.text.contains("DO NOT EXPORT"))
        assertFalse(bundle.json.contains("DO NOT EXPORT"))
        assertEquals(bundle, DiagnosticsV2.render(snapshot))
    }

    private fun frames(body: String) = listOf("ZARA/1".encodeToByteArray(), body.encodeToByteArray())
}
