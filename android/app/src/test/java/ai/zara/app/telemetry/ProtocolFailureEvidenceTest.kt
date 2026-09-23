package ai.zara.app.telemetry

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test

class ProtocolFailureEvidenceTest {
    @Test fun `awaiting acceptance retains the original request and generation`() {
        val trace = TextTurnProtocolTrace(7, "session-7", "request-7", 5_000)
        val evidence = trace.snapshot()
        assertEquals(7L, evidence.connectionGeneration)
        assertEquals("session-7", evidence.sessionId)
        assertEquals("request-7", evidence.requestId)
        assertEquals("awaiting_turn_accepted", evidence.phase)
        assertEquals("turn.accepted", evidence.expectedMessageType)
        assertEquals(1, evidence.pendingRequests)
        assertEquals(5_000, evidence.requestTimeoutMillis)
        assertNull(evidence.lastRx)
    }

    @Test fun `unexpected decoded reply retains metadata without message content`() {
        var now = 1_000_000L
        val trace = TextTurnProtocolTrace(7, "session-7", "request-7", 5_000, nanoClock = { now })
        trace.transmitAttempt(180, 2)
        trace.receive(240, 2)
        trace.decoded("assistant.delta", "event-1", null, "session-7", "turn-1", 4)
        now += 2_000_000L
        val evidence = trace.snapshot()
        assertEquals("assistant.delta", evidence.lastRx?.messageType)
        assertEquals("event-1", evidence.lastRx?.messageId)
        assertEquals("turn-1", evidence.lastRx?.turnId)
        assertEquals(4L, evidence.lastRx?.sequence)
        assertEquals(240L, evidence.lastRx?.bytes)
        assertEquals("decoded", evidence.lastRx?.state)
        assertEquals("turn.submit", evidence.lastTx?.messageType)
        assertEquals("attempted", evidence.lastTx?.state)
        assertEquals(180L, evidence.lastTx?.bytes)
        assertEquals(2L, evidence.elapsedMillis)
        assertEquals("turn.accepted", evidence.expectedMessageType)
    }

    @Test fun `malformed frame retains shape without inventing a message type`() {
        val trace = TextTurnProtocolTrace(7, "session-7", "request-7", 5_000)
        trace.receive(17, 3)
        val evidence = trace.snapshot()
        assertNull(evidence.lastRx?.messageType)
        assertEquals("undecoded", evidence.lastRx?.state)
        assertEquals(3, evidence.lastRx?.frameCount)
        assertEquals(17L, evidence.lastRx?.bytes)
    }

    @Test fun `accepted turn changes expectation only after caller validates receipt`() {
        val trace = TextTurnProtocolTrace(7, "session-7", "request-7", 5_000)
        trace.receive(200, 2)
        trace.decoded("turn.accepted", "receipt-1", "wrong-request", "session-7", "turn-1", null)
        assertEquals(1, trace.snapshot().pendingRequests)
        assertEquals("turn.accepted", trace.snapshot().expectedMessageType)
        trace.accepted("turn-1")
        assertEquals(0, trace.snapshot().pendingRequests)
        assertEquals("awaiting_turn_completion", trace.snapshot().phase)
        assertEquals("turn.completed|assistant.response", trace.snapshot().expectedMessageType)
        assertEquals("turn-1", trace.snapshot().turnId)
    }

    @Test fun `failure snapshot cannot be changed by a later receive or receipt`() {
        val trace = TextTurnProtocolTrace(7, "session-7", "request-7", 5_000)
        trace.receive(200, 2)
        trace.decoded("assistant.delta", "event-1", null, "session-7", "turn-1", 1)
        val failure = trace.snapshot()
        trace.receive(300, 2)
        trace.decoded("turn.accepted", "receipt-1", "request-7", "session-7", "turn-1", null)
        trace.accepted("turn-1")
        assertEquals("assistant.delta", failure.lastRx?.messageType)
        assertEquals(1, failure.messages.size)
        assertEquals("turn.accepted", failure.expectedMessageType)
        assertNull(failure.turnId)
    }

    @Test fun `history is bounded and original transmit survives ring eviction`() {
        val trace = TextTurnProtocolTrace(7, "session-7", "request-7", 5_000)
        trace.transmitAttempt(100, 2)
        repeat(100) { index ->
            trace.receive(200, 2)
            trace.decoded("assistant.delta", "event-$index", null, "session-7", "turn-1", index.toLong())
        }
        val evidence = trace.snapshot()
        assertEquals(32, evidence.messages.size)
        assertEquals(69L, evidence.droppedMessages)
        assertEquals("turn.submit", evidence.lastTx?.messageType)
        assertEquals("event-99", evidence.lastRx?.messageId)
    }

    @Test fun `metadata is bounded and cannot inject additional diagnostic lines`() {
        val trace = TextTurnProtocolTrace(7, "session-7", "request-7", 5_000)
        trace.receive(200, 2)
        trace.decoded("assistant.delta", "x".repeat(1_000), "bad\nreply", "session-7", null, null)
        val reply = requireNotNull(trace.snapshot().lastRx)
        assertEquals(128, reply.messageId?.length)
        assertFalse(requireNotNull(reply.replyTo).contains('\n'))
        assertTrue(requireNotNull(reply.replyTo).length <= 128)
    }
}
