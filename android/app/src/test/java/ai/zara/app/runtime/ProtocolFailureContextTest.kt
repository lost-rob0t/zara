package ai.zara.app.runtime

import ai.zara.app.telemetry.ZaraFailures
import ai.zara.app.telemetry.ZaraOperation
import java.nio.charset.MalformedInputException
import java.util.concurrent.CompletionException
import org.junit.Assert.*
import org.junit.Test

class ProtocolFailureContextTest {
    @Test fun typedWireErrorSurvivesLowLevelCauseAndFutureWrapper() {
        val error = CompletionException(ZaraWireException("invalid UTF-8", MalformedInputException(1)))
        val failure = ZaraFailures.classify(error, ZaraOperation.SUBMIT)
        assertEquals("protocol.malformed", failure.code)
        assertEquals(ZaraWireException::class.java.name, failure.causeClass)
    }

    @Test fun reclassificationRetainsFailingGenerationRequestAndActualMessage() {
        var now = 100_000_000L
        val trace = ProtocolFailureTrace { now }
        trace.begin(7, "session-7", "request-3", "awaiting_turn_acceptance", "turn.accepted")
        trace.transmitted("turn.submit", "request-3", "session-7", frames("PRIVATE INPUT"))
        trace.received(frames("PRIVATE RESPONSE"))
        trace.decoded(TextServerMessage.Progress("event-1", "session-7", "conversation-1", "turn-9", 1, "turn.started"))
        now += 25_000_000L
        val error = ZaraWireException("expected turn.accepted", code = "protocol.unexpected_message")
        attachProtocolFailureContext(error, trace.snapshot())
        val first = ZaraFailures.classify(error, ZaraOperation.SUBMIT, connectionGeneration = 7)
        trace.begin(8, null, "hello-8", "awaiting_hello", "hello.ok")
        attachProtocolFailureContext(error, trace.snapshot())
        val repeated = ZaraFailures.classify(CompletionException(error), ZaraOperation.SUBMIT, connectionGeneration = 8)
        assertEquals(first.protocolContext, repeated.protocolContext)
        assertEquals(7L, repeated.connectionGeneration)
        assertEquals("request-3", repeated.requestId)
        assertEquals("awaiting_turn_acceptance", repeated.phase)
        val context = requireNotNull(repeated.protocolContext)
        assertEquals("turn.accepted", context.expectedMessage)
        assertEquals("turn.started", context.lastRx?.messageType)
        assertEquals("turn-9", context.lastRx?.turnId)
        assertNull(context.turnId)
        assertEquals(1, context.pendingRequestCount)
        assertEquals(25L, context.phaseElapsedMillis)
        assertEquals(2, context.lastRx?.frameCount)
        assertEquals(1, error.suppressed.size)
        assertTrue(error.suppressed.single().stackTrace.isEmpty())
        assertFalse(context.toString().contains("PRIVATE"))
    }

    @Test fun snapshotIsDetachedBoundedAndResetForNextOperation() {
        val trace = ProtocolFailureTrace()
        trace.begin(1, "session-1", "request-1", "awaiting_turn_acceptance", "turn.accepted")
        repeat(20) { trace.received(frames("private payload $it")) }
        val context = requireNotNull(trace.snapshot())
        assertEquals(16, context.recentFrames.size)
        assertEquals(4L, context.droppedFrameCount)
        trace.finish()
        assertNull(trace.snapshot())
        trace.begin(2, "session-2", "request-2", "awaiting_turn_acceptance", "turn.accepted")
        assertNull(trace.snapshot()?.lastRx)
        assertEquals(0, trace.snapshot()?.recentFrames?.size)
        assertEquals(16, context.recentFrames.size)
    }

    @Test fun malformedFramesRetainOnlySizesAndDoNotInventMessageType() {
        val trace = ProtocolFailureTrace()
        trace.begin(1, "session-1", "request-1", "awaiting_turn_acceptance", "turn.accepted")
        trace.received(listOf(byteArrayOf(0, 1, 2)))
        trace.decodeFailed()
        val frame = requireNotNull(trace.snapshot()?.lastRx)
        assertEquals("undecoded", frame.messageType)
        assertEquals("decode_failed", frame.decodeStatus)
        assertEquals(1, frame.frameCount)
        assertNull(frame.envelopeBytes)
        assertEquals(3L, frame.totalBytes)
    }

    @Test fun unsafeIdentifiersAndUntrustedBodyStayOutOfEvidence() {
        val trace = ProtocolFailureTrace()
        trace.begin(1, "https://user:secret@host/", "Bearer secret", "awaiting_turn_acceptance", "turn.accepted")
        trace.received(frames("secret body"))
        trace.decoded(TextServerMessage.AssistantDelta("secret\nheader", "session-1", null, "turn-1", 2, "secret transcript"))
        val context = requireNotNull(trace.snapshot())
        assertNull(context.sessionId)
        assertNull(context.requestId)
        assertNull(context.lastRx?.messageId)
        assertFalse(context.toString().contains("secret"))
    }

    @Test fun acceptanceSeparatesPendingAckFromActiveTurn() {
        val trace = ProtocolFailureTrace()
        trace.begin(1, "session-1", "request-1", "awaiting_turn_acceptance", "turn.accepted")
        trace.accepted("session-1", "turn-1")
        val context = requireNotNull(trace.snapshot())
        assertEquals(0, context.pendingRequestCount)
        assertEquals("streaming_turn", context.phase)
        assertEquals("turn.completed", context.expectedMessage)
        assertEquals("turn-1", context.turnId)
        assertEquals("request-1", context.requestId)
    }

    private fun frames(body: String) = listOf("ZARA/1".encodeToByteArray(), body.encodeToByteArray())
}
