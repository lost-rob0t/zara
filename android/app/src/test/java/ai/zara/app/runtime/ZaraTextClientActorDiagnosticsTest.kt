package ai.zara.app.runtime

import ai.zara.app.telemetry.ZaraFailure
import ai.zara.app.telemetry.ZaraFailures
import ai.zara.app.telemetry.ZaraOperation
import java.util.ArrayDeque
import java.util.concurrent.ExecutionException
import java.util.concurrent.TimeUnit
import org.junit.Assert.*
import org.junit.Test

class ZaraTextClientActorDiagnosticsTest {
    @Test fun realActorPreservesUnexpectedMessageAcrossObserverFutureAndReconnect() {
        val firstDealer = ScriptedDealer(handshake("session-7", "hello-7", "caps-7") + listOf(
            message("turn.started", "event-7", "session-7", "\"turn_id\":\"turn-7\",\"seq\":1,"),
        ))
        val secondDealer = ScriptedDealer(handshake("session-8", "hello-8", "caps-8"))
        val dealers = ArrayDeque(listOf(firstDealer, secondDealer))
        val actor = ZaraTextClientActor(
            dealerFactory = TextDealerFactory { dealers.removeFirst() },
            requestIds = listOf("hello-7", "caps-7", "request-7", "hello-8", "caps-8").iterator(),
        )
        val failures = mutableListOf<ZaraFailure>()
        actor.setConnectionFailureObserver(failures::add)
        try {
            actor.connect(ServerProfile.create("tcp://zara.example:7731"), 7).get(2, TimeUnit.SECONDS)
            val error = failureOf { actor.submitText(7, "session-7", null, "PRIVATE USER INPUT").get(2, TimeUnit.SECONDS) }
            assertEquals(1, failures.size)
            assertEquals("protocol.unexpected_message", failures.single().code)
            val recorded = requireNotNull(failures.single().protocolContext)
            assertEquals("turn.accepted", recorded.expectedMessage)
            assertEquals("turn.started", recorded.lastRx?.messageType)
            assertEquals("event-7", recorded.lastRx?.messageId)
            assertEquals("turn-7", recorded.lastRx?.turnId)
            assertEquals("turn.submit", recorded.lastTx?.messageType)
            assertEquals("decoded", recorded.lastRx?.decodeStatus)
            assertEquals("request-7", recorded.requestId)
            assertEquals(7L, recorded.connectionGeneration)
            assertTrue(failures.single().message.contains("received turn.started"))
            actor.connect(ServerProfile.create("tcp://zara.example:7731"), 8).get(2, TimeUnit.SECONDS)
            val reclassified = ZaraFailures.classify(error, ZaraOperation.SUBMIT)
            assertEquals(recorded, reclassified.protocolContext)
            assertEquals(7L, reclassified.connectionGeneration)
            assertFalse(recorded.toString().contains("PRIVATE USER INPUT"))
        } finally {
            actor.close()
        }
    }

    @Test fun realActorRecordsMalformedFrameSizesBeforeDecoding() {
        val dealer = ScriptedDealer(handshake("session-1", "hello-1", "caps-1") + listOf(listOf(byteArrayOf(1, 2, 3))))
        val actor = ZaraTextClientActor(
            dealerFactory = TextDealerFactory { dealer },
            requestIds = listOf("hello-1", "caps-1", "request-1").iterator(),
        )
        try {
            actor.connect(ServerProfile.create("tcp://zara.example:7731"), 1).get(2, TimeUnit.SECONDS)
            val error = failureOf { actor.submitText(1, "session-1", null, "test").get(2, TimeUnit.SECONDS) }
            val failure = ZaraFailures.classify(error, ZaraOperation.SUBMIT)
            assertEquals("protocol.malformed", failure.code)
            val frame = requireNotNull(failure.protocolContext?.lastRx)
            assertEquals(1, frame.frameCount)
            assertEquals(3L, frame.totalBytes)
            assertEquals("undecoded", frame.messageType)
            assertEquals("decode_failed", frame.decodeStatus)
            assertNull(frame.envelopeBytes)
        } finally {
            actor.close()
        }
    }

    @Test fun helloTimeoutHasItsOwnGenerationAndTransmittedMessage() {
        val actor = ZaraTextClientActor(
            dealerFactory = TextDealerFactory { ScriptedDealer(emptyList()) },
            requestIds = listOf("hello-3").iterator(),
        )
        try {
            val error = failureOf { actor.connect(ServerProfile.create("tcp://zara.example:7731"), 3).get(2, TimeUnit.SECONDS) }
            val failure = ZaraFailures.classify(error, ZaraOperation.RESTORE)
            assertEquals("transport.timeout", failure.code)
            assertEquals(3L, failure.connectionGeneration)
            assertEquals("awaiting_hello", failure.phase)
            assertEquals("hello.ok", failure.protocolContext?.expectedMessage)
            assertEquals("hello", failure.protocolContext?.lastTx?.messageType)
            assertNull(failure.protocolContext?.lastRx)
        } finally {
            actor.close()
        }
    }

    private fun handshake(session: String, hello: String, capabilities: String) = listOf(
        message("hello.ok", "hello-reply", session, "\"reply_to\":\"$hello\",",
            "{\"version\":1,\"max_payload_frames\":4,\"max_payload_frame_bytes\":1048576,\"max_payload_bytes\":4194304}"),
        message("capability.snapshot.ok", "caps-reply", session, "\"reply_to\":\"$capabilities\",", "{\"capabilities\":[]}"),
    )

    private fun message(type: String, id: String, session: String, extra: String, body: String = "{}") = listOf(
        "ZARA/1".encodeToByteArray(),
        "{\"type\":\"$type\",\"id\":\"$id\",\"session_id\":\"$session\",$extra\"timestamp_ns\":1,\"payload_count\":0,\"body\":$body}".encodeToByteArray(),
    )

    private fun failureOf(block: () -> Unit): ExecutionException {
        try { block() } catch (error: ExecutionException) { return error }
        throw AssertionError("Expected actor operation to fail")
    }

    private class ScriptedDealer(responses: List<List<ByteArray>>) : TextDealer {
        private val responses = ArrayDeque(responses)
        override fun send(frames: List<ByteArray>) = Unit
        override fun receive(timeoutMillis: Int): List<ByteArray>? = responses.pollFirst()
        override fun close() = Unit
    }
}
