package ai.zara.app.runtime

import ai.zara.app.telemetry.ZaraFailure
import ai.zara.app.telemetry.ZaraFailureCodes
import ai.zara.app.voice.VoiceCaptureContext
import java.util.ArrayDeque
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class ZaraTextClientActorFailureReportTest {

    @Test fun `voice pump death emits typed connection failure`() {
        val dealer = QueueingTextDealer(
            listOf(
                helloOk("hello-ok-1", "req-1", "session-1", 2),
                capabilityAck("caps-ok-1", "req-caps-1", "session-1", 3),
                voiceAck("audio.input.started", "req-v1", "session-1", "mic-1", null),
                voiceAck("audio.input.committed", "req-v2", "session-1", "mic-1", null),
                listOf("ZARA/1".encodeToByteArray(), byteArrayOf(0x7f)),
            )
        )
        val client = ZaraTextClientActor(
            dealerFactory = TextDealerFactory { dealer },
            requestIds = sequenceOf("req-1", "req-caps-1", "req-v1", "req-v2").iterator(),
            timestamps = sequenceOf(1L, 2L, 3L, 4L).iterator(),
        )
        val failures = TypedFailureRecorder()
        client.setConnectionFailureObserver(failures::record)
        client.connect(ServerProfile.create("tcp://zara.example:7731"), 1).get()

        val context = VoiceCaptureContext("session-1", null, "mic-1")
        client.startVoice(context).get()
        client.commitVoice(context).get()

        val reported = failures.await()
        assertEquals(1, reported.size)
        assertEquals(ZaraFailureCodes.PROTOCOL_MALFORMED, reported[0].code)
        client.close()
    }

    @Test fun `speech marker interleaved with voice acknowledgement succeeds`() {
        val dealer = QueueingTextDealer(
            listOf(
                helloOk("hello-ok-1", "req-1", "session-1", 2),
                capabilityAck("caps-ok-1", "req-caps-1", "session-1", 3),
                voiceAck("audio.input.started", "req-v1", "session-1", "mic-1", null),
                server("{\"body\":{\"pre_speech_samples\":0},\"id\":\"speech-1\",\"payload_count\":0,\"session_id\":\"session-1\",\"stream_id\":\"mic-1\",\"timestamp_ns\":10,\"type\":\"voice.speech.started\"}"),
                voiceAck("audio.input.accepted", "req-v2", "session-1", "mic-1", 0),
            )
        )
        val client = ZaraTextClientActor(
            dealerFactory = TextDealerFactory { dealer },
            requestIds = sequenceOf("req-1", "req-caps-1", "req-v1", "req-v2").iterator(),
            timestamps = sequenceOf(1L, 2L, 3L, 4L).iterator(),
        )
        val failures = TypedFailureRecorder()
        client.setConnectionFailureObserver(failures::record)
        val events = mutableListOf<String>()
        client.setVoiceStreamObserver { events += it.javaClass.simpleName }
        client.connect(ServerProfile.create("tcp://zara.example:7731"), 1).get()

        val context = VoiceCaptureContext("session-1", null, "mic-1")
        client.startVoice(context).get()
        client.sendVoiceChunk(context, 0, ByteArray(1024)).get()

        assertTrue(events.contains("SpeechStarted"))
        assertEquals(0, failures.recorded.size)
        client.close()
    }

    @Test fun `server protocol error carries typed code and retryability`() {
        val dealer = QueueingTextDealer(
            listOf(
                helloOk("hello-ok-1", "req-1", "session-1", 2),
                capabilityAck("caps-ok-1", "req-caps-1", "session-1", 3),
                server("{\"body\":{\"code\":\"turn_budget\",\"message\":\"budget exhausted\",\"retryable\":false},\"id\":\"err-1\",\"payload_count\":0,\"reply_to\":\"req-2\",\"session_id\":\"session-1\",\"timestamp_ns\":4,\"type\":\"protocol.error\"}"),
            )
        )
        val client = ZaraTextClientActor(
            dealerFactory = TextDealerFactory { dealer },
            requestIds = sequenceOf("req-1", "req-caps-1", "req-2").iterator(),
            timestamps = sequenceOf(1L, 2L, 9L).iterator(),
        )
        client.connect(ServerProfile.create("tcp://zara.example:7731"), 1).get()

        val error = assertFutureFails {
            client.submitText(1, "session-1", null, "hello").get()
        }
        val wire = rootCause(error) as ZaraWireException
        assertEquals(ZaraFailureCodes.PROTOCOL_SERVER_ERROR, wire.code)
        assertEquals("turn_budget", wire.serverCode)
        assertEquals(false, wire.retryable)
        client.close()
    }

    @Test fun `fatal runtime error during turn reports connection failure`() {
        val dealer = QueueingTextDealer(
            listOf(
                helloOk("hello-ok-1", "req-1", "session-1", 2),
                capabilityAck("caps-ok-1", "req-caps-1", "session-1", 3),
                server("{\"conversation_id\":\"conversation-1\",\"id\":\"accepted-1\",\"payload_count\":0,\"reply_to\":\"req-2\",\"session_id\":\"session-1\",\"timestamp_ns\":4,\"turn_id\":\"turn-1\",\"type\":\"turn.accepted\"}"),
                server("{\"body\":{\"fatal\":true,\"reason\":\"died\"},\"id\":\"rerr-1\",\"payload_count\":0,\"session_id\":\"session-1\",\"timestamp_ns\":5,\"type\":\"runtime.error\"}"),
            )
        )
        val client = ZaraTextClientActor(
            dealerFactory = TextDealerFactory { dealer },
            requestIds = sequenceOf("req-1", "req-caps-1", "req-2").iterator(),
            timestamps = sequenceOf(1L, 2L, 9L).iterator(),
        )
        val failures = TypedFailureRecorder()
        client.setConnectionFailureObserver(failures::record)
        client.connect(ServerProfile.create("tcp://zara.example:7731"), 1).get()

        assertFutureFails { client.submitText(1, "session-1", null, "hello").get() }

        val reported = failures.await()
        assertTrue(reported.any { it.code == ZaraFailureCodes.PROTOCOL_RUNTIME_ERROR })
        client.close()
    }

    private fun assertFutureFails(block: () -> Unit): Throwable {
        try {
            block()
        } catch (error: Throwable) {
            return error
        }
        throw AssertionError("expected the future to fail")
    }

    private fun rootCause(error: Throwable): Throwable {
        var current = error
        while (current.cause != null) current = current.cause!!
        return current
    }

    private fun helloOk(id: String, replyTo: String, sessionId: String, timestamp: Long): List<ByteArray> =
        server(
            "{\"body\":{\"max_payload_bytes\":4194304,\"max_payload_frame_bytes\":1048576," +
                "\"max_payload_frames\":16,\"version\":1},\"id\":\"$id\",\"payload_count\":0," +
                "\"reply_to\":\"$replyTo\",\"session_id\":\"$sessionId\",\"timestamp_ns\":$timestamp," +
                "\"type\":\"hello.ok\"}"
        )

    private fun capabilityAck(id: String, replyTo: String, sessionId: String, timestamp: Long): List<ByteArray> =
        server(
            "{\"body\":{\"capabilities\":[]},\"id\":\"$id\",\"payload_count\":0," +
                "\"reply_to\":\"$replyTo\",\"session_id\":\"$sessionId\",\"timestamp_ns\":$timestamp," +
                "\"type\":\"capability.snapshot.ok\"}"
        )

    private fun voiceAck(type: String, replyTo: String, sessionId: String, streamId: String, sequence: Long?): List<ByteArray> {
        val seqField = sequence?.let { ",\"seq\":$it" } ?: ""
        return server(
            "{\"id\":\"$type-1\",\"payload_count\":0,\"reply_to\":\"$replyTo\"$seqField," +
                "\"session_id\":\"$sessionId\",\"stream_id\":\"$streamId\",\"timestamp_ns\":9," +
                "\"type\":\"$type\"}"
        )
    }

    private fun server(json: String): List<ByteArray> =
        listOf("ZARA/1".encodeToByteArray(), json.encodeToByteArray())
}

private class TypedFailureRecorder {
    val recorded = mutableListOf<ZaraFailure>()
    private var latch = CountDownLatch(1)

    fun record(failure: ZaraFailure) {
        synchronized(recorded) { recorded += failure }
        latch.countDown()
    }

    fun await(): List<ZaraFailure> {
        check(latch.await(5, TimeUnit.SECONDS)) { "timed out waiting for a typed connection failure" }
        Thread.sleep(50)
        return synchronized(recorded) { recorded.toList() }
    }
}

private class QueueingTextDealer(responses: List<List<ByteArray>>) : TextDealer {
    private val responses = ArrayDeque(responses)
    val sent = mutableListOf<List<ByteArray>>()
    var closed = false

    override fun send(frames: List<ByteArray>) {
        check(!closed)
        sent += frames.map(ByteArray::copyOf)
    }

    override fun receive(timeoutMillis: Int): List<ByteArray>? =
        if (responses.isEmpty()) null else responses.poll()

    override fun close() {
        closed = true
    }
}
