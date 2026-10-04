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

    @Test fun `legal text frames during voice stream do not kill the pump`() {
        val dealer = QueueingTextDealer(
            listOf(
                helloOk("hello-ok-1", "req-1", "session-1", 2),
                capabilityAck("caps-ok-1", "req-caps-1", "session-1", 3),
                voiceAck("audio.input.started", "req-v1", "session-1", "mic-1", null),
                voiceAck("audio.input.committed", "req-v2", "session-1", "mic-1", null),
                server("{\"body\":{\"success\":true},\"conversation_id\":\"conversation-1\",\"id\":\"turn-done\",\"payload_count\":0,\"seq\":5,\"session_id\":\"session-1\",\"timestamp_ns\":20,\"turn_id\":\"turn-1\",\"type\":\"turn.completed\"}"),
                server("{\"body\":{\"text\":\"hello from voice\"},\"conversation_id\":\"conversation-1\",\"id\":\"final-1\",\"payload_count\":0,\"seq\":6,\"session_id\":\"session-1\",\"stream_id\":\"mic-1\",\"timestamp_ns\":21,\"type\":\"voice.transcript.final\"}"),
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

        val context = VoiceCaptureContext("session-1", "conversation-1", "mic-1")
        client.startVoice(context).get()
        client.commitVoice(context).get()

        val deadline = System.currentTimeMillis() + 5_000
        while (!events.contains("Transcript") && System.currentTimeMillis() < deadline) {
            Thread.sleep(20)
        }
        assertTrue(events.contains("Transcript"))
        assertTrue(failures.recorded.isEmpty())
        client.close()
    }

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


    @Test fun `stale voice lifecycle frames before a new turn acceptance are drained by correlation`() {
        val dealer = QueueingTextDealer(
            listOf(
                helloOk("hello-ok-1", "req-1", "session-1", 2),
                capabilityAck("caps-ok-1", "req-caps-1", "session-1", 3),
                server("{\"body\":{\"success\":true},\"conversation_id\":\"voice-conversation\",\"id\":\"stale-done\",\"payload_count\":0,\"seq\":9,\"session_id\":\"session-1\",\"timestamp_ns\":4,\"turn_id\":\"voice-turn\",\"type\":\"turn.completed\"}"),
                server("{\"body\":{\"text\":\"old voice answer\",\"truncated\":false},\"conversation_id\":\"voice-conversation\",\"id\":\"stale-response\",\"payload_count\":0,\"seq\":10,\"session_id\":\"session-1\",\"timestamp_ns\":5,\"turn_id\":\"voice-turn\",\"type\":\"assistant.response\"}"),
                server("{\"conversation_id\":\"conversation-2\",\"id\":\"accepted-2\",\"payload_count\":0,\"reply_to\":\"req-2\",\"session_id\":\"session-1\",\"timestamp_ns\":6,\"turn_id\":\"turn-2\",\"type\":\"turn.accepted\"}"),
                server("{\"body\":{\"text\":\"fresh answer\",\"success\":true},\"conversation_id\":\"conversation-2\",\"id\":\"assistant-done-2\",\"payload_count\":0,\"seq\":1,\"session_id\":\"session-1\",\"timestamp_ns\":7,\"turn_id\":\"turn-2\",\"type\":\"assistant.completed\"}"),
                server("{\"body\":{\"success\":true},\"conversation_id\":\"conversation-2\",\"id\":\"turn-done-2\",\"payload_count\":0,\"seq\":2,\"session_id\":\"session-1\",\"timestamp_ns\":8,\"turn_id\":\"turn-2\",\"type\":\"turn.completed\"}"),
            )
        )
        val stale = mutableListOf<String>()
        val client = ZaraTextClientActor(
            dealerFactory = TextDealerFactory { dealer },
            requestIds = sequenceOf("req-1", "req-caps-1", "req-2").iterator(),
            timestamps = sequenceOf(1L, 2L, 3L).iterator(),
        )
        client.setStaleFrameObserver { type, _ -> stale += type }
        client.connect(ServerProfile.create("tcp://zara.example:7731"), 1).get()

        val result = client.submitText(1, "session-1", null, "new turn").get()

        assertEquals("fresh answer", result.text)
        assertEquals("turn-2", result.turnId)
        assertTrue(stale.contains("TurnCompleted"))
        assertTrue(stale.contains("AssistantResponse"))
        client.close()
    }

    @Test fun `assistant completion uses the turn idle budget instead of the short command ack timeout`() {
        val dealer = QueueingTextDealer(
            listOf(
                helloOk("hello-ok-1", "req-1", "session-1", 2),
                capabilityAck("caps-ok-1", "req-caps-1", "session-1", 3),
                server("{\"conversation_id\":\"conversation-1\",\"id\":\"accepted-1\",\"payload_count\":0,\"reply_to\":\"req-2\",\"session_id\":\"session-1\",\"timestamp_ns\":4,\"turn_id\":\"turn-1\",\"type\":\"turn.accepted\"}"),
                server("{\"body\":{\"text\":\"slow answer\",\"success\":true},\"conversation_id\":\"conversation-1\",\"id\":\"assistant-done\",\"payload_count\":0,\"seq\":1,\"session_id\":\"session-1\",\"timestamp_ns\":5,\"turn_id\":\"turn-1\",\"type\":\"assistant.completed\"}"),
                server("{\"body\":{\"success\":true},\"conversation_id\":\"conversation-1\",\"id\":\"turn-done\",\"payload_count\":0,\"seq\":2,\"session_id\":\"session-1\",\"timestamp_ns\":6,\"turn_id\":\"turn-1\",\"type\":\"turn.completed\"}"),
            )
        )
        val client = ZaraTextClientActor(
            dealerFactory = TextDealerFactory { dealer },
            requestIds = sequenceOf("req-1", "req-caps-1", "req-2").iterator(),
            timestamps = sequenceOf(1L, 2L, 3L).iterator(),
            requestTimeoutMillis = 25,
            turnIdleTimeoutMillis = 250,
        )
        client.connect(ServerProfile.create("tcp://zara.example:7731"), 1).get()

        val result = client.submitText(1, "session-1", null, "slow turn").get()

        assertEquals("slow answer", result.text)
        assertEquals(listOf(250, 250), dealer.receiveTimeouts.takeLast(2))
        client.close()
    }

    @Test fun `submit failure reports the correlated request and accepted turn ids`() {
        val dealer = QueueingTextDealer(
            listOf(
                helloOk("hello-ok-1", "req-1", "session-1", 2),
                capabilityAck("caps-ok-1", "req-caps-1", "session-1", 3),
                server("{\"conversation_id\":\"conversation-1\",\"id\":\"accepted-1\",\"payload_count\":0,\"reply_to\":\"req-2\",\"session_id\":\"session-1\",\"timestamp_ns\":4,\"turn_id\":\"turn-1\",\"type\":\"turn.accepted\"}"),
                listOf("ZARA/1".encodeToByteArray(), byteArrayOf(0x7f)),
            )
        )
        val client = ZaraTextClientActor(
            dealerFactory = TextDealerFactory { dealer },
            requestIds = sequenceOf("req-1", "req-caps-1", "req-2").iterator(),
            timestamps = sequenceOf(1L, 2L, 3L).iterator(),
        )
        val failures = TypedFailureRecorder()
        client.setConnectionFailureObserver(failures::record)
        client.connect(ServerProfile.create("tcp://zara.example:7731"), 1).get()

        assertFutureFails { client.submitText(1, "session-1", null, "explode").get() }

        val failure = failures.await().last()
        assertEquals(ZaraFailureCodes.PROTOCOL_MALFORMED, failure.code)
        assertEquals("turn_response", failure.phase)
        assertEquals("req-2", failure.requestId)
        assertEquals("turn-1", failure.turnId)
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

    val receiveTimeouts = mutableListOf<Int>()

    override fun receive(timeoutMillis: Int): List<ByteArray>? {
        receiveTimeouts += timeoutMillis
        return if (responses.isEmpty()) null else responses.poll()
    }

    override fun close() {
        closed = true
    }
}
