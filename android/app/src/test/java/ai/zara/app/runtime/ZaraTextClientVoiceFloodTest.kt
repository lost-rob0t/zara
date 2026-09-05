package ai.zara.app.runtime

import ai.zara.app.voice.VoiceCaptureContext
import java.util.ArrayDeque
import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class ZaraTextClientVoiceFloodTest {
    @Test fun `voice command fails closed when stream events indefinitely displace its ack`() {
        val responses = mutableListOf<List<ByteArray>>()
        responses += server("{\"body\":{\"max_payload_bytes\":4194304,\"max_payload_frame_bytes\":1048576,\"max_payload_frames\":16,\"version\":1},\"id\":\"hello-ok\",\"payload_count\":0,\"reply_to\":\"hello-1\",\"session_id\":\"session-1\",\"timestamp_ns\":2,\"type\":\"hello.ok\"}")
        responses += server("{\"body\":{\"capabilities\":[]},\"id\":\"caps-ok\",\"payload_count\":0,\"reply_to\":\"caps-1\",\"session_id\":\"session-1\",\"timestamp_ns\":3,\"type\":\"capability.snapshot.ok\"}")
        repeat(257) { sequence ->
            responses += server("{\"body\":{\"text\":\"partial-$sequence\"},\"conversation_id\":\"conversation-1\",\"id\":\"event-$sequence\",\"payload_count\":0,\"seq\":$sequence,\"session_id\":\"session-1\",\"stream_id\":\"mic-1\",\"timestamp_ns\":${sequence + 4},\"type\":\"voice.transcript.partial\"}")
        }
        responses += server("{\"id\":\"start-ok\",\"payload_count\":0,\"reply_to\":\"start-1\",\"session_id\":\"session-1\",\"stream_id\":\"mic-1\",\"timestamp_ns\":999,\"type\":\"audio.input.started\"}")
        val dealer = FloodDealer(responses)
        var events = 0
        val client = ZaraTextClientActor(
            dealerFactory = TextDealerFactory { dealer },
            requestIds = sequenceOf("hello-1", "caps-1", "start-1").iterator(),
            timestamps = sequenceOf(1L, 2L, 3L).iterator(),
        )
        client.setVoiceStreamObserver { events += 1 }
        val session = client.connect(ServerProfile.create("tcp://zara.example:7731"), 1).get()

        val thrown = assertThrows(Exception::class.java) {
            client.startVoice(VoiceCaptureContext(session.sessionId, "conversation-1", "mic-1")).get()
        }

        assertEquals(true, rootCause(thrown) is ZaraWireException)
        assertEquals(256, events)
        client.close()
    }

    private fun server(json: String): List<ByteArray> =
        listOf("ZARA/1".encodeToByteArray(), json.encodeToByteArray())

    private fun rootCause(error: Throwable): Throwable {
        var current = error
        while (current.cause != null && current.cause !== current) current = current.cause!!
        return current
    }
}

private class FloodDealer(responses: List<List<ByteArray>>) : TextDealer {
    private val responses = ArrayDeque(responses)
    private var closed = false

    override fun send(frames: List<ByteArray>) {
        check(!closed)
    }

    override fun receive(timeoutMillis: Int): List<ByteArray>? {
        check(!closed)
        check(timeoutMillis > 0)
        return if (responses.isEmpty()) null else responses.removeFirst()
    }

    override fun close() {
        closed = true
    }
}
