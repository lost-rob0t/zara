package ai.zara.app.runtime

import ai.zara.app.voice.ManualVoiceCapture
import ai.zara.app.voice.VoiceCaptureContext
import java.util.ArrayDeque
import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class ZaraTextClientVoiceTest {
    @Test fun `voice commands are serialized on the authenticated text dealer`() {
        val dealer = VoiceScriptDealer(
            listOf(
                server("{\"body\":{\"max_payload_bytes\":4194304,\"max_payload_frame_bytes\":1048576,\"max_payload_frames\":16,\"version\":1},\"id\":\"hello-ok\",\"payload_count\":0,\"reply_to\":\"hello-1\",\"session_id\":\"session-1\",\"timestamp_ns\":2,\"type\":\"hello.ok\"}"),
                server("{\"body\":{\"capabilities\":[]},\"id\":\"caps-ok\",\"payload_count\":0,\"reply_to\":\"caps-1\",\"session_id\":\"session-1\",\"timestamp_ns\":3,\"type\":\"capability.snapshot.ok\"}"),
                server("{\"id\":\"start-ok\",\"payload_count\":0,\"reply_to\":\"start-1\",\"session_id\":\"session-1\",\"stream_id\":\"mic-1\",\"timestamp_ns\":4,\"type\":\"audio.input.started\"}"),
                server("{\"id\":\"chunk-ok\",\"payload_count\":0,\"reply_to\":\"chunk-1\",\"seq\":0,\"session_id\":\"session-1\",\"stream_id\":\"mic-1\",\"timestamp_ns\":5,\"type\":\"audio.input.accepted\"}"),
                server("{\"id\":\"commit-ok\",\"payload_count\":0,\"reply_to\":\"commit-1\",\"session_id\":\"session-1\",\"stream_id\":\"mic-1\",\"timestamp_ns\":6,\"type\":\"audio.input.committed\"}"),
            )
        )
        var factoryCalls = 0
        val client = ZaraTextClientActor(
            dealerFactory = TextDealerFactory {
                factoryCalls += 1
                dealer
            },
            requestIds = sequenceOf("hello-1", "caps-1", "start-1", "chunk-1", "commit-1").iterator(),
            timestamps = sequenceOf(1L, 2L, 3L, 4L, 5L).iterator(),
        )
        val session = client.connect(ServerProfile.create("tcp://zara.example:7731"), 1).get()
        val context = VoiceCaptureContext(session.sessionId, null, "mic-1")
        val pcm = ByteArray(ManualVoiceCapture.PCM_FRAME_BYTES)

        client.startVoice(context).get()
        client.sendVoiceChunk(context, 0, pcm).get()
        client.commitVoice(context).get()

        assertEquals(1, factoryCalls)
        assertEquals(5, dealer.sent.size)
        assertEquals(true, dealer.sent[2][1].decodeToString().contains("\"type\":\"audio.input.start\""))
        assertEquals(3, dealer.sent[3].size)
        assertEquals(true, dealer.sent[3][2].contentEquals(pcm))
        assertEquals(true, dealer.sent[4][1].decodeToString().contains("\"type\":\"audio.input.commit\""))
        client.close()
    }

    @Test fun `voice acknowledgement with stale stream fails closed`() {
        val dealer = VoiceScriptDealer(
            listOf(
                server("{\"body\":{\"max_payload_bytes\":4194304,\"max_payload_frame_bytes\":1048576,\"max_payload_frames\":16,\"version\":1},\"id\":\"hello-ok\",\"payload_count\":0,\"reply_to\":\"hello-1\",\"session_id\":\"session-1\",\"timestamp_ns\":2,\"type\":\"hello.ok\"}"),
                server("{\"body\":{\"capabilities\":[]},\"id\":\"caps-ok\",\"payload_count\":0,\"reply_to\":\"caps-1\",\"session_id\":\"session-1\",\"timestamp_ns\":3,\"type\":\"capability.snapshot.ok\"}"),
                server("{\"id\":\"start-ok\",\"payload_count\":0,\"reply_to\":\"start-1\",\"session_id\":\"session-1\",\"stream_id\":\"old-mic\",\"timestamp_ns\":4,\"type\":\"audio.input.started\"}"),
            )
        )
        val client = ZaraTextClientActor(
            dealerFactory = TextDealerFactory { dealer },
            requestIds = sequenceOf("hello-1", "caps-1", "start-1").iterator(),
            timestamps = sequenceOf(1L, 2L, 3L).iterator(),
        )
        val session = client.connect(ServerProfile.create("tcp://zara.example:7731"), 1).get()

        val thrown = assertThrows(Exception::class.java) {
            client.startVoice(VoiceCaptureContext(session.sessionId, null, "mic-1")).get()
        }
        assertEquals(true, rootCause(thrown) is ZaraWireException)
        client.close()
    }

    @Test fun `voice command with stale authenticated session is rejected before send`() {
        val dealer = VoiceScriptDealer(
            listOf(
                server("{\"body\":{\"max_payload_bytes\":4194304,\"max_payload_frame_bytes\":1048576,\"max_payload_frames\":16,\"version\":1},\"id\":\"hello-ok\",\"payload_count\":0,\"reply_to\":\"hello-1\",\"session_id\":\"session-1\",\"timestamp_ns\":2,\"type\":\"hello.ok\"}"),
                server("{\"body\":{\"capabilities\":[]},\"id\":\"caps-ok\",\"payload_count\":0,\"reply_to\":\"caps-1\",\"session_id\":\"session-1\",\"timestamp_ns\":3,\"type\":\"capability.snapshot.ok\"}"),
            )
        )
        val client = ZaraTextClientActor(
            dealerFactory = TextDealerFactory { dealer },
            requestIds = sequenceOf("hello-1", "caps-1").iterator(),
            timestamps = sequenceOf(1L, 2L).iterator(),
        )
        client.connect(ServerProfile.create("tcp://zara.example:7731"), 1).get()

        val thrown = assertThrows(Exception::class.java) {
            client.startVoice(VoiceCaptureContext("old-session", null, "mic-1")).get()
        }
        assertEquals(true, rootCause(thrown) is StaleTextSessionException)
        assertEquals(2, dealer.sent.size)
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

private class VoiceScriptDealer(responses: List<List<ByteArray>>) : TextDealer {
    private val responses = ArrayDeque(responses)
    val sent = mutableListOf<List<ByteArray>>()
    private var closed = false

    override fun send(frames: List<ByteArray>) {
        check(!closed)
        sent += frames.map(ByteArray::copyOf)
    }

    override fun receive(timeoutMillis: Int): List<ByteArray>? {
        check(timeoutMillis > 0)
        return if (responses.isEmpty()) null else responses.removeFirst()
    }

    override fun close() {
        closed = true
    }
}
