package ai.zara.app.runtime

import java.util.ArrayDeque
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class ZaraTextClientVoiceNegotiationTest {
    @Test fun `voice capable actor offers and records canonical server output format`() {
        val dealer = NegotiationDealer(
            listOf(
                server("{\"body\":{\"audio_output_format\":{\"channels\":1,\"codec\":\"pcm_s16le\",\"sample_rate\":24000},\"max_payload_bytes\":4194304,\"max_payload_frame_bytes\":1048576,\"max_payload_frames\":16,\"version\":1},\"id\":\"hello-ok\",\"payload_count\":0,\"reply_to\":\"hello-1\",\"session_id\":\"session-1\",\"timestamp_ns\":2,\"type\":\"hello.ok\"}"),
                server("{\"body\":{\"capabilities\":[]},\"id\":\"caps-ok\",\"payload_count\":0,\"reply_to\":\"caps-1\",\"session_id\":\"session-1\",\"timestamp_ns\":3,\"type\":\"capability.snapshot.ok\"}"),
            )
        )
        val offered = AudioOutputFormat.pcmS16leMono(24_000)
        val client = ZaraTextClientActor(
            dealerFactory = TextDealerFactory { dealer },
            requestIds = sequenceOf("hello-1", "caps-1").iterator(),
            timestamps = sequenceOf(1L, 2L).iterator(),
            audioOutputFormats = listOf(offered),
        )

        val connected = client.connect(ServerProfile.create("tcp://zara.example:7731"), 1).get()

        assertEquals("session-1", connected.sessionId)
        assertEquals(offered, client.negotiatedAudioOutputFormat())
        assertTrue(dealer.sent.first()[1].decodeToString().contains("\"audio_output_formats\":[{\"channels\":1,\"codec\":\"pcm_s16le\",\"sample_rate\":24000}]"))
        client.close()
    }

    private fun server(json: String): List<ByteArray> =
        listOf("ZARA/1".encodeToByteArray(), json.encodeToByteArray())
}

private class NegotiationDealer(responses: List<List<ByteArray>>) : TextDealer {
    private val responses = ArrayDeque(responses)
    val sent = mutableListOf<List<ByteArray>>()
    private var closed = false

    override fun send(frames: List<ByteArray>) {
        check(!closed)
        sent += frames.map(ByteArray::copyOf)
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
