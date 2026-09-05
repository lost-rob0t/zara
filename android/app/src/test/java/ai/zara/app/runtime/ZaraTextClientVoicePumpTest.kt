package ai.zara.app.runtime

import ai.zara.app.voice.VoiceCaptureContext
import ai.zara.app.voice.VoiceStreamEvent
import java.util.ArrayDeque
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class ZaraTextClientVoicePumpTest {
    @Test fun `post commit pump drains transcript and audio events on same dealer owner`() {
        val dealer = PumpScriptDealer(
            listOf(
                server("{\"body\":{\"max_payload_bytes\":4194304,\"max_payload_frame_bytes\":1048576,\"max_payload_frames\":16,\"version\":1},\"id\":\"hello-ok\",\"payload_count\":0,\"reply_to\":\"hello-1\",\"session_id\":\"session-1\",\"timestamp_ns\":2,\"type\":\"hello.ok\"}"),
                server("{\"body\":{\"capabilities\":[]},\"id\":\"caps-ok\",\"payload_count\":0,\"reply_to\":\"caps-1\",\"session_id\":\"session-1\",\"timestamp_ns\":3,\"type\":\"capability.snapshot.ok\"}"),
                server("{\"id\":\"commit-ok\",\"payload_count\":0,\"reply_to\":\"commit-1\",\"session_id\":\"session-1\",\"stream_id\":\"mic-1\",\"timestamp_ns\":4,\"type\":\"audio.input.committed\"}"),
                server("{\"body\":{\"text\":\"hello world\"},\"conversation_id\":\"conversation-1\",\"id\":\"transcript-1\",\"payload_count\":0,\"seq\":0,\"session_id\":\"session-1\",\"stream_id\":\"mic-1\",\"timestamp_ns\":5,\"type\":\"voice.transcript.final\"}"),
                server("{\"body\":{\"channels\":1,\"codec\":\"pcm_s16le\",\"sample_rate\":24000},\"id\":\"audio-start\",\"payload_count\":0,\"session_id\":\"session-1\",\"stream_id\":\"speaker-1\",\"timestamp_ns\":6,\"turn_id\":\"turn-1\",\"type\":\"audio.output.start\"}"),
                listOf(
                    "ZARA/1".encodeToByteArray(),
                    "{\"body\":{},\"content_type\":\"audio/pcm;codec=pcm_s16le\",\"id\":\"audio-chunk\",\"payload_count\":1,\"seq\":0,\"session_id\":\"session-1\",\"stream_id\":\"speaker-1\",\"timestamp_ns\":7,\"turn_id\":\"turn-1\",\"type\":\"audio.output.chunk\"}".encodeToByteArray(),
                    byteArrayOf(1, 0, 2, 0),
                ),
                server("{\"body\":{},\"id\":\"audio-done\",\"payload_count\":0,\"session_id\":\"session-1\",\"stream_id\":\"speaker-1\",\"timestamp_ns\":8,\"turn_id\":\"turn-1\",\"type\":\"audio.output.done\"}"),
            )
        )
        val events = mutableListOf<VoiceStreamEvent>()
        val drained = CountDownLatch(4)
        val client = ZaraTextClientActor(
            dealerFactory = TextDealerFactory { dealer },
            requestIds = sequenceOf("hello-1", "caps-1", "commit-1").iterator(),
            timestamps = sequenceOf(1L, 2L, 3L).iterator(),
        )
        client.setVoiceStreamObserver { event ->
            synchronized(events) { events += event }
            drained.countDown()
        }
        val session = client.connect(ServerProfile.create("tcp://zara.example:7731"), 1).get()

        client.commitVoice(VoiceCaptureContext(session.sessionId, "conversation-1", "mic-1")).get()

        assertTrue(drained.await(1, TimeUnit.SECONDS))
        synchronized(events) {
            assertEquals(4, events.size)
            assertTrue(events[0] is VoiceStreamEvent.Transcript)
            assertTrue(events[1] is VoiceStreamEvent.AudioStarted)
            assertTrue(events[2] is VoiceStreamEvent.AudioChunk)
            assertTrue(events[3] is VoiceStreamEvent.AudioDone)
        }
        client.close()
    }

    private fun server(json: String): List<ByteArray> =
        listOf("ZARA/1".encodeToByteArray(), json.encodeToByteArray())
}

private class PumpScriptDealer(responses: List<List<ByteArray>>) : TextDealer {
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
