package ai.zara.app.voice

import ai.zara.app.runtime.ZaraWireException
import org.junit.Assert.assertArrayEquals
import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class ZaraVoiceStreamCodecTest {
    @Test fun `partial and final transcript preserve session conversation stream and sequence`() {
        val partial = ZaraVoiceStreamCodec.decode(
            server("{\"body\":{\"text\":\"hello wor\"},\"conversation_id\":\"conversation-1\",\"id\":\"partial-1\",\"payload_count\":0,\"seq\":3,\"session_id\":\"session-1\",\"stream_id\":\"mic-1\",\"timestamp_ns\":5,\"trace_id\":\"trace-1\",\"type\":\"voice.transcript.partial\"}")
        )
        assertEquals(
            VoiceStreamEvent.Transcript(
                sessionId = "session-1",
                conversationId = "conversation-1",
                streamId = "mic-1",
                sequence = 3,
                text = "hello wor",
                final = false,
            ),
            partial,
        )

        val final = ZaraVoiceStreamCodec.decode(
            server("{\"body\":{\"text\":\"hello world\"},\"conversation_id\":\"conversation-1\",\"id\":\"final-1\",\"payload_count\":0,\"seq\":4,\"session_id\":\"session-1\",\"stream_id\":\"mic-1\",\"timestamp_ns\":6,\"trace_id\":\"trace-1\",\"type\":\"voice.transcript.final\"}")
        ) as VoiceStreamEvent.Transcript
        assertEquals(true, final.final)
        assertEquals("hello world", final.text)
    }

    @Test fun `audio output start chunk done retain binary payload and strict correlation`() {
        val start = ZaraVoiceStreamCodec.decode(
            server("{\"body\":{\"channels\":1,\"codec\":\"pcm_s16le\",\"sample_rate\":24000},\"id\":\"audio-start\",\"payload_count\":0,\"session_id\":\"session-1\",\"stream_id\":\"speaker-1\",\"timestamp_ns\":7,\"turn_id\":\"turn-1\",\"type\":\"audio.output.start\"}")
        )
        assertEquals(
            VoiceStreamEvent.AudioStarted("session-1", "turn-1", "speaker-1", 24_000, 1),
            start,
        )

        val pcm = byteArrayOf(1, 0, 2, 0)
        val chunk = ZaraVoiceStreamCodec.decode(
            listOf(
                "ZARA/1".encodeToByteArray(),
                "{\"content_type\":\"audio/pcm;codec=pcm_s16le\",\"id\":\"audio-chunk\",\"payload_count\":1,\"seq\":0,\"session_id\":\"session-1\",\"stream_id\":\"speaker-1\",\"timestamp_ns\":8,\"turn_id\":\"turn-1\",\"type\":\"audio.output.chunk\"}".encodeToByteArray(),
                pcm,
            )
        ) as VoiceStreamEvent.AudioChunk
        assertEquals(0L, chunk.sequence)
        assertArrayEquals(pcm, chunk.pcm)

        assertEquals(
            VoiceStreamEvent.AudioDone("session-1", "turn-1", "speaker-1"),
            ZaraVoiceStreamCodec.decode(
                server("{\"id\":\"audio-done\",\"payload_count\":0,\"session_id\":\"session-1\",\"stream_id\":\"speaker-1\",\"timestamp_ns\":9,\"turn_id\":\"turn-1\",\"type\":\"audio.output.done\"}")
            ),
        )
    }

    @Test fun `audio payload and transcript shapes fail closed`() {
        assertThrows(ZaraWireException::class.java) {
            ZaraVoiceStreamCodec.decode(
                listOf(
                    "ZARA/1".encodeToByteArray(),
                    "{\"content_type\":\"audio/pcm;codec=pcm_s16le\",\"id\":\"audio-chunk\",\"payload_count\":1,\"seq\":0,\"session_id\":\"session-1\",\"stream_id\":\"speaker-1\",\"timestamp_ns\":8,\"turn_id\":\"turn-1\",\"type\":\"audio.output.chunk\"}".encodeToByteArray(),
                    byteArrayOf(1),
                )
            )
        }
        assertThrows(ZaraWireException::class.java) {
            ZaraVoiceStreamCodec.decode(
                server("{\"body\":{\"text\":\"hello\",\"extra\":true},\"conversation_id\":\"conversation-1\",\"id\":\"partial-1\",\"payload_count\":0,\"seq\":3,\"session_id\":\"session-1\",\"stream_id\":\"mic-1\",\"timestamp_ns\":5,\"type\":\"voice.transcript.partial\"}")
            )
        }
        assertThrows(ZaraWireException::class.java) {
            ZaraVoiceStreamCodec.decode(
                server("{\"body\":{\"text\":\"hello\"},\"id\":\"partial-1\",\"payload_count\":0,\"seq\":3,\"session_id\":\"session-1\",\"stream_id\":\"mic-1\",\"timestamp_ns\":5,\"type\":\"voice.transcript.partial\"}")
            )
        }
    }

    private fun server(json: String): List<ByteArray> =
        listOf("ZARA/1".encodeToByteArray(), json.encodeToByteArray())
}
