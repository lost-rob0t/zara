package ai.zara.app.voice

import org.junit.Assert.assertArrayEquals
import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class ZaraVoiceCodecTest {
    @Test
    fun startMatchesCanonicalZara1PcmGeometry() {
        val frames = ZaraVoiceCodec.encodeStart(
            requestId = "voice-start-1",
            sessionId = "session-1",
            conversationId = "conversation-1",
            streamId = "mic-1",
            timestampNs = 10,
        )

        assertEquals("ZARA/1", frames[0].decodeToString())
        assertEquals(
            "{\"body\":{\"channels\":1,\"codec\":\"pcm_s16le\",\"frame_samples\":512,\"sample_rate\":16000}," +
                "\"conversation_id\":\"conversation-1\",\"id\":\"voice-start-1\",\"payload_count\":0," +
                "\"session_id\":\"session-1\",\"stream_id\":\"mic-1\",\"timestamp_ns\":10," +
                "\"type\":\"audio.input.start\"}",
            frames[1].decodeToString(),
        )
        assertEquals(2, frames.size)
    }

    @Test
    fun chunkCarriesExactlyOneCanonicalPcmFrameAndSequence() {
        val pcm = ByteArray(ManualVoiceCapture.PCM_FRAME_BYTES) { index -> index.toByte() }
        val frames = ZaraVoiceCodec.encodeChunk(
            requestId = "voice-chunk-1",
            sessionId = "session-1",
            conversationId = null,
            streamId = "mic-1",
            sequence = 7,
            timestampNs = 11,
            pcm = pcm,
        )

        assertEquals("ZARA/1", frames[0].decodeToString())
        assertEquals(
            "{\"content_type\":\"audio/pcm;codec=pcm_s16le\",\"id\":\"voice-chunk-1\"," +
                "\"payload_count\":1,\"seq\":7,\"session_id\":\"session-1\",\"stream_id\":\"mic-1\"," +
                "\"timestamp_ns\":11,\"type\":\"audio.input.chunk\"}",
            frames[1].decodeToString(),
        )
        assertEquals(3, frames.size)
        assertArrayEquals(pcm, frames[2])
        pcm[0] = 99
        assertEquals(0, frames[2][0].toInt())
    }

    @Test
    fun terminalFramesArePayloadFreeAndKeepConversationCorrelation() {
        val commit = ZaraVoiceCodec.encodeCommit(
            requestId = "voice-commit-1",
            sessionId = "session-1",
            conversationId = "conversation-1",
            streamId = "mic-1",
            timestampNs = 12,
        )
        val cancel = ZaraVoiceCodec.encodeCancel(
            requestId = "voice-cancel-1",
            sessionId = "session-1",
            conversationId = "conversation-1",
            streamId = "mic-1",
            timestampNs = 13,
        )

        assertEquals(2, commit.size)
        assertEquals(2, cancel.size)
        assertEquals(
            "{\"conversation_id\":\"conversation-1\",\"id\":\"voice-commit-1\",\"payload_count\":0," +
                "\"session_id\":\"session-1\",\"stream_id\":\"mic-1\",\"timestamp_ns\":12," +
                "\"type\":\"audio.input.commit\"}",
            commit[1].decodeToString(),
        )
        assertEquals(
            "{\"conversation_id\":\"conversation-1\",\"id\":\"voice-cancel-1\",\"payload_count\":0," +
                "\"session_id\":\"session-1\",\"stream_id\":\"mic-1\",\"timestamp_ns\":13," +
                "\"type\":\"audio.input.cancel\"}",
            cancel[1].decodeToString(),
        )
    }

    @Test
    fun invalidGeometryInputsFailBeforeWireEmission() {
        assertThrows(IllegalArgumentException::class.java) {
            ZaraVoiceCodec.encodeChunk(
                requestId = "chunk",
                sessionId = "session",
                conversationId = null,
                streamId = "stream",
                sequence = 0,
                timestampNs = 0,
                pcm = ByteArray(ManualVoiceCapture.PCM_FRAME_BYTES - 2),
            )
        }
        assertThrows(IllegalArgumentException::class.java) {
            ZaraVoiceCodec.encodeChunk(
                requestId = "chunk",
                sessionId = "session",
                conversationId = null,
                streamId = "stream",
                sequence = -1,
                timestampNs = 0,
                pcm = ByteArray(ManualVoiceCapture.PCM_FRAME_BYTES),
            )
        }
    }
}
