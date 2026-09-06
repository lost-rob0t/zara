package ai.zara.app.voice

import ai.zara.app.runtime.ZaraWireException
import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class ZaraVoiceStreamPayloadLimitTest {
    @Test
    fun `audio output payload has an absolute one MiB client ceiling`() {
        val maximum = ByteArray(1024 * 1024)
        val accepted = ZaraVoiceStreamCodec.decode(chunk(maximum)) as VoiceStreamEvent.AudioChunk
        assertEquals(maximum.size, accepted.pcm.size)

        assertThrows(ZaraWireException::class.java) {
            ZaraVoiceStreamCodec.decode(chunk(ByteArray((1024 * 1024) + 2)))
        }
    }

    private fun chunk(pcm: ByteArray): List<ByteArray> = listOf(
        "ZARA/1".encodeToByteArray(),
        "{\"content_type\":\"audio/pcm;codec=pcm_s16le\",\"id\":\"audio-chunk\",\"payload_count\":1,\"seq\":0,\"session_id\":\"session-1\",\"stream_id\":\"speaker-1\",\"timestamp_ns\":8,\"turn_id\":\"turn-1\",\"type\":\"audio.output.chunk\"}".encodeToByteArray(),
        pcm,
    )
}
