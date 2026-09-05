package ai.zara.app.runtime

import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class ZaraVoiceNegotiationTest {
    @Test fun `voice capable hello advertises one exact android pcm format`() {
        val frames = ZaraVoiceHelloCodec.encodeHello(
            requestId = "hello-1",
            timestampNs = 1,
            audioOutputFormats = listOf(AudioOutputFormat.pcmS16leMono(24_000)),
        )
        assertEquals(
            "{\"body\":{\"audio_output_formats\":[{\"channels\":1,\"codec\":\"pcm_s16le\",\"sample_rate\":24000}],\"versions\":[1]},\"id\":\"hello-1\",\"payload_count\":0,\"timestamp_ns\":1,\"type\":\"hello\"}",
            frames[1].decodeToString(),
        )
    }

    @Test fun `hello ok records exactly negotiated android audio format`() {
        val message = ZaraVoiceHelloCodec.decodeHelloOk(
            frames(
                "{\"body\":{\"audio_output_format\":{\"channels\":1,\"codec\":\"pcm_s16le\",\"sample_rate\":24000},\"max_payload_bytes\":4194304,\"max_payload_frame_bytes\":1048576,\"max_payload_frames\":16,\"version\":1},\"id\":\"hello-ok\",\"payload_count\":0,\"reply_to\":\"hello-1\",\"session_id\":\"session-1\",\"timestamp_ns\":2,\"type\":\"hello.ok\"}"
            )
        )

        assertEquals("hello-1", message.replyTo)
        assertEquals("session-1", message.sessionId)
        assertEquals(AudioOutputFormat.pcmS16leMono(24_000), message.audioOutputFormat)
    }

    @Test fun `unsupported or malformed selected format fails closed`() {
        listOf(
            "{\"channels\":2,\"codec\":\"pcm_s16le\",\"sample_rate\":24000}",
            "{\"channels\":1,\"codec\":\"opus\",\"sample_rate\":24000}",
            "{\"channels\":1,\"codec\":\"pcm_s16le\",\"sample_rate\":0}",
            "{\"channels\":1,\"codec\":\"pcm_s16le\",\"sample_rate\":24000,\"extra\":true}",
        ).forEach { format ->
            assertThrows(ZaraWireException::class.java) {
                ZaraVoiceHelloCodec.decodeHelloOk(
                    frames(
                        "{\"body\":{\"audio_output_format\":$format,\"max_payload_bytes\":4194304,\"max_payload_frame_bytes\":1048576,\"max_payload_frames\":16,\"version\":1},\"id\":\"hello-ok\",\"payload_count\":0,\"reply_to\":\"hello-1\",\"session_id\":\"session-1\",\"timestamp_ns\":2,\"type\":\"hello.ok\"}"
                    )
                )
            }
        }
    }

    @Test fun `voice hello requires one bounded supported offer`() {
        assertThrows(IllegalArgumentException::class.java) {
            ZaraVoiceHelloCodec.encodeHello("hello-1", 1, emptyList())
        }
        assertThrows(IllegalArgumentException::class.java) {
            ZaraVoiceHelloCodec.encodeHello(
                "hello-1",
                1,
                List(9) { AudioOutputFormat.pcmS16leMono(24_000) },
            )
        }
    }

    private fun frames(json: String): List<ByteArray> =
        listOf("ZARA/1".encodeToByteArray(), json.encodeToByteArray())
}
