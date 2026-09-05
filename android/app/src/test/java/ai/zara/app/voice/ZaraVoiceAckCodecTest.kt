package ai.zara.app.voice

import ai.zara.app.runtime.ZaraWireException
import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class ZaraVoiceAckCodecTest {
    @Test fun `decodes correlated start and chunk acknowledgements`() {
        val started = ZaraVoiceAckCodec.decode(
            server("{\"conversation_id\":\"conversation-1\",\"id\":\"ack-1\",\"payload_count\":0,\"reply_to\":\"req-1\",\"session_id\":\"session-1\",\"stream_id\":\"mic-1\",\"timestamp_ns\":9,\"type\":\"audio.input.started\"}")
        )
        assertEquals(
            VoiceServerReply.Acknowledged(
                type = "audio.input.started",
                replyTo = "req-1",
                sessionId = "session-1",
                conversationId = "conversation-1",
                streamId = "mic-1",
                sequence = null,
            ),
            started,
        )

        val accepted = ZaraVoiceAckCodec.decode(
            server("{\"conversation_id\":\"conversation-1\",\"id\":\"ack-2\",\"payload_count\":0,\"reply_to\":\"req-2\",\"seq\":0,\"session_id\":\"session-1\",\"stream_id\":\"mic-1\",\"timestamp_ns\":10,\"type\":\"audio.input.accepted\"}")
        )
        assertEquals(0L, (accepted as VoiceServerReply.Acknowledged).sequence)
    }

    @Test fun `decodes protocol error without accepting it as success`() {
        val reply = ZaraVoiceAckCodec.decode(
            server("{\"body\":{\"code\":\"audio_backpressure\",\"message\":\"busy\",\"retryable\":true},\"id\":\"error-1\",\"payload_count\":0,\"reply_to\":\"req-2\",\"session_id\":\"session-1\",\"timestamp_ns\":11,\"type\":\"protocol.error\"}")
        ) as VoiceServerReply.ProtocolError
        assertEquals("audio_backpressure", reply.code)
        assertEquals(true, reply.retryable)
    }

    @Test fun `unknown fields and payload frames fail closed`() {
        assertThrows(ZaraWireException::class.java) {
            ZaraVoiceAckCodec.decode(
                server("{\"id\":\"ack-1\",\"payload_count\":0,\"reply_to\":\"req-1\",\"session_id\":\"session-1\",\"stream_id\":\"mic-1\",\"timestamp_ns\":9,\"type\":\"audio.input.started\",\"wat\":1}")
            )
        }
        assertThrows(ZaraWireException::class.java) {
            ZaraVoiceAckCodec.decode(
                listOf(
                    "ZARA/1".encodeToByteArray(),
                    "{\"id\":\"ack-1\",\"payload_count\":1,\"reply_to\":\"req-1\",\"session_id\":\"session-1\",\"stream_id\":\"mic-1\",\"timestamp_ns\":9,\"type\":\"audio.input.started\"}".encodeToByteArray(),
                    byteArrayOf(1),
                )
            )
        }
    }

    @Test fun `unsupported reply types fail closed`() {
        assertThrows(ZaraWireException::class.java) {
            ZaraVoiceAckCodec.decode(
                server("{\"id\":\"ack-1\",\"payload_count\":0,\"reply_to\":\"req-1\",\"session_id\":\"session-1\",\"stream_id\":\"mic-1\",\"timestamp_ns\":9,\"type\":\"turn.accepted\"}")
            )
        }
    }

    private fun server(json: String): List<ByteArray> =
        listOf("ZARA/1".encodeToByteArray(), json.encodeToByteArray())
}
