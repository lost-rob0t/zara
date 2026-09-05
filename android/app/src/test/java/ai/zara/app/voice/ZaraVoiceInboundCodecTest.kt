package ai.zara.app.voice

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class ZaraVoiceInboundCodecTest {
    @Test fun `classifies canonical transcript event without consuming it as an acknowledgement`() {
        val frames = listOf(
            "ZARA/1".encodeToByteArray(),
            "{\"body\":{\"text\":\"hello\"},\"conversation_id\":\"conversation-1\",\"id\":\"event-1\",\"payload_count\":0,\"seq\":4,\"session_id\":\"session-1\",\"stream_id\":\"mic-1\",\"timestamp_ns\":1,\"type\":\"voice.transcript.partial\"}".encodeToByteArray(),
        )

        val decoded = ZaraVoiceInboundCodec.decode(frames)

        assertTrue(decoded is VoiceInboundMessage.Stream)
        val transcript = (decoded as VoiceInboundMessage.Stream).event as VoiceStreamEvent.Transcript
        assertEquals("hello", transcript.text)
        assertEquals(4L, transcript.sequence)
    }

    @Test fun `classifies canonical command acknowledgement`() {
        val frames = listOf(
            "ZARA/1".encodeToByteArray(),
            "{\"body\":{},\"id\":\"ack-1\",\"payload_count\":0,\"reply_to\":\"request-1\",\"session_id\":\"session-1\",\"stream_id\":\"mic-1\",\"timestamp_ns\":2,\"type\":\"audio.input.committed\"}".encodeToByteArray(),
        )

        val decoded = ZaraVoiceInboundCodec.decode(frames)

        assertTrue(decoded is VoiceInboundMessage.Reply)
        val reply = (decoded as VoiceInboundMessage.Reply).reply as VoiceServerReply.Acknowledged
        assertEquals("request-1", reply.replyTo)
        assertEquals("audio.input.committed", reply.type)
    }
}
