package ai.zara.app.voice

import ai.zara.app.runtime.ZaraWireException

sealed interface VoiceInboundMessage {
    data class Stream(val event: VoiceStreamEvent) : VoiceInboundMessage
    data class Reply(val reply: VoiceServerReply) : VoiceInboundMessage
}

object ZaraVoiceInboundCodec {
    fun decode(frames: List<ByteArray>): VoiceInboundMessage {
        try {
            return VoiceInboundMessage.Stream(ZaraVoiceStreamCodec.decode(frames))
        } catch (streamError: ZaraWireException) {
            try {
                return VoiceInboundMessage.Reply(ZaraVoiceAckCodec.decode(frames))
            } catch (replyError: ZaraWireException) {
                streamError.addSuppressed(replyError)
                throw streamError
            }
        }
    }
}
