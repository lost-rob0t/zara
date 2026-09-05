package ai.zara.app.voice

import java.util.concurrent.CompletableFuture

interface VoiceCommandClient {
    fun startVoice(context: VoiceCaptureContext): CompletableFuture<Unit>

    fun sendVoiceChunk(
        context: VoiceCaptureContext,
        sequence: Long,
        pcm: ByteArray,
    ): CompletableFuture<Unit>

    fun commitVoice(context: VoiceCaptureContext): CompletableFuture<Unit>

    fun cancelVoice(context: VoiceCaptureContext): CompletableFuture<Unit>
}

class AuthenticatedVoiceIngress(
    private val client: VoiceCommandClient,
) : VoiceIngress {
    override fun start(context: VoiceCaptureContext) {
        client.startVoice(context).join()
    }

    override fun chunk(context: VoiceCaptureContext, sequence: Long, pcm: ByteArray) {
        client.sendVoiceChunk(context, sequence, pcm).join()
    }

    override fun commit(context: VoiceCaptureContext) {
        client.commitVoice(context).join()
    }

    override fun cancel(context: VoiceCaptureContext) {
        client.cancelVoice(context).join()
    }
}
