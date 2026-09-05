package ai.zara.app.voice

data class VoiceCaptureContext(
    val sessionId: String,
    val conversationId: String?,
    val streamId: String,
) {
    init {
        require(sessionId.isNotBlank()) { "session id is required" }
        require(streamId.isNotBlank()) { "stream id is required" }
        require(conversationId == null || conversationId.isNotBlank()) {
            "conversation id must be absent or non-blank"
        }
    }
}

sealed interface ManualVoiceState {
    data object Idle : ManualVoiceState

    data class Capturing(
        val context: VoiceCaptureContext,
        val nextSequence: Long,
    ) : ManualVoiceState
}

interface VoiceIngress {
    fun start(context: VoiceCaptureContext)
    fun chunk(context: VoiceCaptureContext, sequence: Long, pcm: ByteArray)
    fun commit(context: VoiceCaptureContext)
    fun cancel(context: VoiceCaptureContext)
}

class ManualVoiceCapture(private val ingress: VoiceIngress) {
    private var state: ManualVoiceState = ManualVoiceState.Idle

    @Synchronized
    fun state(): ManualVoiceState = state

    @Synchronized
    fun begin(
        context: VoiceCaptureContext,
        permissionGranted: Boolean,
        connected: Boolean,
    ) {
        check(state is ManualVoiceState.Idle) { "manual voice capture is already active" }
        check(permissionGranted) { "microphone permission is required" }
        check(connected) { "authenticated Zara session is required" }

        ingress.start(context)
        state = ManualVoiceState.Capturing(context, nextSequence = 0)
    }

    @Synchronized
    fun acceptPcm(pcm: ByteArray) {
        require(pcm.size == PCM_FRAME_BYTES) {
            "manual voice PCM frame must be exactly $PCM_FRAME_BYTES bytes"
        }
        val capturing = state as? ManualVoiceState.Capturing
            ?: throw IllegalStateException("manual voice capture is not active")
        check(capturing.nextSequence < Long.MAX_VALUE) { "manual voice sequence exhausted" }

        ingress.chunk(
            capturing.context,
            capturing.nextSequence,
            pcm.copyOf(),
        )
        state = capturing.copy(nextSequence = capturing.nextSequence + 1)
    }

    @Synchronized
    fun commit() {
        val capturing = state as? ManualVoiceState.Capturing
            ?: throw IllegalStateException("manual voice capture is not active")
        ingress.commit(capturing.context)
        state = ManualVoiceState.Idle
    }

    @Synchronized
    fun cancel() {
        check(cancelIfActive()) { "manual voice capture is not active" }
    }

    @Synchronized
    fun cancelIfActive(): Boolean {
        val capturing = state as? ManualVoiceState.Capturing ?: return false
        ingress.cancel(capturing.context)
        state = ManualVoiceState.Idle
        return true
    }

    companion object {
        const val PCM_FRAME_BYTES = 1024
        const val PCM_SAMPLE_RATE_HZ = 16_000
        const val PCM_CHANNELS = 1
        const val PCM_FRAME_SAMPLES = 512
    }
}
