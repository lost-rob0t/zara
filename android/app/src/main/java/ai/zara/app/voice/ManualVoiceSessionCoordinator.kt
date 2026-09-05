package ai.zara.app.voice

import ai.zara.app.runtime.RuntimeState
import ai.zara.app.runtime.ServerConnection

class ManualVoiceSessionCoordinator(
    private val pushToTalk: PushToTalkController,
    private val streamIds: Iterator<String> = generateSequence {
        "mic-${java.util.UUID.randomUUID().toString().replace("-", "")}"
    }.iterator(),
) : AutoCloseable {
    private var activeBinding: VoiceSessionBinding? = null

    @Synchronized
    fun state(): ManualVoiceState = pushToTalk.state()

    @Synchronized
    fun press(runtime: RuntimeState, permissionGranted: Boolean) {
        val connected = runtime.server as? ServerConnection.Connected
            ?: throw IllegalStateException("authenticated Zara session is required")
        val sessionId = runtime.sessionId
            ?: throw IllegalStateException("connected Zara session is missing session id")
        check(streamIds.hasNext()) { "voice stream id source exhausted" }
        pushToTalk.press(
            context = VoiceCaptureContext(
                sessionId = sessionId,
                conversationId = runtime.selectedConversationId,
                streamId = streamIds.next(),
            ),
            permissionGranted = permissionGranted,
            connected = true,
        )
        activeBinding = VoiceSessionBinding(connected.generation, sessionId)
    }

    @Synchronized
    fun release() {
        try {
            pushToTalk.release()
        } finally {
            activeBinding = null
        }
    }

    @Synchronized
    fun cancel() {
        cancelActive()
    }

    @Synchronized
    fun onMicrophonePermissionChanged(granted: Boolean) {
        if (granted || pushToTalk.state() !is ManualVoiceState.Capturing) return
        cancelActive()
    }

    @Synchronized
    fun onHostStopped() {
        if (pushToTalk.state() is ManualVoiceState.Capturing) cancelActive()
    }

    @Synchronized
    fun onRuntimeStateChanged(runtime: RuntimeState) {
        val binding = activeBinding ?: return
        if (pushToTalk.state() !is ManualVoiceState.Capturing) {
            activeBinding = null
            return
        }
        val connected = runtime.server as? ServerConnection.Connected
        if (
            connected == null ||
            connected.generation != binding.generation ||
            runtime.sessionId != binding.sessionId
        ) {
            cancelActive()
        }
    }

    @Synchronized
    override fun close() {
        try {
            pushToTalk.close()
        } finally {
            activeBinding = null
        }
    }

    private fun cancelActive() {
        try {
            pushToTalk.cancel()
        } finally {
            activeBinding = null
        }
    }

    private data class VoiceSessionBinding(
        val generation: Long,
        val sessionId: String,
    )
}
