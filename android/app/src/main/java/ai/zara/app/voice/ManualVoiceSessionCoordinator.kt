package ai.zara.app.voice

import ai.zara.app.runtime.RuntimeState
import ai.zara.app.runtime.ServerConnection

class ManualVoiceSessionCoordinator(
    private val pushToTalk: PushToTalkController,
    private val streamIds: Iterator<String> = generateSequence {
        "mic-${java.util.UUID.randomUUID().toString().replace("-", "")}"
    }.iterator(),
) : AutoCloseable {
    fun state(): ManualVoiceState = pushToTalk.state()

    fun press(runtime: RuntimeState, permissionGranted: Boolean) {
        check(runtime.server is ServerConnection.Connected) {
            "authenticated Zara session is required"
        }
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
    }

    fun release() {
        pushToTalk.release()
    }

    fun cancel() {
        pushToTalk.cancel()
    }

    fun onMicrophonePermissionChanged(granted: Boolean) {
        pushToTalk.onMicrophonePermissionChanged(granted)
    }

    override fun close() {
        pushToTalk.close()
    }
}