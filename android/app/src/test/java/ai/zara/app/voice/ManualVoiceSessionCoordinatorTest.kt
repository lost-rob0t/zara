package ai.zara.app.voice

import ai.zara.app.runtime.EnrollmentReadiness
import ai.zara.app.runtime.RuntimeState
import ai.zara.app.runtime.ServerConnection
import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class ManualVoiceSessionCoordinatorTest {
    @Test fun `press binds stream to canonical connected session and conversation`() {
        val ingress = CoordinatorIngress()
        val recorder = CoordinatorRecorder()
        val coordinator = ManualVoiceSessionCoordinator(
            PushToTalkController(ManualVoiceCapture(ingress), recorder),
            streamIds = sequenceOf("mic-1").iterator(),
        )
        val state = RuntimeState.initial().copy(
            enrollment = EnrollmentReadiness.Ready,
            server = ServerConnection.Connected(4),
            sessionId = "session-1",
            selectedConversationId = "conversation-1",
        )

        coordinator.press(state, permissionGranted = true)
        coordinator.release()

        assertEquals(
            listOf(
                "start:session-1:conversation-1:mic-1",
                "commit:mic-1",
            ),
            ingress.calls,
        )
        assertEquals(listOf("start", "stop"), recorder.calls)
    }

    @Test fun `disconnected runtime is rejected before microphone ownership begins`() {
        val ingress = CoordinatorIngress()
        val recorder = CoordinatorRecorder()
        val coordinator = ManualVoiceSessionCoordinator(
            PushToTalkController(ManualVoiceCapture(ingress), recorder),
            streamIds = sequenceOf("mic-1").iterator(),
        )

        assertThrows(IllegalStateException::class.java) {
            coordinator.press(RuntimeState.initial(), permissionGranted = true)
        }
        assertEquals(emptyList<String>(), ingress.calls)
        assertEquals(emptyList<String>(), recorder.calls)
    }

    @Test fun `permission denial cannot open a server audio stream`() {
        val ingress = CoordinatorIngress()
        val recorder = CoordinatorRecorder()
        val coordinator = ManualVoiceSessionCoordinator(
            PushToTalkController(ManualVoiceCapture(ingress), recorder),
            streamIds = sequenceOf("mic-1").iterator(),
        )
        val state = RuntimeState.initial().copy(
            enrollment = EnrollmentReadiness.Ready,
            server = ServerConnection.Connected(2),
            sessionId = "session-1",
        )

        assertThrows(IllegalStateException::class.java) {
            coordinator.press(state, permissionGranted = false)
        }
        assertEquals(emptyList<String>(), ingress.calls)
        assertEquals(emptyList<String>(), recorder.calls)
    }

    @Test fun `permission revocation crosses coordinator and cancels single capture owner`() {
        val ingress = CoordinatorIngress()
        val recorder = CoordinatorRecorder()
        val coordinator = ManualVoiceSessionCoordinator(
            PushToTalkController(ManualVoiceCapture(ingress), recorder),
            streamIds = sequenceOf("mic-1").iterator(),
        )
        val state = RuntimeState.initial().copy(
            enrollment = EnrollmentReadiness.Ready,
            server = ServerConnection.Connected(2),
            sessionId = "session-1",
        )

        coordinator.press(state, permissionGranted = true)
        coordinator.onMicrophonePermissionChanged(granted = false)
        coordinator.onMicrophonePermissionChanged(granted = false)

        assertEquals(
            listOf(
                "start:session-1:null:mic-1",
                "cancel:mic-1",
            ),
            ingress.calls,
        )
        assertEquals(listOf("start", "stop"), recorder.calls)
        assertEquals(ManualVoiceState.Idle, coordinator.state())
    }

    @Test fun `host stop cancels active capture exactly once and is idle-safe`() {
        val ingress = CoordinatorIngress()
        val recorder = CoordinatorRecorder()
        val coordinator = ManualVoiceSessionCoordinator(
            PushToTalkController(ManualVoiceCapture(ingress), recorder),
            streamIds = sequenceOf("mic-1").iterator(),
        )
        val state = RuntimeState.initial().copy(
            enrollment = EnrollmentReadiness.Ready,
            server = ServerConnection.Connected(2),
            sessionId = "session-1",
        )

        coordinator.press(state, permissionGranted = true)
        coordinator.onHostStopped()
        coordinator.onHostStopped()

        assertEquals(
            listOf(
                "start:session-1:null:mic-1",
                "cancel:mic-1",
            ),
            ingress.calls,
        )
        assertEquals(listOf("start", "stop"), recorder.calls)
        assertEquals(ManualVoiceState.Idle, coordinator.state())
    }
}

private class CoordinatorIngress : VoiceIngress {
    val calls = mutableListOf<String>()

    override fun start(context: VoiceCaptureContext) {
        calls += "start:${context.sessionId}:${context.conversationId}:${context.streamId}"
    }

    override fun chunk(context: VoiceCaptureContext, sequence: Long, pcm: ByteArray) = Unit

    override fun commit(context: VoiceCaptureContext) {
        calls += "commit:${context.streamId}"
    }

    override fun cancel(context: VoiceCaptureContext) {
        calls += "cancel:${context.streamId}"
    }
}

private class CoordinatorRecorder : PcmRecorder {
    val calls = mutableListOf<String>()

    override fun start(onFrame: (ByteArray) -> Unit, onFailure: (Throwable) -> Unit) {
        calls += "start"
    }

    override fun stop() {
        calls += "stop"
    }

    override fun close() = Unit
}
