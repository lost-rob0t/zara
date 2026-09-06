package ai.zara.app.voice

import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class ManualVoiceCaptureTest {
    @Test
    fun pushToTalkOwnsOneBoundedPcmStreamAndCommitsInOrder() {
        val sink = RecordingVoiceIngress()
        val capture = ManualVoiceCapture(sink)

        capture.begin(
            VoiceCaptureContext(
                sessionId = "session-1",
                conversationId = "conversation-1",
                streamId = "stream-1",
            ),
            permissionGranted = true,
            connected = true,
        )
        capture.acceptPcm(ByteArray(1024) { 7 })
        capture.acceptPcm(ByteArray(1024) { 9 })
        capture.commit()

        assertEquals(
            listOf(
                "start:stream-1",
                "chunk:stream-1:0:1024",
                "chunk:stream-1:1:1024",
                "commit:stream-1",
            ),
            sink.events,
        )
        assertEquals(ManualVoiceState.Idle, capture.state())
    }

    @Test
    fun permissionAndConnectionFailClosedBeforeOpeningMicrophoneStream() {
        val sink = RecordingVoiceIngress()
        val capture = ManualVoiceCapture(sink)
        val context = VoiceCaptureContext("session-1", null, "stream-1")

        assertThrows(IllegalStateException::class.java) {
            capture.begin(context, permissionGranted = false, connected = true)
        }
        assertThrows(IllegalStateException::class.java) {
            capture.begin(context, permissionGranted = true, connected = false)
        }
        assertEquals(emptyList<String>(), sink.events)
        assertEquals(ManualVoiceState.Idle, capture.state())
    }

    @Test
    fun pcmGeometryAndTerminalStateAreStrict() {
        val sink = RecordingVoiceIngress()
        val capture = ManualVoiceCapture(sink)
        capture.begin(
            VoiceCaptureContext("session-1", null, "stream-1"),
            permissionGranted = true,
            connected = true,
        )

        assertThrows(IllegalArgumentException::class.java) {
            capture.acceptPcm(ByteArray(1022))
        }
        capture.cancel()
        assertThrows(IllegalStateException::class.java) {
            capture.acceptPcm(ByteArray(1024))
        }
        assertEquals(listOf("start:stream-1", "cancel:stream-1"), sink.events)
    }

    @Test
    fun terminalIngressFailuresStillClearLocalCaptureState() {
        val context = VoiceCaptureContext("session-1", null, "stream-1")

        val commitCapture = ManualVoiceCapture(FailingTerminalVoiceIngress(failCommit = true))
        commitCapture.begin(context, permissionGranted = true, connected = true)
        val commitFailure = assertThrows(IllegalStateException::class.java) {
            commitCapture.commit()
        }
        assertEquals("commit transport failed", commitFailure.message)
        assertEquals(ManualVoiceState.Idle, commitCapture.state())

        val cancelCapture = ManualVoiceCapture(FailingTerminalVoiceIngress(failCancel = true))
        cancelCapture.begin(context, permissionGranted = true, connected = true)
        val cancelFailure = assertThrows(IllegalStateException::class.java) {
            cancelCapture.cancel()
        }
        assertEquals("cancel transport failed", cancelFailure.message)
        assertEquals(ManualVoiceState.Idle, cancelCapture.state())
    }
}

private class RecordingVoiceIngress : VoiceIngress {
    val events = mutableListOf<String>()

    override fun start(context: VoiceCaptureContext) {
        events += "start:${context.streamId}"
    }

    override fun chunk(context: VoiceCaptureContext, sequence: Long, pcm: ByteArray) {
        events += "chunk:${context.streamId}:$sequence:${pcm.size}"
    }

    override fun commit(context: VoiceCaptureContext) {
        events += "commit:${context.streamId}"
    }

    override fun cancel(context: VoiceCaptureContext) {
        events += "cancel:${context.streamId}"
    }
}

private class FailingTerminalVoiceIngress(
    private val failCommit: Boolean = false,
    private val failCancel: Boolean = false,
) : VoiceIngress {
    override fun start(context: VoiceCaptureContext) = Unit

    override fun chunk(context: VoiceCaptureContext, sequence: Long, pcm: ByteArray) = Unit

    override fun commit(context: VoiceCaptureContext) {
        if (failCommit) throw IllegalStateException("commit transport failed")
    }

    override fun cancel(context: VoiceCaptureContext) {
        if (failCancel) throw IllegalStateException("cancel transport failed")
    }
}
