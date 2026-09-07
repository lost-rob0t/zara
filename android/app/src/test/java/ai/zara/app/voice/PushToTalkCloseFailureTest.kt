package ai.zara.app.voice

import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class PushToTalkCloseFailureTest {
    @Test
    fun closeStillClosesRecorderWhenCanonicalCancelFails() {
        val recorder = CloseTrackingRecorder()
        val controller = PushToTalkController(
            ManualVoiceCapture(CancelFailingIngress()),
            recorder,
        )
        controller.press(
            VoiceCaptureContext("session-1", null, "stream-1"),
            permissionGranted = true,
            connected = true,
        )

        val error = assertThrows(IllegalStateException::class.java) {
            controller.close()
        }

        assertEquals("cancel transport failed", error.message)
        assertEquals(1, recorder.closeCount)
    }

    @Test
    fun closeKeepsCancelFailurePrimaryWhenRecorderCloseAlsoFails() {
        val recorder = CloseTrackingRecorder(failClose = true)
        val controller = PushToTalkController(
            ManualVoiceCapture(CancelFailingIngress()),
            recorder,
        )
        controller.press(
            VoiceCaptureContext("session-1", null, "stream-1"),
            permissionGranted = true,
            connected = true,
        )

        val error = assertThrows(IllegalStateException::class.java) {
            controller.close()
        }

        assertEquals("cancel transport failed", error.message)
        assertEquals(1, recorder.closeCount)
        assertEquals(1, error.suppressed.size)
        assertEquals("recorder close failed", error.suppressed.single().message)
    }
}

private class CloseTrackingRecorder(
    private val failClose: Boolean = false,
) : PcmRecorder {
    var closeCount = 0
        private set

    private var running = false

    override fun start(onFrame: (ByteArray) -> Unit, onFailure: (Throwable) -> Unit) {
        running = true
    }

    override fun stop() {
        check(running)
        running = false
    }

    override fun close() {
        closeCount += 1
        if (failClose) throw IllegalStateException("recorder close failed")
    }
}

private class CancelFailingIngress : VoiceIngress {
    override fun start(context: VoiceCaptureContext) = Unit

    override fun chunk(context: VoiceCaptureContext, sequence: Long, pcm: ByteArray) = Unit

    override fun commit(context: VoiceCaptureContext) = Unit

    override fun cancel(context: VoiceCaptureContext) {
        throw IllegalStateException("cancel transport failed")
    }
}
