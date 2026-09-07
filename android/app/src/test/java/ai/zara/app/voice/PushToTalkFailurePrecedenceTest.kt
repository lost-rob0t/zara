package ai.zara.app.voice

import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class PushToTalkFailurePrecedenceTest {
    @Test
    fun recorderStartFailureRemainsPrimaryWhenCanonicalCancelAlsoFails() {
        val events = mutableListOf<String>()
        val controller = PushToTalkController(
            ManualVoiceCapture(FailingCancelIngress(events)),
            FailurePcmRecorder(events, failStart = true),
        )

        val error = assertThrows(IllegalStateException::class.java) {
            controller.press(
                VoiceCaptureContext("session-1", null, "stream-1"),
                permissionGranted = true,
                connected = true,
            )
        }

        assertEquals("recorder failed", error.message)
        assertEquals(1, error.suppressed.size)
        assertEquals("cancel transport failed", error.suppressed.single().message)
    }

    @Test
    fun recorderStopFailureRemainsPrimaryWhenCanonicalCancelAlsoFails() {
        val events = mutableListOf<String>()
        val recorder = FailurePcmRecorder(events, failStop = true)
        val controller = PushToTalkController(
            ManualVoiceCapture(FailingCancelIngress(events)),
            recorder,
        )
        controller.press(
            VoiceCaptureContext("session-1", null, "stream-1"),
            permissionGranted = true,
            connected = true,
        )

        val error = assertThrows(IllegalStateException::class.java) {
            controller.cancel()
        }

        assertEquals("recorder stop failed", error.message)
        assertEquals(1, error.suppressed.size)
        assertEquals("cancel transport failed", error.suppressed.single().message)
    }
}

private class FailurePcmRecorder(
    private val events: MutableList<String>,
    private val failStart: Boolean = false,
    private val failStop: Boolean = false,
) : PcmRecorder {
    private var running = false

    override fun start(onFrame: (ByteArray) -> Unit, onFailure: (Throwable) -> Unit) {
        events += "recorder.start"
        if (failStart) throw IllegalStateException("recorder failed")
        running = true
    }

    override fun stop() {
        check(running)
        events += "recorder.stop"
        running = false
        if (failStop) throw IllegalStateException("recorder stop failed")
    }

    override fun close() = Unit
}

private class FailingCancelIngress(private val events: MutableList<String>) : VoiceIngress {
    override fun start(context: VoiceCaptureContext) {
        events += "start:${context.streamId}"
    }

    override fun chunk(context: VoiceCaptureContext, sequence: Long, pcm: ByteArray) = Unit

    override fun commit(context: VoiceCaptureContext) = Unit

    override fun cancel(context: VoiceCaptureContext) {
        events += "cancel:${context.streamId}"
        throw IllegalStateException("cancel transport failed")
    }
}
