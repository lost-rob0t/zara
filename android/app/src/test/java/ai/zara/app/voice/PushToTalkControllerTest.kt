package ai.zara.app.voice

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertThrows
import org.junit.Test

class PushToTalkControllerTest {
    @Test fun pressStreamsFramesAndReleaseStopsRecorderBeforeCommit() {
        val events = mutableListOf<String>(); val ingress = OrderedIngress(events); val recorder = FakePcmRecorder(events)
        val controller = PushToTalkController(ManualVoiceCapture(ingress), recorder)
        controller.press(VoiceCaptureContext("session-1", "conversation-1", "stream-1"), true, true)
        recorder.emit(ByteArray(1024)); controller.release()
        assertEquals(listOf("start:stream-1","recorder.start","chunk:stream-1:0","recorder.stop","commit:stream-1"), events)
        assertFalse(recorder.running); assertEquals(ManualVoiceState.Idle, controller.state())
    }

    @Test fun recorderStartFailureCancelsOpenedDaemonStream() {
        val events = mutableListOf<String>(); val recorder = FakePcmRecorder(events, failStart = true)
        val controller = PushToTalkController(ManualVoiceCapture(OrderedIngress(events)), recorder)
        assertThrows(IllegalStateException::class.java) { controller.press(VoiceCaptureContext("session-1", null, "stream-1"), true, true) }
        assertEquals(listOf("start:stream-1","recorder.start","cancel:stream-1"), events)
        assertEquals(ManualVoiceState.Idle, controller.state())
    }

    @Test fun recorderRuntimeFailureCancelsCanonicalStreamAndSurfacesError() {
        val events = mutableListOf<String>(); val failures = mutableListOf<String>(); val recorder = FakePcmRecorder(events)
        val controller = PushToTalkController(ManualVoiceCapture(OrderedIngress(events)), recorder, onRecorderFailure = { failures += requireNotNull(it.message) })
        controller.press(VoiceCaptureContext("session-1", null, "stream-1"), true, true)
        recorder.fail(IllegalStateException("audio read failed"))
        assertEquals(listOf("start:stream-1","recorder.start","cancel:stream-1"), events)
        assertEquals(listOf("audio read failed"), failures); assertEquals(ManualVoiceState.Idle, controller.state())
    }

    @Test fun cancelStopsRecorderBeforeCanonicalCancel() {
        val events = mutableListOf<String>(); val controller = PushToTalkController(ManualVoiceCapture(OrderedIngress(events)), FakePcmRecorder(events))
        controller.press(VoiceCaptureContext("session-1", null, "stream-1"), true, true); controller.cancel()
        assertEquals(listOf("start:stream-1","recorder.start","recorder.stop","cancel:stream-1"), events)
    }

    @Test fun permissionRevocationStopsRecorderAndCanonicalStreamExactlyOnce() {
        val events = mutableListOf<String>(); val recorder = FakePcmRecorder(events)
        val controller = PushToTalkController(ManualVoiceCapture(OrderedIngress(events)), recorder)
        controller.press(VoiceCaptureContext("session-1", null, "stream-1"), true, true)
        controller.onMicrophonePermissionChanged(false); controller.onMicrophonePermissionChanged(false); controller.onMicrophonePermissionChanged(true)
        assertEquals(listOf("start:stream-1","recorder.start","recorder.stop","cancel:stream-1"), events)
        assertFalse(recorder.running); assertEquals(ManualVoiceState.Idle, controller.state())
    }

    @Test fun permissionRevocationStillCancelsCanonicalStreamWhenRecorderStopFails() {
        val events = mutableListOf<String>(); val recorder = FakePcmRecorder(events, failStop = true)
        val controller = PushToTalkController(ManualVoiceCapture(OrderedIngress(events)), recorder)
        controller.press(VoiceCaptureContext("session-1", null, "stream-1"), true, true)
        assertThrows(IllegalStateException::class.java) { controller.onMicrophonePermissionChanged(false) }
        assertEquals(listOf("start:stream-1","recorder.start","recorder.stop","cancel:stream-1"), events)
        assertEquals(ManualVoiceState.Idle, controller.state())
    }

    @Test fun recorderStartFailureRemainsPrimaryWhenCanonicalCancelAlsoFails() {
        val events = mutableListOf<String>()
        val controller = PushToTalkController(
            ManualVoiceCapture(FailingCancelIngress(events)),
            FakePcmRecorder(events, failStart = true),
        )
        val error = assertThrows(IllegalStateException::class.java) {
            controller.press(VoiceCaptureContext("session-1", null, "stream-1"), true, true)
        }
        assertEquals("recorder failed", error.message)
        assertEquals(1, error.suppressed.size)
        assertEquals("cancel transport failed", error.suppressed.single().message)
    }

    @Test fun recorderStopFailureRemainsPrimaryWhenCanonicalCancelAlsoFails() {
        val events = mutableListOf<String>(); val recorder = FakePcmRecorder(events, failStop = true)
        val controller = PushToTalkController(ManualVoiceCapture(FailingCancelIngress(events)), recorder)
        controller.press(VoiceCaptureContext("session-1", null, "stream-1"), true, true)
        val error = assertThrows(IllegalStateException::class.java) { controller.cancel() }
        assertEquals("recorder stop failed", error.message)
        assertEquals(1, error.suppressed.size)
        assertEquals("cancel transport failed", error.suppressed.single().message)
    }
}

private class FakePcmRecorder(
    private val events: MutableList<String>,
    private val failStart: Boolean = false,
    private val failStop: Boolean = false,
) : PcmRecorder {
    private var consumer: ((ByteArray) -> Unit)? = null
    private var failureConsumer: ((Throwable) -> Unit)? = null
    var running = false
        private set
    override fun start(onFrame: (ByteArray) -> Unit, onFailure: (Throwable) -> Unit) {
        events += "recorder.start"; if (failStart) throw IllegalStateException("recorder failed")
        check(!running); running = true; consumer = onFrame; failureConsumer = onFailure
    }
    override fun stop() {
        check(running); events += "recorder.stop"; running = false; consumer = null; failureConsumer = null
        if (failStop) throw IllegalStateException("recorder stop failed")
    }
    fun emit(frame: ByteArray) { check(running); requireNotNull(consumer).invoke(frame) }
    fun fail(error: Throwable) { check(running); running = false; consumer = null; val callback = requireNotNull(failureConsumer); failureConsumer = null; callback(error) }
    override fun close() { if (running) stop() }
}

private class OrderedIngress(private val events: MutableList<String>) : VoiceIngress {
    override fun start(context: VoiceCaptureContext) { events += "start:${context.streamId}" }
    override fun chunk(context: VoiceCaptureContext, sequence: Long, pcm: ByteArray) { events += "chunk:${context.streamId}:$sequence" }
    override fun commit(context: VoiceCaptureContext) { events += "commit:${context.streamId}" }
    override fun cancel(context: VoiceCaptureContext) { events += "cancel:${context.streamId}" }
}

private class FailingCancelIngress(private val events: MutableList<String>) : VoiceIngress {
    override fun start(context: VoiceCaptureContext) { events += "start:${context.streamId}" }
    override fun chunk(context: VoiceCaptureContext, sequence: Long, pcm: ByteArray) = Unit
    override fun commit(context: VoiceCaptureContext) = Unit
    override fun cancel(context: VoiceCaptureContext) { events += "cancel:${context.streamId}"; throw IllegalStateException("cancel transport failed") }
}
