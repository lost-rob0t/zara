package ai.zara.app.voice

interface PcmRecorder : AutoCloseable {
    fun start(onFrame: (ByteArray) -> Unit)
    fun stop()
}

class PushToTalkController(
    private val capture: ManualVoiceCapture,
    private val recorder: PcmRecorder,
) : AutoCloseable {
    fun state(): ManualVoiceState = capture.state()

    fun press(
        context: VoiceCaptureContext,
        permissionGranted: Boolean,
        connected: Boolean,
    ) {
        capture.begin(context, permissionGranted, connected)
        try {
            recorder.start(capture::acceptPcm)
        } catch (error: Throwable) {
            capture.cancel()
            throw error
        }
    }

    fun release() {
        check(capture.state() is ManualVoiceState.Capturing) {
            "push-to-talk capture is not active"
        }
        try {
            recorder.stop()
        } catch (error: Throwable) {
            capture.cancel()
            throw error
        }
        capture.commit()
    }

    fun cancel() {
        check(capture.state() is ManualVoiceState.Capturing) {
            "push-to-talk capture is not active"
        }
        var stopFailure: Throwable? = null
        try {
            recorder.stop()
        } catch (error: Throwable) {
            stopFailure = error
        }
        capture.cancel()
        if (stopFailure != null) throw stopFailure
    }

    override fun close() {
        if (capture.state() is ManualVoiceState.Capturing) {
            cancel()
        }
        recorder.close()
    }
}
