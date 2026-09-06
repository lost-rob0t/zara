package ai.zara.app.voice

interface PcmRecorder : AutoCloseable {
    fun start(onFrame: (ByteArray) -> Unit, onFailure: (Throwable) -> Unit)
    fun stop()
}

class PushToTalkController(
    private val capture: ManualVoiceCapture,
    private val recorder: PcmRecorder,
    private val onRecorderFailure: (Throwable) -> Unit = {},
) : AutoCloseable {
    fun state(): ManualVoiceState = capture.state()

    fun press(
        context: VoiceCaptureContext,
        permissionGranted: Boolean,
        connected: Boolean,
    ) {
        capture.begin(context, permissionGranted, connected)
        try {
            recorder.start(
                capture::acceptPcm,
                ::handleRecorderFailure,
            )
        } catch (error: Throwable) {
            capture.cancelIfActive()
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
            capture.cancelIfActive()
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
        capture.cancelIfActive()
        if (stopFailure != null) throw stopFailure
    }

    fun onMicrophonePermissionChanged(granted: Boolean) {
        if (granted || capture.state() !is ManualVoiceState.Capturing) return
        cancel()
    }

    override fun close() {
        val cancelFailure = if (capture.state() is ManualVoiceState.Capturing) {
            runCatching { cancel() }.exceptionOrNull()
        } else {
            null
        }
        val closeFailure = runCatching { recorder.close() }.exceptionOrNull()
        if (cancelFailure != null) {
            if (closeFailure != null) cancelFailure.addSuppressed(closeFailure)
            throw cancelFailure
        }
        if (closeFailure != null) throw closeFailure
    }

    private fun handleRecorderFailure(error: Throwable) {
        capture.cancelIfActive()
        onRecorderFailure(error)
    }
}