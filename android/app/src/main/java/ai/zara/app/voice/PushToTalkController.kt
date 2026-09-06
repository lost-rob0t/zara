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
            cancelCapturePreserving(error)
        }
    }

    fun release() {
        check(capture.state() is ManualVoiceState.Capturing) {
            "push-to-talk capture is not active"
        }
        try {
            recorder.stop()
        } catch (error: Throwable) {
            cancelCapturePreserving(error)
        }
        capture.commit()
    }

    fun cancel() {
        check(capture.state() is ManualVoiceState.Capturing) {
            "push-to-talk capture is not active"
        }
        val stopFailure = runCatching { recorder.stop() }.exceptionOrNull()
        val cancelFailure = runCatching { capture.cancelIfActive() }.exceptionOrNull()
        if (stopFailure != null) {
            if (cancelFailure != null) stopFailure.addSuppressed(cancelFailure)
            throw stopFailure
        }
        if (cancelFailure != null) throw cancelFailure
    }

    fun onMicrophonePermissionChanged(granted: Boolean) {
        if (granted || capture.state() !is ManualVoiceState.Capturing) return
        cancel()
    }

    override fun close() {
        if (capture.state() is ManualVoiceState.Capturing) {
            cancel()
        }
        recorder.close()
    }

    private fun handleRecorderFailure(error: Throwable) {
        val cleanupFailure = runCatching { capture.cancelIfActive() }.exceptionOrNull()
        if (cleanupFailure != null) error.addSuppressed(cleanupFailure)
        onRecorderFailure(error)
    }

    private fun cancelCapturePreserving(error: Throwable): Nothing {
        val cleanupFailure = runCatching { capture.cancelIfActive() }.exceptionOrNull()
        if (cleanupFailure != null) error.addSuppressed(cleanupFailure)
        throw error
    }
}
