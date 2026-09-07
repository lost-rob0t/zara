package ai.zara.app.voice

interface PcmOutput : AutoCloseable {
    fun start(sampleRate: Int, channels: Int)
    fun write(pcm: ByteArray)
    fun stop()
    override fun close()
}

class VoicePlaybackController(
    private val output: PcmOutput,
    sessionId: String,
    private val audioFocus: AudioFocusController? = null,
) : AutoCloseable {
    private var state = VoiceStreamState.connected(sessionId)
    private var outputActive = false
    private var closed = false

    fun state(): VoiceStreamState = state

    fun accept(event: VoiceStreamEvent) {
        check(!closed) { "voice playback is closed" }
        when (event) {
            is VoiceStreamEvent.AudioStarted -> {
                val next = reduceVoiceStream(state, event)
                if (outputActive) {
                    stopOutputAndReleaseFocus()
                }
                if (audioFocus?.acquire() == false) {
                    state = clearAudioState()
                    throw IllegalStateException("Android assistant audio focus was denied")
                }
                try {
                    output.start(event.sampleRate, event.channels)
                } catch (error: Throwable) {
                    audioFocus?.release()
                    state = clearAudioState()
                    throw error
                }
                outputActive = true
                state = next
            }
            is VoiceStreamEvent.AudioChunk -> {
                val next = reduceVoiceStream(state, event)
                check(outputActive) { "audio output is not active" }
                try {
                    output.write(event.pcm.copyOf())
                } catch (error: Throwable) {
                    val cleanupError = runCatching { stopOutputAndReleaseFocus() }.exceptionOrNull()
                    state = clearAudioState()
                    if (cleanupError != null) error.addSuppressed(cleanupError)
                    throw error
                }
                state = next
            }
            is VoiceStreamEvent.AudioDone -> {
                val next = reduceVoiceStream(state, event)
                if (outputActive) {
                    stopOutputAndReleaseFocus()
                }
                state = next
            }
            is VoiceStreamEvent.Transcript -> {
                state = reduceVoiceStream(state, event)
            }
        }
    }

    fun interrupt(): ActiveAudioOutput? {
        check(!closed) { "voice playback is closed" }
        val interrupted = state.audio ?: return null
        if (outputActive) {
            stopOutputAndReleaseFocus()
        } else {
            audioFocus?.release()
        }
        state = clearAudioState()
        return interrupted
    }

    override fun close() {
        if (closed) return
        closed = true
        val stopFailure = if (outputActive) {
            runCatching { stopOutputAndReleaseFocus() }.exceptionOrNull()
        } else {
            runCatching { audioFocus?.release() }.exceptionOrNull()
        }
        val closeFailure = runCatching { output.close() }.exceptionOrNull()
        if (stopFailure != null) {
            if (closeFailure != null) stopFailure.addSuppressed(closeFailure)
            throw stopFailure
        }
        if (closeFailure != null) throw closeFailure
    }

    private fun stopOutputAndReleaseFocus() {
        try {
            output.stop()
        } finally {
            outputActive = false
            audioFocus?.release()
        }
    }

    private fun clearAudioState(): VoiceStreamState = state.copy(
        audio = null,
        lastAudioSequence = null,
        lastAudioChunk = null,
    )
}
