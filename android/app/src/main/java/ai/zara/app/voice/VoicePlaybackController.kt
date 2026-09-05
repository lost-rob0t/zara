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
                if (outputActive) output.stop()
                output.start(event.sampleRate, event.channels)
                outputActive = true
                state = next
            }
            is VoiceStreamEvent.AudioChunk -> {
                val next = reduceVoiceStream(state, event)
                check(outputActive) { "audio output is not active" }
                output.write(event.pcm.copyOf())
                state = next
            }
            is VoiceStreamEvent.AudioDone -> {
                val next = reduceVoiceStream(state, event)
                if (outputActive) {
                    output.stop()
                    outputActive = false
                }
                state = next
            }
            is VoiceStreamEvent.Transcript -> {
                state = reduceVoiceStream(state, event)
            }
        }
    }

    override fun close() {
        if (closed) return
        closed = true
        if (outputActive) {
            output.stop()
            outputActive = false
        }
        output.close()
    }
}
