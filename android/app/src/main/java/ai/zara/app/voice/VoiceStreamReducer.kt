package ai.zara.app.voice

class StaleVoiceStreamException(message: String) : IllegalStateException(message)

data class ActiveAudioOutput(
    val turnId: String,
    val streamId: String,
    val sampleRate: Int,
    val channels: Int,
)

data class VoiceStreamState(
    val sessionId: String,
    val conversationId: String? = null,
    val transcriptStreamId: String? = null,
    val transcriptText: String = "",
    val transcriptFinal: Boolean = false,
    val lastTranscriptSequence: Long? = null,
    val audio: ActiveAudioOutput? = null,
    val lastAudioSequence: Long? = null,
    val lastAudioChunk: ByteArray? = null,
) {
    companion object {
        fun connected(sessionId: String): VoiceStreamState {
            require(sessionId.isNotBlank()) { "session id is required" }
            return VoiceStreamState(sessionId = sessionId)
        }
    }
}

fun reduceVoiceStream(
    state: VoiceStreamState,
    event: VoiceStreamEvent,
): VoiceStreamState {
    if (event.sessionId != state.sessionId) {
        throw StaleVoiceStreamException("voice event belongs to a stale session")
    }
    return when (event) {
        is VoiceStreamEvent.Transcript -> reduceTranscript(state, event)
        is VoiceStreamEvent.AudioStarted -> state.copy(
            audio = ActiveAudioOutput(
                turnId = event.turnId,
                streamId = event.streamId,
                sampleRate = event.sampleRate,
                channels = event.channels,
            ),
            lastAudioSequence = null,
            lastAudioChunk = null,
        )
        is VoiceStreamEvent.AudioChunk -> reduceAudioChunk(state, event)
        is VoiceStreamEvent.AudioDone -> reduceAudioDone(state, event)
    }
}

private fun reduceTranscript(
    state: VoiceStreamState,
    event: VoiceStreamEvent.Transcript,
): VoiceStreamState {
    val previous = state.lastTranscriptSequence
    if (previous != null && event.sequence <= previous) {
        throw StaleVoiceStreamException("voice transcript sequence is stale")
    }
    if (
        state.transcriptStreamId != null &&
        state.transcriptStreamId != event.streamId &&
        !state.transcriptFinal
    ) {
        throw StaleVoiceStreamException("voice transcript stream changed before terminal transcript")
    }
    return state.copy(
        conversationId = event.conversationId,
        transcriptStreamId = event.streamId,
        transcriptText = event.text,
        transcriptFinal = event.final,
        lastTranscriptSequence = event.sequence,
    )
}

private fun reduceAudioChunk(
    state: VoiceStreamState,
    event: VoiceStreamEvent.AudioChunk,
): VoiceStreamState {
    val active = state.audio
        ?: throw StaleVoiceStreamException("audio chunk arrived without active output")
    if (active.turnId != event.turnId || active.streamId != event.streamId) {
        throw StaleVoiceStreamException("audio chunk belongs to stale output")
    }
    val previous = state.lastAudioSequence
    if (previous != null && event.sequence <= previous) {
        throw StaleVoiceStreamException("audio output sequence is stale")
    }
    return state.copy(
        lastAudioSequence = event.sequence,
        lastAudioChunk = event.pcm.copyOf(),
    )
}

private fun reduceAudioDone(
    state: VoiceStreamState,
    event: VoiceStreamEvent.AudioDone,
): VoiceStreamState {
    val active = state.audio
        ?: throw StaleVoiceStreamException("audio completion arrived without active output")
    if (active.turnId != event.turnId || active.streamId != event.streamId) {
        throw StaleVoiceStreamException("audio completion belongs to stale output")
    }
    return state.copy(
        audio = null,
        lastAudioSequence = null,
        lastAudioChunk = null,
    )
}
