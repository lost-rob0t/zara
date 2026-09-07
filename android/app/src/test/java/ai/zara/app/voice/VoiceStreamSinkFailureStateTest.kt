package ai.zara.app.voice

import java.util.concurrent.ExecutionException
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Assert.assertThrows
import org.junit.Test

class VoiceStreamSinkFailureStateTest {
    @Test
    fun failedBargeInStillPublishesCanonicalClearedAudioState() {
        val states = mutableListOf<VoiceStreamState>()
        val sink = VoiceStreamSinkActor(
            playbackFactory = { sessionId ->
                VoicePlaybackController(SinkStopFailingOutput(), sessionId)
            },
            stateObserver = states::add,
        )
        sink.accept(
            VoiceStreamEvent.AudioStarted("session-1", "turn-1", "speaker-1", 24_000, 1),
        ).get()

        val error = assertThrows(ExecutionException::class.java) {
            sink.interrupt().get()
        }

        assertEquals("speaker stop failed", error.cause?.message)
        assertEquals(2, states.size)
        assertNull(states.last().audio)
        runCatching { sink.close() }
    }

    @Test
    fun failedAudioDoneStillPublishesCanonicalClearedAudioState() {
        val states = mutableListOf<VoiceStreamState>()
        val sink = VoiceStreamSinkActor(
            playbackFactory = { sessionId ->
                VoicePlaybackController(SinkStopFailingOutput(), sessionId)
            },
            stateObserver = states::add,
        )
        sink.accept(
            VoiceStreamEvent.AudioStarted("session-1", "turn-1", "speaker-1", 24_000, 1),
        ).get()

        val error = assertThrows(ExecutionException::class.java) {
            sink.accept(
                VoiceStreamEvent.AudioDone("session-1", "turn-1", "speaker-1"),
            ).get()
        }

        assertEquals("speaker stop failed", error.cause?.message)
        assertEquals(2, states.size)
        assertNull(states.last().audio)
        runCatching { sink.close() }
    }
}

private class SinkStopFailingOutput : PcmOutput {
    override fun start(sampleRate: Int, channels: Int) = Unit
    override fun write(pcm: ByteArray) = Unit
    override fun stop() {
        throw IllegalStateException("speaker stop failed")
    }
    override fun close() = Unit
}
