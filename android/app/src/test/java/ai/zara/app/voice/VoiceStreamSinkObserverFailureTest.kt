package ai.zara.app.voice

import java.util.concurrent.CountDownLatch
import java.util.concurrent.ExecutionException
import java.util.concurrent.TimeUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class VoiceStreamSinkObserverFailureTest {
    @Test
    fun observerFailureDoesNotReplaceTypedMailboxBackpressure() {
        val entered = CountDownLatch(1)
        val release = CountDownLatch(1)
        val sink = VoiceStreamSinkActor(
            playbackFactory = { sessionId ->
                VoicePlaybackController(ObserverBlockingOutput(entered, release), sessionId)
            },
            failureObserver = { throw IllegalStateException("diagnostic observer failed") },
            capacity = 1,
        )

        val active = sink.accept(started())
        entered.await(1, TimeUnit.SECONDS)
        val queued = sink.accept(
            VoiceStreamEvent.AudioChunk("session-1", "turn-1", "speaker-1", 0, byteArrayOf(1, 0)),
        )

        val error = assertThrows(VoiceStreamBackpressureException::class.java) {
            sink.accept(
                VoiceStreamEvent.AudioChunk("session-1", "turn-1", "speaker-1", 1, byteArrayOf(2, 0)),
            )
        }

        assertEquals("voice stream mailbox is full", error.message)
        assertEquals(1, error.suppressed.size)
        assertEquals("diagnostic observer failed", error.suppressed.single().message)
        release.countDown()
        active.get()
        queued.get()
        runCatching { sink.close() }
    }

    @Test
    fun observerFailureCannotWedgeResetAfterPlaybackCloseFailure() {
        val sink = VoiceStreamSinkActor(
            playbackFactory = { sessionId ->
                VoicePlaybackController(ObserverFailingCloseOutput(), sessionId)
            },
            failureObserver = { throw IllegalStateException("diagnostic observer failed") },
        )
        sink.accept(
            VoiceStreamEvent.Transcript("session-1", "conversation-1", "mic-1", 0, "ready", true),
        ).get()

        val error = assertThrows(ExecutionException::class.java) {
            sink.reset().get(1, TimeUnit.SECONDS)
        }

        assertEquals("speaker close failed", error.cause?.message)
        assertEquals(1, error.cause?.suppressed?.size)
        assertEquals("diagnostic observer failed", error.cause?.suppressed?.single()?.message)
        runCatching { sink.close() }
    }

    private fun started() =
        VoiceStreamEvent.AudioStarted("session-1", "turn-1", "speaker-1", 24_000, 1)
}

private class ObserverBlockingOutput(
    private val entered: CountDownLatch,
    private val release: CountDownLatch,
) : PcmOutput {
    override fun start(sampleRate: Int, channels: Int) {
        entered.countDown()
        release.await(1, TimeUnit.SECONDS)
    }
    override fun write(pcm: ByteArray) = Unit
    override fun stop() = Unit
    override fun close() = Unit
}

private class ObserverFailingCloseOutput : PcmOutput {
    override fun start(sampleRate: Int, channels: Int) = Unit
    override fun write(pcm: ByteArray) = Unit
    override fun stop() = Unit
    override fun close() {
        throw IllegalStateException("speaker close failed")
    }
}
