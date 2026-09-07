package ai.zara.app.voice

import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import org.junit.Assert.assertThrows
import org.junit.Assert.assertTrue
import org.junit.Test

class VoiceStreamSinkSelfCloseTest {
    @Test
    fun `observer initiated close cannot deadlock the sink actor`() {
        lateinit var sink: VoiceStreamSinkActor
        val returnedFromClose = CountDownLatch(1)
        sink = VoiceStreamSinkActor(
            playbackFactory = { sessionId -> VoicePlaybackController(SelfCloseOutput(), sessionId) },
            stateObserver = {
                sink.close()
                returnedFromClose.countDown()
            },
        )

        val accepted = sink.accept(
            VoiceStreamEvent.Transcript(
                sessionId = "session-1",
                conversationId = "conversation-1",
                streamId = "mic-1",
                sequence = 0,
                text = "done",
                final = true,
            )
        )

        assertTrue(returnedFromClose.await(1, TimeUnit.SECONDS))
        accepted.get(1, TimeUnit.SECONDS)
        assertThrows(IllegalStateException::class.java) {
            sink.accept(
                VoiceStreamEvent.Transcript(
                    "session-1", "conversation-1", "mic-1", 1, "stale", true
                )
            )
        }
    }
}

private class SelfCloseOutput : PcmOutput {
    override fun start(sampleRate: Int, channels: Int) = Unit
    override fun write(pcm: ByteArray) = Unit
    override fun stop() = Unit
    override fun close() = Unit
}
