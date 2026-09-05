package ai.zara.app.voice

import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Assert.assertTrue
import org.junit.Test

class VoiceStreamSinkActorTest {
    @Test fun `stream sink serializes reducer and playback ownership`() {
        val output = RecordingSinkOutput()
        val states = mutableListOf<VoiceStreamState>()
        val sink = VoiceStreamSinkActor(
            playbackFactory = { sessionId -> VoicePlaybackController(output, sessionId) },
            stateObserver = states::add,
        )

        sink.accept(VoiceStreamEvent.AudioStarted("session-1", "turn-1", "speaker-1", 24_000, 1)).get()
        sink.accept(
            VoiceStreamEvent.AudioChunk(
                "session-1",
                "turn-1",
                "speaker-1",
                0,
                byteArrayOf(1, 0),
            )
        ).get()
        sink.accept(VoiceStreamEvent.AudioDone("session-1", "turn-1", "speaker-1")).get()

        assertEquals(listOf("start", "write:2", "stop"), output.calls)
        assertEquals(3, states.size)
        assertEquals(null, states.last().audio)
        sink.close()
    }

    @Test fun `new authenticated session replaces prior playback owner`() {
        val outputs = mutableListOf<RecordingSinkOutput>()
        val sink = VoiceStreamSinkActor(
            playbackFactory = { sessionId ->
                RecordingSinkOutput().also(outputs::add).let { VoicePlaybackController(it, sessionId) }
            },
        )

        sink.accept(VoiceStreamEvent.Transcript("session-1", "conversation-1", "mic-1", 0, "one", true)).get()
        sink.accept(VoiceStreamEvent.Transcript("session-2", "conversation-2", "mic-2", 0, "two", true)).get()

        assertEquals(2, outputs.size)
        assertEquals(listOf("close"), outputs.first().calls)
        sink.close()
    }

    @Test fun `runtime reset closes active owner and permits fresh same-session stream`() {
        val outputs = mutableListOf<RecordingSinkOutput>()
        val sink = VoiceStreamSinkActor(
            playbackFactory = { sessionId ->
                RecordingSinkOutput().also(outputs::add).let { VoicePlaybackController(it, sessionId) }
            },
        )

        sink.accept(VoiceStreamEvent.AudioStarted("session-1", "turn-1", "speaker-1", 24_000, 1)).get()
        sink.reset().get()
        sink.accept(VoiceStreamEvent.Transcript("session-1", "conversation-2", "mic-2", 0, "fresh", true)).get()

        assertEquals(2, outputs.size)
        assertEquals(listOf("start", "stop", "close"), outputs.first().calls)
        sink.close()
    }

    @Test fun `bounded mailbox rejects excess events instead of growing without limit`() {
        val entered = CountDownLatch(1)
        val release = CountDownLatch(1)
        val output = BlockingSinkOutput(entered, release)
        val sink = VoiceStreamSinkActor(
            playbackFactory = { sessionId -> VoicePlaybackController(output, sessionId) },
            capacity = 1,
        )

        val first = sink.accept(VoiceStreamEvent.AudioStarted("session-1", "turn-1", "speaker-1", 24_000, 1))
        entered.await(1, TimeUnit.SECONDS)
        val queued = sink.accept(
            VoiceStreamEvent.AudioChunk("session-1", "turn-1", "speaker-1", 0, byteArrayOf(1, 0))
        )

        assertThrows(VoiceStreamBackpressureException::class.java) {
            sink.accept(
                VoiceStreamEvent.AudioChunk("session-1", "turn-1", "speaker-1", 1, byteArrayOf(2, 0))
            )
        }

        release.countDown()
        first.get()
        queued.get()
        sink.close()
    }

    @Test fun `playback failure is emitted by sink actor instead of disappearing in ignored future`() {
        val failures = mutableListOf<Throwable>()
        val output = FailingWriteSinkOutput()
        val sink = VoiceStreamSinkActor(
            playbackFactory = { sessionId -> VoicePlaybackController(output, sessionId) },
            failureObserver = failures::add,
        )
        sink.accept(VoiceStreamEvent.AudioStarted("session-1", "turn-1", "speaker-1", 24_000, 1)).get()

        assertThrows(Exception::class.java) {
            sink.accept(
                VoiceStreamEvent.AudioChunk("session-1", "turn-1", "speaker-1", 0, byteArrayOf(1, 0))
            ).get()
        }

        assertEquals(1, failures.size)
        assertTrue(failures.single().message!!.contains("speaker write failed"))
        sink.close()
    }
}

private open class RecordingSinkOutput : PcmOutput {
    val calls = mutableListOf<String>()
    override fun start(sampleRate: Int, channels: Int) { calls += "start" }
    override fun write(pcm: ByteArray) { calls += "write:${pcm.size}" }
    override fun stop() { calls += "stop" }
    override fun close() { calls += "close" }
}

private class BlockingSinkOutput(
    private val entered: CountDownLatch,
    private val release: CountDownLatch,
) : RecordingSinkOutput() {
    override fun start(sampleRate: Int, channels: Int) {
        entered.countDown()
        release.await(1, TimeUnit.SECONDS)
        super.start(sampleRate, channels)
    }
}

private class FailingWriteSinkOutput : RecordingSinkOutput() {
    override fun write(pcm: ByteArray) {
        throw IllegalStateException("speaker write failed")
    }
}
