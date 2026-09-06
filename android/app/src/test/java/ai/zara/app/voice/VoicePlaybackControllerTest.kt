package ai.zara.app.voice

import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class VoicePlaybackControllerTest {
    @Test fun `audio start chunk done owns one output lifecycle`() {
        val sink = RecordingPcmOutput()
        val controller = VoicePlaybackController(sink, "session-1")

        controller.accept(
            VoiceStreamEvent.AudioStarted("session-1", "turn-1", "speaker-1", 24_000, 1)
        )
        controller.accept(
            VoiceStreamEvent.AudioChunk(
                "session-1",
                "turn-1",
                "speaker-1",
                4,
                byteArrayOf(1, 0, 2, 0),
            )
        )
        controller.accept(
            VoiceStreamEvent.AudioDone("session-1", "turn-1", "speaker-1")
        )

        assertEquals(
            listOf(
                "start:24000:1",
                "write:4",
                "stop",
            ),
            sink.calls,
        )
    }

    @Test fun `superseding output stops old sink before starting replacement`() {
        val sink = RecordingPcmOutput()
        val controller = VoicePlaybackController(sink, "session-1")

        controller.accept(
            VoiceStreamEvent.AudioStarted("session-1", "turn-1", "speaker-1", 24_000, 1)
        )
        controller.accept(
            VoiceStreamEvent.AudioStarted("session-1", "turn-2", "speaker-2", 24_000, 1)
        )

        assertEquals(
            listOf(
                "start:24000:1",
                "stop",
                "start:24000:1",
            ),
            sink.calls,
        )
    }

    @Test fun `failed replacement leaves no fabricated active output`() {
        val sink = RecordingPcmOutput(failOnStartNumber = 2)
        val controller = VoicePlaybackController(sink, "session-1")
        controller.accept(
            VoiceStreamEvent.AudioStarted("session-1", "turn-1", "speaker-1", 24_000, 1)
        )

        assertThrows(IllegalStateException::class.java) {
            controller.accept(
                VoiceStreamEvent.AudioStarted("session-1", "turn-2", "speaker-2", 24_000, 1)
            )
        }

        assertEquals(null, controller.state().audio)
        controller.close()
        assertEquals(
            listOf("start:24000:1", "stop", "start:24000:1", "close"),
            sink.calls,
        )
    }

    @Test fun `failed PCM write stops output and clears active stream`() {
        val sink = RecordingPcmOutput(failOnWrite = true)
        val controller = VoicePlaybackController(sink, "session-1")
        controller.accept(
            VoiceStreamEvent.AudioStarted("session-1", "turn-3", "speaker-3", 24_000, 1)
        )

        assertThrows(IllegalStateException::class.java) {
            controller.accept(
                VoiceStreamEvent.AudioChunk(
                    "session-1",
                    "turn-3",
                    "speaker-3",
                    0,
                    byteArrayOf(1, 0),
                )
            )
        }

        assertEquals(null, controller.state().audio)
        assertEquals(listOf("start:24000:1", "write:2", "stop"), sink.calls)
        controller.close()
        assertEquals(listOf("start:24000:1", "write:2", "stop", "close"), sink.calls)
    }

    @Test fun `barge in returns interrupted turn stops speaker immediately and rejects late chunk`() {
        val sink = RecordingPcmOutput()
        val controller = VoicePlaybackController(sink, "session-1")
        controller.accept(
            VoiceStreamEvent.AudioStarted("session-1", "turn-7", "speaker-7", 24_000, 1)
        )

        val interrupted = controller.interrupt()

        assertEquals("turn-7", interrupted?.turnId)
        assertEquals("speaker-7", interrupted?.streamId)
        assertEquals(null, controller.state().audio)
        assertEquals(listOf("start:24000:1", "stop"), sink.calls)
        assertThrows(StaleVoiceStreamException::class.java) {
            controller.accept(
                VoiceStreamEvent.AudioChunk(
                    "session-1",
                    "turn-7",
                    "speaker-7",
                    0,
                    byteArrayOf(1, 0),
                )
            )
        }
    }

    @Test fun `stale chunk never reaches speaker`() {
        val sink = RecordingPcmOutput()
        val controller = VoicePlaybackController(sink, "session-1")
        controller.accept(
            VoiceStreamEvent.AudioStarted("session-1", "turn-2", "speaker-2", 24_000, 1)
        )

        assertThrows(StaleVoiceStreamException::class.java) {
            controller.accept(
                VoiceStreamEvent.AudioChunk(
                    "session-1",
                    "turn-1",
                    "speaker-1",
                    1,
                    byteArrayOf(1, 0),
                )
            )
        }
        assertEquals(listOf("start:24000:1"), sink.calls)
    }

    @Test fun `close releases active output exactly once`() {
        val sink = RecordingPcmOutput()
        val controller = VoicePlaybackController(sink, "session-1")
        controller.accept(
            VoiceStreamEvent.AudioStarted("session-1", "turn-1", "speaker-1", 24_000, 1)
        )
        controller.close()
        controller.close()

        assertEquals(listOf("start:24000:1", "stop", "close"), sink.calls)
    }
}

private class RecordingPcmOutput(
    private val failOnStartNumber: Int? = null,
    private val failOnWrite: Boolean = false,
) : PcmOutput {
    val calls = mutableListOf<String>()
    private var startCount = 0

    override fun start(sampleRate: Int, channels: Int) {
        calls += "start:$sampleRate:$channels"
        startCount += 1
        if (startCount == failOnStartNumber) throw IllegalStateException("start failed")
    }

    override fun write(pcm: ByteArray) {
        calls += "write:${pcm.size}"
        if (failOnWrite) throw IllegalStateException("write failed")
    }

    override fun stop() {
        calls += "stop"
    }

    override fun close() {
        calls += "close"
    }
}
