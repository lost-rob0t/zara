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

private class RecordingPcmOutput : PcmOutput {
    val calls = mutableListOf<String>()

    override fun start(sampleRate: Int, channels: Int) {
        calls += "start:$sampleRate:$channels"
    }

    override fun write(pcm: ByteArray) {
        calls += "write:${pcm.size}"
    }

    override fun stop() {
        calls += "stop"
    }

    override fun close() {
        calls += "close"
    }
}
