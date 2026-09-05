package ai.zara.app.voice

import org.junit.Assert.assertEquals
import org.junit.Test

class VoiceStreamBargeInTest {
    @Test fun `sink interrupt returns active turn after local speaker stop`() {
        val output = BargeInOutput()
        val states = mutableListOf<VoiceStreamState>()
        val sink = VoiceStreamSinkActor(
            playbackFactory = { sessionId -> VoicePlaybackController(output, sessionId) },
            stateObserver = states::add,
        )
        sink.accept(
            VoiceStreamEvent.AudioStarted("session-1", "turn-9", "speaker-9", 24_000, 1)
        ).get()

        val interrupted = sink.interrupt().get()

        assertEquals("turn-9", interrupted?.turnId)
        assertEquals("speaker-9", interrupted?.streamId)
        assertEquals(listOf("start", "stop"), output.calls)
        assertEquals(null, states.last().audio)
        sink.close()
    }
}

private class BargeInOutput : PcmOutput {
    val calls = mutableListOf<String>()
    override fun start(sampleRate: Int, channels: Int) { calls += "start" }
    override fun write(pcm: ByteArray) { calls += "write" }
    override fun stop() { calls += "stop" }
    override fun close() { calls += "close" }
}
