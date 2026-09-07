package ai.zara.app.voice

import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Assert.assertThrows
import org.junit.Test

class VoicePlaybackCloseFailureTest {
    @Test
    fun closeClearsCanonicalAudioAfterSuccessfulHardwareCleanup() {
        val output = CloseFailingPcmOutput()
        val controller = VoicePlaybackController(output, "session-1")
        controller.accept(
            VoiceStreamEvent.AudioStarted("session-1", "turn-1", "speaker-1", 24_000, 1)
        )

        controller.close()

        assertNull(controller.state().audio)
        assertEquals(listOf("start", "stop", "close"), output.calls)
    }

    @Test
    fun closeStillClosesOutputWhenStopFails() {
        val output = CloseFailingPcmOutput(failStop = true)
        val controller = VoicePlaybackController(output, "session-1")
        controller.accept(
            VoiceStreamEvent.AudioStarted("session-1", "turn-1", "speaker-1", 24_000, 1)
        )

        val error = assertThrows(IllegalStateException::class.java) {
            controller.close()
        }

        assertEquals("speaker stop failed", error.message)
        assertNull(controller.state().audio)
        assertEquals(listOf("start", "stop", "close"), output.calls)
    }

    @Test
    fun closeKeepsStopFailurePrimaryWhenOutputCloseAlsoFails() {
        val output = CloseFailingPcmOutput(failStop = true, failClose = true)
        val controller = VoicePlaybackController(output, "session-1")
        controller.accept(
            VoiceStreamEvent.AudioStarted("session-1", "turn-1", "speaker-1", 24_000, 1)
        )

        val error = assertThrows(IllegalStateException::class.java) {
            controller.close()
        }

        assertEquals("speaker stop failed", error.message)
        assertEquals(1, error.suppressed.size)
        assertEquals("speaker close failed", error.suppressed.single().message)
        assertNull(controller.state().audio)
        assertEquals(listOf("start", "stop", "close"), output.calls)
    }
}

private class CloseFailingPcmOutput(
    private val failStop: Boolean = false,
    private val failClose: Boolean = false,
) : PcmOutput {
    val calls = mutableListOf<String>()

    override fun start(sampleRate: Int, channels: Int) {
        calls += "start"
    }

    override fun write(pcm: ByteArray) = Unit

    override fun stop() {
        calls += "stop"
        if (failStop) throw IllegalStateException("speaker stop failed")
    }

    override fun close() {
        calls += "close"
        if (failClose) throw IllegalStateException("speaker close failed")
    }
}
