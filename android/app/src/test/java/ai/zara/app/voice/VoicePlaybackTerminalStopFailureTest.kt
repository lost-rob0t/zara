package ai.zara.app.voice

import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Assert.assertThrows
import org.junit.Test

class VoicePlaybackTerminalStopFailureTest {
    @Test
    fun audioDoneStopFailureStillClearsCanonicalActiveAudio() {
        val output = TerminalStopFailingOutput()
        val controller = VoicePlaybackController(output, "session-1")
        controller.accept(started())

        val error = assertThrows(IllegalStateException::class.java) {
            controller.accept(VoiceStreamEvent.AudioDone("session-1", "turn-1", "speaker-1"))
        }

        assertEquals("speaker stop failed", error.message)
        assertNull(controller.state().audio)
        assertEquals(listOf("start", "stop"), output.calls)
    }

    @Test
    fun interruptStopFailureStillClearsCanonicalActiveAudio() {
        val output = TerminalStopFailingOutput()
        val controller = VoicePlaybackController(output, "session-1")
        controller.accept(started())

        val error = assertThrows(IllegalStateException::class.java) {
            controller.interrupt()
        }

        assertEquals("speaker stop failed", error.message)
        assertNull(controller.state().audio)
        assertEquals(listOf("start", "stop"), output.calls)
    }

    @Test
    fun replacementStopFailureDoesNotLeaveSupersededAudioActive() {
        val output = TerminalStopFailingOutput()
        val controller = VoicePlaybackController(output, "session-1")
        controller.accept(started())

        val error = assertThrows(IllegalStateException::class.java) {
            controller.accept(
                VoiceStreamEvent.AudioStarted("session-1", "turn-2", "speaker-2", 24_000, 1),
            )
        }

        assertEquals("speaker stop failed", error.message)
        assertNull(controller.state().audio)
        assertEquals(listOf("start", "stop"), output.calls)
    }

    private fun started() =
        VoiceStreamEvent.AudioStarted("session-1", "turn-1", "speaker-1", 24_000, 1)
}

private class TerminalStopFailingOutput : PcmOutput {
    val calls = mutableListOf<String>()

    override fun start(sampleRate: Int, channels: Int) {
        calls += "start"
    }

    override fun write(pcm: ByteArray) = Unit

    override fun stop() {
        calls += "stop"
        throw IllegalStateException("speaker stop failed")
    }

    override fun close() = Unit
}
