package ai.zara.app.voice

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNull
import org.junit.Assert.assertThrows
import org.junit.Test

class VoicePlaybackStartFailurePrecedenceTest {
    @Test
    fun speakerStartFailureStaysPrimaryWhenFocusAbandonAlsoFails() {
        val focusPlatform = StartFailureFocusPlatform()
        val focus = AudioFocusController(focusPlatform) { }
        val controller = VoicePlaybackController(StartFailingOutput(), "session-1", focus)

        val error = assertThrows(IllegalStateException::class.java) {
            controller.accept(
                VoiceStreamEvent.AudioStarted("session-1", "turn-1", "speaker-1", 24_000, 1),
            )
        }

        assertEquals("speaker start failed", error.message)
        assertEquals(1, error.suppressed.size)
        assertEquals("focus abandon failed", error.suppressed.single().message)
        assertNull(controller.state().audio)
        assertFalse(focus.isHeld())
    }
}

private class StartFailingOutput : PcmOutput {
    override fun start(sampleRate: Int, channels: Int) {
        throw IllegalStateException("speaker start failed")
    }

    override fun write(pcm: ByteArray) = Unit
    override fun stop() = Unit
    override fun close() = Unit
}

private class StartFailureFocusPlatform : AudioFocusPlatform {
    override fun request(onLoss: (AudioFocusLoss) -> Unit): Boolean = true

    override fun abandon() {
        throw IllegalStateException("focus abandon failed")
    }
}
