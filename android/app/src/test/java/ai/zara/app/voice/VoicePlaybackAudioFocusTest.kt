package ai.zara.app.voice

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertThrows
import org.junit.Test

class VoicePlaybackAudioFocusTest {
    @Test
    fun `playback refuses to start when assistant audio focus is denied`() {
        val sink = FocusRecordingOutput()
        val platform = PlaybackFocusPlatform(granted = false)
        val focus = AudioFocusController(platform) { }
        val controller = VoicePlaybackController(sink, "session-1", focus)

        assertThrows(IllegalStateException::class.java) {
            controller.accept(started())
        }

        assertEquals(emptyList<String>(), sink.calls)
        assertFalse(focus.isHeld())
        assertEquals(0, platform.abandons)
    }

    @Test
    fun `normal audio completion releases focus exactly once`() {
        val sink = FocusRecordingOutput()
        val platform = PlaybackFocusPlatform(granted = true)
        val focus = AudioFocusController(platform) { }
        val controller = VoicePlaybackController(sink, "session-1", focus)

        controller.accept(started())
        controller.accept(VoiceStreamEvent.AudioDone("session-1", "turn-1", "speaker-1"))

        assertEquals(listOf("start", "stop"), sink.calls)
        assertEquals(1, platform.requests)
        assertEquals(1, platform.abandons)
    }

    @Test
    fun `failed speaker start abandons already granted focus`() {
        val sink = FocusRecordingOutput(failStart = true)
        val platform = PlaybackFocusPlatform(granted = true)
        val focus = AudioFocusController(platform) { }
        val controller = VoicePlaybackController(sink, "session-1", focus)

        assertThrows(IllegalStateException::class.java) {
            controller.accept(started())
        }

        assertFalse(focus.isHeld())
        assertEquals(1, platform.abandons)
    }

    @Test
    fun `failed PCM write stops output and abandons focus`() {
        val sink = FocusRecordingOutput(failWrite = true)
        val platform = PlaybackFocusPlatform(granted = true)
        val focus = AudioFocusController(platform) { }
        val controller = VoicePlaybackController(sink, "session-1", focus)
        controller.accept(started())

        assertThrows(IllegalStateException::class.java) {
            controller.accept(
                VoiceStreamEvent.AudioChunk(
                    "session-1",
                    "turn-1",
                    "speaker-1",
                    0,
                    byteArrayOf(1, 0),
                )
            )
        }

        assertFalse(focus.isHeld())
        assertEquals(1, platform.abandons)
        assertEquals(listOf("start", "write", "stop"), sink.calls)
        assertEquals(null, controller.state().audio)
    }

    @Test
    fun `system focus loss interrupts the single playback owner`() {
        val sink = FocusRecordingOutput()
        val platform = PlaybackFocusPlatform(granted = true)
        lateinit var controller: VoicePlaybackController
        val focus = AudioFocusController(platform) { controller.interrupt() }
        controller = VoicePlaybackController(sink, "session-1", focus)
        controller.accept(started())

        platform.lose(AudioFocusLoss.Transient)

        assertEquals(listOf("start", "stop"), sink.calls)
        assertEquals(null, controller.state().audio)
        assertThrows(StaleVoiceStreamException::class.java) {
            controller.accept(
                VoiceStreamEvent.AudioChunk(
                    "session-1",
                    "turn-1",
                    "speaker-1",
                    0,
                    byteArrayOf(1, 0),
                )
            )
        }
    }

    private fun started() =
        VoiceStreamEvent.AudioStarted("session-1", "turn-1", "speaker-1", 24_000, 1)

    private class PlaybackFocusPlatform(private val granted: Boolean) : AudioFocusPlatform {
        var requests = 0
        var abandons = 0
        private var listener: ((AudioFocusLoss) -> Unit)? = null

        override fun request(onLoss: (AudioFocusLoss) -> Unit): Boolean {
            requests += 1
            listener = if (granted) onLoss else null
            return granted
        }

        override fun abandon() {
            abandons += 1
            listener = null
        }

        fun lose(loss: AudioFocusLoss) {
            val current = listener
            listener = null
            current?.invoke(loss)
        }
    }

    private class FocusRecordingOutput(
        private val failStart: Boolean = false,
        private val failWrite: Boolean = false,
    ) : PcmOutput {
        val calls = mutableListOf<String>()

        override fun start(sampleRate: Int, channels: Int) {
            if (failStart) throw IllegalStateException("speaker start failed")
            calls += "start"
        }

        override fun write(pcm: ByteArray) {
            calls += "write"
            if (failWrite) throw IllegalStateException("speaker write failed")
        }

        override fun stop() {
            calls += "stop"
        }

        override fun close() = Unit
    }
}
