package ai.zara.app.voice

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class AudioFocusControllerTest {
    @Test
    fun `granted focus becomes held and releases exactly once`() {
        val platform = FakeAudioFocusPlatform(granted = true)
        val losses = mutableListOf<AudioFocusLoss>()
        val controller = AudioFocusController(platform, losses::add)

        assertTrue(controller.acquire())
        assertTrue(controller.isHeld())
        controller.release()
        controller.release()

        assertFalse(controller.isHeld())
        assertEquals(1, platform.requests)
        assertEquals(1, platform.abandons)
        assertEquals(emptyList<AudioFocusLoss>(), losses)
    }

    @Test
    fun `denied focus never becomes held or abandons`() {
        val platform = FakeAudioFocusPlatform(granted = false)
        val controller = AudioFocusController(platform) { }

        assertFalse(controller.acquire())
        assertFalse(controller.isHeld())
        controller.release()

        assertEquals(1, platform.requests)
        assertEquals(0, platform.abandons)
    }

    @Test
    fun `focus loss clears ownership before notifying playback owner`() {
        val platform = FakeAudioFocusPlatform(granted = true)
        val observedHeld = mutableListOf<Boolean>()
        lateinit var controller: AudioFocusController
        controller = AudioFocusController(platform) {
            observedHeld += controller.isHeld()
        }

        assertTrue(controller.acquire())
        platform.lose(AudioFocusLoss.Transient)

        assertEquals(listOf(false), observedHeld)
        assertFalse(controller.isHeld())
        assertEquals(0, platform.abandons)
    }

    @Test
    fun `second acquire while held does not ask platform twice`() {
        val platform = FakeAudioFocusPlatform(granted = true)
        val controller = AudioFocusController(platform) { }

        assertTrue(controller.acquire())
        assertTrue(controller.acquire())

        assertEquals(1, platform.requests)
    }

    private class FakeAudioFocusPlatform(
        private val granted: Boolean,
    ) : AudioFocusPlatform {
        var requests = 0
        var abandons = 0
        private var listener: ((AudioFocusLoss) -> Unit)? = null

        override fun request(onLoss: (AudioFocusLoss) -> Unit): Boolean {
            requests += 1
            listener = onLoss
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
}
