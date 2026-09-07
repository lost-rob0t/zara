package ai.zara.app.voice

import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class AudioFocusSynchronousLossTest {
    @Test
    fun `focus loss during request cannot leave a phantom held lease`() {
        val platform = SynchronousLossPlatform()
        val losses = mutableListOf<AudioFocusLoss>()
        val controller = AudioFocusController(platform, losses::add)

        assertFalse(controller.acquire())
        assertFalse(controller.isHeld())
        assertTrue(losses.isEmpty())

        platform.loseDuringRequest = false
        assertTrue(controller.acquire())
        assertTrue(controller.isHeld())
    }

    private class SynchronousLossPlatform : AudioFocusPlatform {
        var loseDuringRequest = true

        override fun request(onLoss: (AudioFocusLoss) -> Unit): Boolean {
            if (loseDuringRequest) onLoss(AudioFocusLoss.Transient)
            return true
        }

        override fun abandon() = Unit
    }
}
