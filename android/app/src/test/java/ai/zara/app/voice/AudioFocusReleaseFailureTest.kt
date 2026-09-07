package ai.zara.app.voice

import org.junit.Assert.assertThrows
import org.junit.Assert.assertTrue
import org.junit.Test

class AudioFocusReleaseFailureTest {
    @Test
    fun `failed abandon preserves focus lease and permits retry`() {
        val platform = FailingAbandonFocusPlatform()
        val controller = AudioFocusController(platform) { }

        assertTrue(controller.acquire())
        platform.failAbandon = true

        assertThrows(IllegalStateException::class.java) { controller.release() }
        assertTrue(controller.isHeld())

        platform.failAbandon = false
        controller.release()

        assertTrue(platform.abandoned)
        assertTrue(!controller.isHeld())
    }

    private class FailingAbandonFocusPlatform : AudioFocusPlatform {
        var failAbandon = false
        var abandoned = false
            private set

        override fun request(onLoss: (AudioFocusLoss) -> Unit): Boolean = true

        override fun abandon() {
            check(!failAbandon) { "synthetic focus abandon failure" }
            abandoned = true
        }
    }
}
