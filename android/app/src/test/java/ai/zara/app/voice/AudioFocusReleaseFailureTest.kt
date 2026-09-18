package ai.zara.app.voice

import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import org.junit.Assert.assertFalse
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
        assertFalse(controller.isHeld())
    }

    @Test
    fun `acquire cannot claim focus while release is in flight`() {
        val platform = BlockingAbandonFocusPlatform()
        val controller = AudioFocusController(platform) { }

        assertTrue(controller.acquire())
        val releaseThread = Thread(controller::release)
        releaseThread.start()

        assertTrue(platform.abandonStarted.await(2, TimeUnit.SECONDS))
        assertThrows(IllegalStateException::class.java) { controller.acquire() }

        platform.allowAbandon.countDown()
        releaseThread.join(2_000)
        assertFalse(releaseThread.isAlive)
        assertFalse(controller.isHeld())
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

    private class BlockingAbandonFocusPlatform : AudioFocusPlatform {
        val abandonStarted = CountDownLatch(1)
        val allowAbandon = CountDownLatch(1)

        override fun request(onLoss: (AudioFocusLoss) -> Unit): Boolean = true

        override fun abandon() {
            abandonStarted.countDown()
            check(allowAbandon.await(2, TimeUnit.SECONDS)) { "synthetic abandon timeout" }
        }
    }
}
