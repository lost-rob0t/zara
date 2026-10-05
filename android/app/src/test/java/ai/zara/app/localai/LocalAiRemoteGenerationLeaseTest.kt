package ai.zara.app.localai

import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicInteger
import kotlin.concurrent.thread
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class LocalAiRemoteGenerationLeaseTest {
    @Test
    fun callerDeathCancelsOnceUnlinksOnceAndFencesWork() {
        val cancels = AtomicInteger(0)
        val unlinks = AtomicInteger(0)
        val lease = LocalAiRemoteGenerationLease(
            cancel = { cancels.incrementAndGet() },
            unlink = { unlinks.incrementAndGet() },
        )

        assertTrue(lease.runIfActive { true } ?: false)
        assertTrue(lease.callerDied())
        assertFalse(lease.callerDied())
        assertEquals(1, cancels.get())
        assertEquals(1, unlinks.get())
        assertEquals(null, lease.runIfActive { true })
        assertFalse(lease.finish())
    }

    @Test
    fun normalTerminalPathUnlinksWithoutCancelling() {
        val cancels = AtomicInteger(0)
        val unlinks = AtomicInteger(0)
        val lease = LocalAiRemoteGenerationLease(
            cancel = { cancels.incrementAndGet() },
            unlink = { unlinks.incrementAndGet() },
        )

        assertTrue(lease.finish())
        assertFalse(lease.finish())
        assertFalse(lease.callerDied())
        assertEquals(0, cancels.get())
        assertEquals(1, unlinks.get())
    }

    @Test
    fun acceptedStartIsOrderedBeforeCallerDeathCancellation() {
        val started = CountDownLatch(1)
        val releaseStart = CountDownLatch(1)
        val deathFinished = CountDownLatch(1)
        val cancelObservedStart = AtomicBoolean(false)
        val lease = LocalAiRemoteGenerationLease(
            cancel = { cancelObservedStart.set(started.count == 0L) },
            unlink = {},
        )

        val startThread = thread(start = true, name = "remote-generation-start") {
            lease.runIfActive {
                started.countDown()
                assertTrue(releaseStart.await(2, TimeUnit.SECONDS))
                true
            }
        }
        assertTrue(started.await(2, TimeUnit.SECONDS))

        val deathThread = thread(start = true, name = "remote-generation-death") {
            lease.callerDied()
            deathFinished.countDown()
        }
        assertFalse(
            "caller death must serialize behind an accepted generation start",
            deathFinished.await(100, TimeUnit.MILLISECONDS),
        )
        releaseStart.countDown()
        assertTrue(deathFinished.await(2, TimeUnit.SECONDS))
        assertTrue(cancelObservedStart.get())

        startThread.join(2_000)
        deathThread.join(2_000)
    }
}
