package ai.zara.app.localai

import java.util.concurrent.CountDownLatch
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicReference
import org.junit.Assert.*
import org.junit.Test

class LiteRtLifecycleTest {
    @Test fun nativeCloseRetainsOwnershipAndExcludesUnloadUntilItFinishes() {
        val lock = Any()
        val conversation = Any()
        val current = AtomicReference<Any?>(conversation)
        val entered = CountDownLatch(1)
        val release = CountDownLatch(1)
        val unloadAttempted = CountDownLatch(1)
        val unloaded = CountDownLatch(1)
        val pool = Executors.newFixedThreadPool(2)
        try {
            val close = pool.submit {
                closeOwnedLiteRtConversation(lock, { current.get() === conversation }, {
                    entered.countDown()
                    check(release.await(2, TimeUnit.SECONDS))
                }, { current.set(null) })
            }
            assertTrue(entered.await(2, TimeUnit.SECONDS))
            val unload = pool.submit {
                unloadAttempted.countDown()
                synchronized(lock) { unloaded.countDown() }
            }
            assertTrue(unloadAttempted.await(2, TimeUnit.SECONDS))
            assertSame("Native conversation stays owned until close returns", conversation, current.get())
            assertFalse("Engine unload must wait for native conversation close", unloaded.await(100, TimeUnit.MILLISECONDS))
            release.countDown()
            close.get(2, TimeUnit.SECONDS)
            unload.get(2, TimeUnit.SECONDS)
            assertNull(current.get())
        } finally {
            release.countDown()
            pool.shutdownNow()
        }
    }

    @Test fun staleAndDuplicateCompletionCannotCloseReleasedOrReplacementConversation() {
        val lock = Any()
        val old = Any()
        val current = AtomicReference<Any?>(old)
        val closes = AtomicInteger()
        fun finish() = closeOwnedLiteRtConversation(lock, { current.get() === old },
            { closes.incrementAndGet(); Unit }, { current.set(null) })
        finish()
        finish()
        val replacement = Any()
        current.set(replacement)
        finish()
        assertEquals(1, closes.get())
        assertSame(replacement, current.get())
    }
}
