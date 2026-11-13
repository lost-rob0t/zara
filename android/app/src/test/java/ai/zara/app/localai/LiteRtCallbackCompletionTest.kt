package ai.zara.app.localai

import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.CountDownLatch
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class LiteRtCallbackCompletionTest {
    @Test
    fun terminalCallbackReturnsBeforeNativeCloseAndNotifiesAfterClose() {
        val closer = Executors.newSingleThreadExecutor { runnable -> Thread(runnable, "litert-close") }
        val finishEntered = CountDownLatch(1)
        val allowFinish = CountDownLatch(1)
        val notified = CountDownLatch(1)
        val events = CopyOnWriteArrayList<String>()
        val terminal = AtomicBoolean(false)
        try {
            val callbackThread = Thread({
                dispatchLiteRtCompletion(terminal, closer, {
                    assertEquals("litert-close", Thread.currentThread().name)
                    events += "close"
                    finishEntered.countDown()
                    assertTrue(allowFinish.await(2, TimeUnit.SECONDS))
                }) {
                    events += "notify"
                    notified.countDown()
                }
                dispatchLiteRtCompletion(terminal, closer, { events += "duplicate close" }) {
                    events += "duplicate notify"
                }
            }, "litert-callback")
            callbackThread.start()
            assertTrue(finishEntered.await(2, TimeUnit.SECONDS))
            callbackThread.join(500)
            assertFalse("LiteRT callback must not wait for native close", callbackThread.isAlive)
            assertEquals(listOf("close"), events)
            allowFinish.countDown()
            assertTrue(notified.await(2, TimeUnit.SECONDS))
            assertEquals(listOf("close", "notify"), events)
        } finally {
            allowFinish.countDown()
            closer.shutdownNow()
        }
    }
}
