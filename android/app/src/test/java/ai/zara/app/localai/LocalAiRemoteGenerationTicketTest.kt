package ai.zara.app.localai

import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import kotlin.concurrent.thread
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

class LocalAiRemoteGenerationTicketTest {
    @Test
    fun terminateFencesLateChunkDeliveryAndIsIdempotent() {
        val ticket = LocalAiRemoteGenerationTicket()
        var delivered = false

        assertTrue(ticket.deliver { delivered = true })
        assertTrue(delivered)
        assertTrue(ticket.terminate())
        assertFalse(ticket.terminate())

        delivered = false
        assertFalse(ticket.deliver { delivered = true })
        assertFalse(delivered)
    }

    @Test
    fun terminateWaitsForInFlightDeliveryBeforeReturning() {
        val ticket = LocalAiRemoteGenerationTicket()
        val deliveryStarted = CountDownLatch(1)
        val releaseDelivery = CountDownLatch(1)
        val deliveryFinished = CountDownLatch(1)
        val terminateFinished = CountDownLatch(1)
        val terminateResult = AtomicBoolean(false)

        val deliveryThread = thread(start = true, name = "remote-generation-delivery") {
            try {
                assertTrue(
                    ticket.deliver {
                        deliveryStarted.countDown()
                        assertTrue(releaseDelivery.await(2, TimeUnit.SECONDS))
                    }
                )
            } finally {
                deliveryFinished.countDown()
            }
        }

        assertTrue(deliveryStarted.await(2, TimeUnit.SECONDS))
        val terminateThread = thread(start = true, name = "remote-generation-terminate") {
            terminateResult.set(ticket.terminate())
            terminateFinished.countDown()
        }

        assertFalse(
            "termination must not return while a chunk callback is still in flight",
            terminateFinished.await(100, TimeUnit.MILLISECONDS),
        )
        releaseDelivery.countDown()
        assertTrue(deliveryFinished.await(2, TimeUnit.SECONDS))
        assertTrue(terminateFinished.await(2, TimeUnit.SECONDS))
        assertTrue(terminateResult.get())
        assertFalse(ticket.deliver { error("late chunk escaped cancellation fence") })

        deliveryThread.join(2_000)
        terminateThread.join(2_000)
    }

    @Test
    fun callbackFailureTerminatesTicketAndFencesLateDelivery() {
        val ticket = LocalAiRemoteGenerationTicket()
        try {
            ticket.deliver { error("socket closed") }
            fail("chunk consumer failure must escape to the remote client")
        } catch (_: IllegalStateException) {
        }

        var lateDelivery = false
        assertFalse(ticket.deliver { lateDelivery = true })
        assertFalse(lateDelivery)
        assertFalse(ticket.terminate())
    }
}
