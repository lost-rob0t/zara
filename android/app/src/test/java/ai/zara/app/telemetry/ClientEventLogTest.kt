package ai.zara.app.telemetry

import java.util.concurrent.CountDownLatch
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class ClientEventLogTest {

    @Test fun `events receive strictly monotonic sequences and ordered snapshots`() {
        val log = ClientEventLog()
        log.record(ClientEventNames.REMOTE_CONNECT_BEGIN, operation = ZaraOperation.CONNECT)
        log.record(ClientEventNames.REMOTE_CONNECT_READY, operation = ZaraOperation.CONNECT)
        log.record(ClientEventNames.REMOTE_DISCONNECTED, code = ZaraFailureCodes.TRANSPORT_CLOSED)

        val events = log.snapshot()
        assertEquals(3, events.size)
        assertEquals(listOf(1L, 2L, 3L), events.map { it.sequence })
        assertEquals(ClientEventNames.REMOTE_CONNECT_BEGIN, events[0].name)
        assertEquals(ClientEventNames.REMOTE_DISCONNECTED, events[2].name)
        assertEquals(ZaraFailureCodes.TRANSPORT_CLOSED, events[2].code)
        assertTrue(events[0].wallTimeMillis > 0)
        assertTrue(events[0].monotonicNanos >= 0)
    }

    @Test fun `ring keeps only the newest bounded window`() {
        val log = ClientEventLog(capacity = 8)
        repeat(20) { index ->
            log.recordProtocolMessage(
                direction = ClientEventLog.Direction.RX,
                messageType = "assistant.delta",
                messageSequence = null,
                messageBytes = index.toLong(),
            )
        }
        val events = log.snapshot()
        assertEquals(8, events.size)
        assertEquals(12L, events.first().messageBytes)
        assertEquals(19L, events.last().messageBytes)
        assertEquals(listOf(13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L), events.map { it.sequence })
    }

    @Test fun `messages are bounded and secrets are redacted`() {
        val log = ClientEventLog()
        log.record(
            ClientEventNames.PROTOCOL_FAILED,
            code = ZaraFailureCodes.PROTOCOL_MALFORMED,
            message = "token=super-secret leaked then " + "x".repeat(2_000),
        )
        val event = log.snapshot().single()
        assertFalse(event.message.orEmpty().contains("super-secret"))
        assertTrue(event.message.orEmpty().contains("token=<redacted>"))
        assertTrue(event.message.orEmpty().length <= 512)
    }

    @Test fun `protocol message events carry metadata only without content`() {
        val log = ClientEventLog()
        log.recordProtocolMessage(
            direction = ClientEventLog.Direction.RX,
            messageType = "assistant.delta",
            messageSequence = 4,
            messageBytes = 128,
            connectionGeneration = 2,
        )
        val event = log.snapshot().single()
        assertEquals(ClientEventNames.PROTOCOL_MESSAGE_RX, event.name)
        assertEquals("assistant.delta", event.messageType)
        assertEquals(4L, event.messageSequence)
        assertEquals(128L, event.messageBytes)
        assertEquals(2L, event.connectionGeneration)
        assertEquals(null, event.message)
    }

    @Test fun `unknown event names are rejected`() {
        val log = ClientEventLog()
        val error = runCatching { log.record("totally.made.up") }.exceptionOrNull()
        assertTrue(error is IllegalArgumentException)
    }

    @Test fun `concurrent recording never duplicates or reorders sequences`() {
        val log = ClientEventLog(capacity = 64)
        val threads = 4
        val perThread = 100
        val ready = CountDownLatch(threads)
        val done = CountDownLatch(threads)
        val pool = Executors.newFixedThreadPool(threads)
        repeat(threads) {
            pool.submit {
                ready.countDown()
                ready.await()
                repeat(perThread) { log.record(ClientEventNames.PROTOCOL_MESSAGE_TX) }
                done.countDown()
            }
        }
        check(done.await(5, TimeUnit.SECONDS))
        pool.shutdown()
        val sequences = log.snapshot().map { it.sequence }
        assertEquals(sequences.size, sequences.distinct().size)
        assertEquals(sequences, sequences.sorted())
    }
}
