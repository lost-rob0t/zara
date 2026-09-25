package ai.zara.app.ui

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class TurnRetryPolicyTest {
    @Test fun `retry budget is two total attempts`() {
        assertEquals(2, TurnRetryPolicy.MAX_ATTEMPTS)
    }

    @Test fun `first connected retryable failure auto retries exactly once`() {
        val failure = retryableFailure(connected = true)
        assertTrue(TurnRetryPolicy.shouldAutoRetry(failure, attempt = 1))
        assertFalse(TurnRetryPolicy.shouldAutoRetry(failure, attempt = 2))
    }

    @Test fun `disconnected failure requires reconnect instead of burning automatic retry`() {
        val failure = retryableFailure(connected = false)
        assertFalse(TurnRetryPolicy.shouldAutoRetry(failure, attempt = 1))
        assertTrue(TurnRetryPolicy.decorate(failure, attempt = 1).retryPossible)
    }

    @Test fun `cancel and stale generation never auto retry`() {
        val cancelled = retryableFailure(connected = true).copy(code = "protocol.turn_cancelled")
        val stale = retryableFailure(connected = true).copy(code = "protocol.stale_generation")
        assertFalse(TurnRetryPolicy.shouldAutoRetry(cancelled, attempt = 1))
        assertFalse(TurnRetryPolicy.shouldAutoRetry(stale, attempt = 1))
    }

    @Test fun `retry is blocked until canonical terminal state is durably persisted`() {
        val unresolved = retryableFailure(connected = true).copy(terminalPersisted = false)

        assertFalse(TurnRetryPolicy.shouldAutoRetry(unresolved, attempt = 1))
        assertFalse(TurnRetryPolicy.canManualRetry(unresolved))
    }

    @Test fun `final attempt hides retry and reports exhausted budget`() {
        val failure = TurnRetryPolicy.decorate(retryableFailure(connected = true), attempt = 2)
        assertEquals(2, failure.attempt)
        assertEquals(2, failure.maxAttempts)
        assertFalse(failure.retryPossible)
        assertEquals("Attempt 2/2", retryStatusLabel(failure))
    }

    @Test fun `automatic retry state is visible and not manually clickable`() {
        val retrying = TurnRetryPolicy.decorate(
            retryableFailure(connected = true),
            attempt = 2,
            autoRetrying = true,
        )
        assertTrue(retrying.autoRetrying)
        assertFalse(retrying.retryPossible)
        assertEquals("Retrying automatically — attempt 2/2", retryStatusLabel(retrying))
    }

    private fun retryableFailure(connected: Boolean): TurnFailure = TurnFailure(
        title = "Retryable",
        explanation = "Retryable failure",
        subsystem = "transport",
        operation = "submit",
        code = "transport.timeout",
        connectionState = if (connected) "connected" else "disconnected",
        recovery = "retryable",
        incidentId = null,
        retryPossible = true,
        reconnectPossible = !connected,
    )
}
