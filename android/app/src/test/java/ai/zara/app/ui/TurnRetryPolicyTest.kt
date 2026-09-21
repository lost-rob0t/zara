package ai.zara.app.ui

import ai.zara.app.telemetry.ZaraFailure
import ai.zara.app.telemetry.ZaraOperation
import ai.zara.app.telemetry.ZaraRecovery
import ai.zara.app.telemetry.ZaraSubsystem
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

    private fun retryableFailure(connected: Boolean): TurnFailure = TurnFailures.from(
        failure = ZaraFailure(
            code = "transport.timeout",
            subsystem = ZaraSubsystem.TRANSPORT,
            operation = ZaraOperation.SUBMIT,
            recovery = ZaraRecovery.RETRYABLE,
            message = "retry",
            cause = null,
        ),
        transportConnected = connected,
        incidentId = null,
    )
}
