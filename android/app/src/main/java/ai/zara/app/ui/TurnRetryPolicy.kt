package ai.zara.app.ui

import ai.zara.app.telemetry.ZaraFailureCodes

/**
 * UI retry policy for one canonical chat turn.
 *
 * MAX_ATTEMPTS counts the initial execution. A value of 2 therefore permits
 * at most one retry. Retry never changes routing/runtime identity and never
 * reconnects implicitly; disconnected recovery stays an explicit reconnect.
 */
object TurnRetryPolicy {
    const val MAX_ATTEMPTS = 2

    fun decorate(
        failure: TurnFailure,
        attempt: Int,
        autoRetrying: Boolean = false,
    ): TurnFailure {
        require(attempt in 1..MAX_ATTEMPTS) { "retry attempt is outside the bounded budget" }
        val retryAvailable = failure.retryPossible &&
            attempt < MAX_ATTEMPTS &&
            !autoRetrying
        return failure.copy(
            retryPossible = retryAvailable,
            attempt = attempt,
            maxAttempts = MAX_ATTEMPTS,
            autoRetrying = autoRetrying,
        )
    }

    fun shouldAutoRetry(failure: TurnFailure, attempt: Int): Boolean {
        if (attempt >= MAX_ATTEMPTS) return false
        if (!failure.retryPossible || failure.reconnectPossible) return false
        if (failure.code == ZaraFailureCodes.PROTOCOL_TURN_CANCELLED) return false
        if (failure.code == ZaraFailureCodes.PROTOCOL_STALE_GENERATION) return false
        return true
    }

    fun canManualRetry(failure: TurnFailure?): Boolean =
        failure?.retryPossible == true && !failure.autoRetrying
}

fun retryStatusLabel(failure: TurnFailure): String =
    if (failure.autoRetrying) {
        "Retrying automatically — attempt ${failure.attempt}/${failure.maxAttempts}"
    } else {
        "Attempt ${failure.attempt}/${failure.maxAttempts}"
    }
