package ai.zara.app.ui

import ai.zara.app.auth.AuthenticationException
import ai.zara.app.runtime.TextRequestTimeoutException
import ai.zara.app.runtime.ZaraWireException
import java.io.IOException
import java.util.Collections
import java.util.IdentityHashMap

object UiOperationFailure {
    private const val MAX_CAUSE_DEPTH = 8

    fun summarize(error: Throwable): String {
        val seen = Collections.newSetFromMap(IdentityHashMap<Throwable, Boolean>())
        var current: Throwable? = error
        var depth = 0

        while (current != null && depth < MAX_CAUSE_DEPTH && seen.add(current)) {
            when (current) {
                is SecurityException -> return "permission_denied"
                is AuthenticationException -> return "authentication_failed"
                is TextRequestTimeoutException -> return when (current.message) {
                    "ZARA/1 voice hello timed out" -> "server_hello_timeout"
                    "ZARA/1 capability negotiation timed out" -> "capability_negotiation_timeout"
                    else -> "request_timeout"
                }
                is ZaraWireException -> return "protocol_error"
                is IllegalArgumentException -> return "invalid_input"
                is IOException -> return "network_error"
            }
            current = current.cause
            depth += 1
        }

        return "operation_failed"
    }
}
