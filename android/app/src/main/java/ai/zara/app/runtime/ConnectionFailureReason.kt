package ai.zara.app.runtime

import java.io.IOException
import java.util.Collections
import java.util.IdentityHashMap

object ConnectionFailureReason {
    private const val MAX_CAUSE_DEPTH = 8

    fun summarize(error: Throwable?): String {
        if (error == null) return "connection_failed"

        val seen = Collections.newSetFromMap(IdentityHashMap<Throwable, Boolean>())
        var current: Throwable? = error
        var depth = 0
        while (current != null && depth < MAX_CAUSE_DEPTH && seen.add(current)) {
            when (current) {
                is SecurityException -> return "permission_denied"
                is IOException -> return "network_error"
            }
            current = current.cause
            depth += 1
        }
        return "connection_failed"
    }
}
