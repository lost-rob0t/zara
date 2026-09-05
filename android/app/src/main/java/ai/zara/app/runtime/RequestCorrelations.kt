package ai.zara.app.runtime

enum class CorrelationResult {
    Accepted,
    Stale,
    Unknown,
}

private data class PendingRequest(
    val generation: Long,
    val sessionId: String,
)

class RequestCorrelations(private val limit: Int) {
    private val pending = LinkedHashMap<String, PendingRequest>()

    init {
        require(limit > 0) { "correlation limit must be positive" }
    }

    val size: Int
        get() = pending.size

    fun register(requestId: String, generation: Long, sessionId: String) {
        require(requestId.isNotBlank()) { "request id is required" }
        require(generation > 0) { "generation must be positive" }
        require(sessionId.isNotBlank()) { "session id is required" }
        require(requestId !in pending) { "request id is already pending" }
        check(pending.size < limit) { "pending request limit reached" }
        pending[requestId] = PendingRequest(generation, sessionId)
    }

    fun complete(requestId: String, generation: Long, sessionId: String): CorrelationResult {
        val request = pending[requestId] ?: return CorrelationResult.Unknown
        if (request.generation != generation || request.sessionId != sessionId) {
            return CorrelationResult.Stale
        }
        pending.remove(requestId)
        return CorrelationResult.Accepted
    }

    fun dropGeneration(generation: Long): Int {
        var dropped = 0
        val iterator = pending.entries.iterator()
        while (iterator.hasNext()) {
            if (iterator.next().value.generation == generation) {
                iterator.remove()
                dropped += 1
            }
        }
        return dropped
    }

    fun clear(): Int {
        val count = pending.size
        pending.clear()
        return count
    }
}
