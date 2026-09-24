package ai.zara.app.peer

private const val MAX_ENDPOINTS = 8
private const val MAX_ENDPOINT_CHARS = 256
private const val MAX_FAILURE_CHARS = 256
private const val MAX_NODE_ID_CHARS = 128
private val NODE_ID = Regex("[A-Za-z0-9][A-Za-z0-9._:-]{0,127}")

data class PeerNodeIdentity(
    val nodeId: String,
    val curvePublicKeyZ85: String,
    val enrollmentGeneration: Long,
) {
    init {
        require(nodeId.length <= MAX_NODE_ID_CHARS && NODE_ID.matches(nodeId)) {
            "node id must use the bounded canonical peer-id grammar"
        }
        require(curvePublicKeyZ85.length == 40 && curvePublicKeyZ85.none(Char::isWhitespace)) {
            "CURVE public key must be one 40-character Z85 value"
        }
        require(enrollmentGeneration > 0) {
            "enrollment generation must be positive"
        }
    }
}

enum class PeerListenerPhase {
    STOPPED,
    STARTING,
    ACTIVE,
    STOPPING,
    FAILED,
}

data class PeerListenerSnapshot(
    val generation: Long,
    val phase: PeerListenerPhase,
    val identity: PeerNodeIdentity?,
    val endpoints: List<String>,
    val failure: String?,
)

/**
 * Pure lifecycle authority for Android's future ZARA/1 peer listener service.
 *
 * It deliberately owns no socket, wake lock, discovery, trust, or authorization.
 * Service callbacks must present the generation they were started under; stale
 * callbacks cannot resurrect a listener after stop/restart or publish old network
 * endpoints. The durable CURVE/node identity is supplied by the existing auth
 * authority and cannot rotate inside this lifecycle.
 */
class PeerNodeListenerLifecycle(
    restoredIdentity: PeerNodeIdentity? = null,
) {
    private var current = PeerListenerSnapshot(
        generation = 0,
        phase = PeerListenerPhase.STOPPED,
        identity = restoredIdentity,
        endpoints = emptyList(),
        failure = null,
    )

    @Synchronized
    fun snapshot(): PeerListenerSnapshot = current.copy(endpoints = current.endpoints.toList())

    @Synchronized
    fun requestStart(identity: PeerNodeIdentity): Long {
        val established = current.identity
        if (established != null && established != identity) {
            throw IllegalStateException("peer listener identity rotation requires enrollment authority")
        }

        return when (current.phase) {
            PeerListenerPhase.STARTING,
            PeerListenerPhase.ACTIVE,
            -> current.generation

            PeerListenerPhase.STOPPING ->
                throw IllegalStateException("peer listener cannot start while stop is in progress")

            PeerListenerPhase.STOPPED,
            PeerListenerPhase.FAILED,
            -> {
                val generation = nextGeneration()
                current = PeerListenerSnapshot(
                    generation = generation,
                    phase = PeerListenerPhase.STARTING,
                    identity = established ?: identity,
                    endpoints = emptyList(),
                    failure = null,
                )
                generation
            }
        }
    }

    @Synchronized
    fun listenerStarted(generation: Long, endpoints: List<String>): Boolean {
        if (generation != current.generation || current.phase != PeerListenerPhase.STARTING) {
            return false
        }
        val normalized = validateEndpoints(endpoints, requireNonEmpty = true)
        current = current.copy(
            phase = PeerListenerPhase.ACTIVE,
            endpoints = normalized,
            failure = null,
        )
        return true
    }

    @Synchronized
    fun networkChanged(generation: Long, endpoints: List<String>): Boolean {
        if (generation != current.generation || current.phase != PeerListenerPhase.ACTIVE) {
            return false
        }
        current = current.copy(endpoints = validateEndpoints(endpoints, requireNonEmpty = true))
        return true
    }

    @Synchronized
    fun requestStop(): Long? {
        return when (current.phase) {
            PeerListenerPhase.STOPPED -> null
            PeerListenerPhase.STOPPING -> current.generation
            PeerListenerPhase.STARTING,
            PeerListenerPhase.ACTIVE,
            PeerListenerPhase.FAILED,
            -> {
                val generation = nextGeneration()
                current = current.copy(
                    generation = generation,
                    phase = PeerListenerPhase.STOPPING,
                    endpoints = emptyList(),
                    failure = null,
                )
                generation
            }
        }
    }

    @Synchronized
    fun listenerStopped(generation: Long): Boolean {
        if (generation != current.generation || current.phase != PeerListenerPhase.STOPPING) {
            return false
        }
        current = current.copy(
            phase = PeerListenerPhase.STOPPED,
            endpoints = emptyList(),
            failure = null,
        )
        return true
    }

    @Synchronized
    fun listenerFailed(generation: Long, reason: String): Boolean {
        if (
            generation != current.generation ||
            current.phase !in setOf(PeerListenerPhase.STARTING, PeerListenerPhase.ACTIVE)
        ) {
            return false
        }
        require(reason.isNotBlank() && reason.length <= MAX_FAILURE_CHARS) {
            "listener failure must be bounded and non-blank"
        }
        current = current.copy(
            phase = PeerListenerPhase.FAILED,
            endpoints = emptyList(),
            failure = reason,
        )
        return true
    }

    private fun nextGeneration(): Long {
        check(current.generation < Long.MAX_VALUE) { "peer listener generation exhausted" }
        return current.generation + 1
    }

    private fun validateEndpoints(endpoints: List<String>, requireNonEmpty: Boolean): List<String> {
        require(endpoints.size <= MAX_ENDPOINTS) { "too many peer endpoints" }
        if (requireNonEmpty) {
            require(endpoints.isNotEmpty()) { "active peer listener requires an endpoint" }
        }

        val normalized = LinkedHashSet<String>(endpoints.size)
        endpoints.forEach { endpoint ->
            require(endpoint.length in 1..MAX_ENDPOINT_CHARS) { "peer endpoint is out of bounds" }
            require(endpoint.startsWith("tcp://")) { "peer listener supports only ZARA/1 TCP endpoints" }
            require(endpoint.none(Char::isWhitespace)) { "peer endpoint cannot contain whitespace" }
            require(normalized.add(endpoint)) { "duplicate peer endpoint" }
        }
        return normalized.toList()
    }
}
