package ai.zara.app.telemetry

data class FailureIncident(
    val failure: ZaraFailure,
    val firstSeenMillis: Long,
    val lastSeenMillis: Long,
    val lastSuccess: String?,
)

class FailureIncidentTracker(
    private val clock: () -> Long = System::currentTimeMillis,
) {
    private var incident: FailureIncident? = null
    private var lastSuccess: String? = null
    private val lock = Any()

    fun record(failure: ZaraFailure): FailureIncident? = synchronized(lock) {
        val current = incident
        if (
            current != null &&
            failure.connectionGeneration != null &&
            current.failure.connectionGeneration != null &&
            failure.connectionGeneration < current.failure.connectionGeneration!!
        ) {
            return null
        }
        val now = clock()
        val updated = if (
            current != null &&
            failure.connectionGeneration == current.failure.connectionGeneration &&
            failure.code == current.failure.code
        ) {
            current.copy(
                failure = failure,
                lastSeenMillis = now,
            )
        } else {
            FailureIncident(
                failure = failure,
                firstSeenMillis = now,
                lastSeenMillis = now,
                lastSuccess = lastSuccess,
            )
        }
        incident = updated
        updated
    }

    fun noteSuccess(phase: String) {
        synchronized(lock) {
            lastSuccess = phase
        }
    }

    fun primary(): FailureIncident? = synchronized(lock) { incident }

    fun clear() = synchronized(lock) {
        incident = null
        lastSuccess = null
    }
}
