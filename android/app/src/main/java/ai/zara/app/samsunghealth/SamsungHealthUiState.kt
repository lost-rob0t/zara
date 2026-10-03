package ai.zara.app.samsunghealth

import android.app.Activity
import java.util.concurrent.CompletableFuture

data class SamsungHealthUiState(
    val availability: SamsungHealthAvailability? = null,
    val supportedMetrics: Set<SamsungHealthMetric> = emptySet(),
    val grantedMetrics: Set<SamsungHealthMetric> = emptySet(),
    val readings: Map<SamsungHealthMetric, SamsungHealthReading> = emptyMap(),
    val busyMetric: SamsungHealthMetric? = null,
    val refreshing: Boolean = false,
    val error: String? = null,
) {
    val ready: Boolean
        get() = availability == SamsungHealthAvailability.READY
}

class SamsungHealthUiController(
    private val plugin: SamsungHealthAndroidPlugin,
    private val publish: (SamsungHealthUiState) -> Unit,
) : AutoCloseable {
    private val lock = Any()
    private var generation = 0L
    private var closed = false
    private var stateValue = SamsungHealthUiState(supportedMetrics = plugin.supportedMetrics())

    fun state(): SamsungHealthUiState = synchronized(lock) { stateValue }

    fun refresh(): CompletableFuture<SamsungHealthUiState> {
        val token = begin { it.copy(refreshing = true, error = null) }
        return plugin.status().thenCompose { status ->
            val availability = status.status?.availability ?: SamsungHealthAvailability.ERROR
            if (availability != SamsungHealthAvailability.READY) {
                CompletableFuture.completedFuture(
                    state().copy(
                        availability = availability,
                        supportedMetrics = plugin.supportedMetrics(),
                        grantedMetrics = emptySet(),
                        refreshing = false,
                    ),
                )
            } else {
                plugin.permissions().thenApply { permissions ->
                    state().copy(
                        availability = availability,
                        supportedMetrics = plugin.supportedMetrics(),
                        grantedMetrics = permissions.grantedPermissions.orEmpty(),
                        refreshing = false,
                    )
                }
            }
        }.publishIfCurrent(token)
    }

    fun read(metric: SamsungHealthMetric): CompletableFuture<SamsungHealthUiState> {
        val token = begin { current ->
            require(current.ready) { "Samsung Health is not ready" }
            require(metric in current.supportedMetrics) { "Samsung Health metric is unsupported" }
            require(metric in current.grantedMetrics) { "Samsung Health permission is required" }
            require(current.busyMetric == null) { "Another Samsung Health read is active" }
            current.copy(busyMetric = metric, error = null)
        }
        return plugin.today(metric).thenApply { reply ->
            val reading = requireNotNull(reply.reading) { "Samsung Health returned no reading" }
            state().copy(
                readings = state().readings + (metric to reading),
                busyMetric = null,
            )
        }.publishIfCurrent(token)
    }

    fun requestPermissions(
        activity: Activity,
        metrics: Set<SamsungHealthMetric>,
    ): CompletableFuture<SamsungHealthUiState> {
        val selected = metrics.intersect(plugin.supportedMetrics())
        require(selected.isNotEmpty()) { "No supported Samsung Health permissions selected" }
        val token = begin { it.copy(refreshing = true, error = null) }
        return plugin.requestPermissions(activity, selected).thenApply { granted ->
            state().copy(
                grantedMetrics = granted,
                refreshing = false,
            )
        }.publishIfCurrent(token)
    }

    private fun begin(change: (SamsungHealthUiState) -> SamsungHealthUiState): Long {
        val next: SamsungHealthUiState
        val token: Long
        synchronized(lock) {
            check(!closed) { "Samsung Health UI controller is closed" }
            generation += 1
            token = generation
            next = change(stateValue)
            stateValue = next
        }
        publish(next)
        return token
    }

    private fun CompletableFuture<SamsungHealthUiState>.publishIfCurrent(
        token: Long,
    ): CompletableFuture<SamsungHealthUiState> {
        val result = CompletableFuture<SamsungHealthUiState>()
        whenComplete { value, failure ->
            val next: SamsungHealthUiState?
            synchronized(lock) {
                if (closed || generation != token) {
                    next = null
                } else if (failure == null) {
                    stateValue = value
                    next = value
                } else {
                    stateValue = stateValue.copy(
                        busyMetric = null,
                        refreshing = false,
                        error = "Samsung Health request failed.",
                    )
                    next = stateValue
                }
            }
            if (next == null) {
                result.completeExceptionally(IllegalStateException("Stale Samsung Health result discarded"))
            } else {
                publish(next)
                if (failure == null) result.complete(next) else result.completeExceptionally(failure)
            }
        }
        return result
    }

    override fun close() {
        synchronized(lock) {
            if (closed) return
            closed = true
            generation += 1
        }
    }
}
