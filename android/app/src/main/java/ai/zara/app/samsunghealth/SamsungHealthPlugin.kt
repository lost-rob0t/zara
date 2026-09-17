package ai.zara.app.samsunghealth

import ai.zara.app.prolog.PrologWorkspace
import android.app.Activity
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

enum class SamsungHealthMetric(val atom: String) {
    STEPS("steps"),
    SLEEP("sleep"),
    HEART_RATE("heart_rate"),
    EXERCISE("exercise");

    companion object {
        fun fromAtom(value: String): SamsungHealthMetric? = entries.firstOrNull { it.atom == value }
    }
}

enum class SamsungHealthAvailability {
    SDK_MISSING,
    PLATFORM_NOT_INSTALLED,
    PLATFORM_TOO_OLD,
    PLATFORM_DISABLED,
    PLATFORM_NOT_INITIALIZED,
    AUTHORIZATION_REQUIRED,
    READY,
    ERROR,
}

data class SamsungHealthPlatformStatus(
    val availability: SamsungHealthAvailability,
    val detail: String? = null,
)

data class SamsungHealthReading(
    val metric: SamsungHealthMetric,
    val values: Map<String, String>,
) {
    init {
        require(values.size <= MAX_FIELDS) { "Samsung Health reading has too many fields" }
        var bytes = 0
        values.forEach { (key, value) ->
            require(key.matches(FIELD_NAME)) { "Samsung Health reading field name is invalid" }
            require(value.length <= MAX_FIELD_CHARS) { "Samsung Health reading field is too large" }
            require(value.none { it.code < 0x20 }) { "Samsung Health reading contains control characters" }
            bytes += key.toByteArray(Charsets.UTF_8).size
            bytes += value.toByteArray(Charsets.UTF_8).size
        }
        require(bytes <= MAX_PAYLOAD_BYTES) { "Samsung Health reading is too large" }
    }

    private companion object {
        val FIELD_NAME = Regex("[a-z][a-z0-9_]{0,63}")
        const val MAX_FIELDS = 32
        const val MAX_FIELD_CHARS = 4_096
        const val MAX_PAYLOAD_BYTES = 8 * 1024
    }
}

sealed interface SamsungHealthAction {
    data object Status : SamsungHealthAction
    data object Permissions : SamsungHealthAction
    data class ReadToday(val metric: SamsungHealthMetric) : SamsungHealthAction
    data class LocalError(val code: String, val subject: String) : SamsungHealthAction
}

data class SamsungHealthPluginReply(
    val text: String,
    val success: Boolean,
    val status: SamsungHealthPlatformStatus? = null,
    val reading: SamsungHealthReading? = null,
)

object SamsungHealthPrologCodec {
    private const val MAX_TERM_CHARS = 512
    private val readToday = Regex(
        "^samsung_health_action\\(read_today\\(([a-z][a-z0-9_]{0,31})\\)\\)$",
    )
    private val localError = Regex(
        "^samsung_health_action\\(error\\(([a-z][a-z0-9_]{0,31}),([a-z][a-z0-9_]{0,63})\\)\\)$",
    )

    fun decode(text: String): SamsungHealthAction? {
        val source = text.trim()
        if (!source.startsWith("samsung_health_action")) return null
        require(source.length <= MAX_TERM_CHARS) { "Samsung Health Prolog effect is too large" }
        require(source.none { it.code < 0x20 }) { "Samsung Health Prolog effect contains control characters" }
        return when (source) {
            "samsung_health_action(status)" -> SamsungHealthAction.Status
            "samsung_health_action(permissions)" -> SamsungHealthAction.Permissions
            else -> decodeStructured(source)
        }
    }

    private fun decodeStructured(source: String): SamsungHealthAction {
        readToday.matchEntire(source)?.let { match ->
            val metric = SamsungHealthMetric.fromAtom(match.groupValues[1])
                ?: throw IllegalArgumentException("Unknown Samsung Health metric")
            return SamsungHealthAction.ReadToday(metric)
        }
        localError.matchEntire(source)?.let { match ->
            return SamsungHealthAction.LocalError(match.groupValues[1], match.groupValues[2])
        }
        throw IllegalArgumentException("Malformed Samsung Health Prolog effect")
    }
}

interface SamsungHealthGateway {
    fun status(): SamsungHealthPlatformStatus
    fun grantedPermissions(): Set<SamsungHealthMetric>
    fun requestPermissions(
        activity: Activity,
        metrics: Set<SamsungHealthMetric>,
    ): CompletableFuture<Set<SamsungHealthMetric>>
    fun readToday(metric: SamsungHealthMetric): SamsungHealthReading
}

class UnavailableSamsungHealthGateway(
    private val reason: SamsungHealthAvailability = SamsungHealthAvailability.SDK_MISSING,
) : SamsungHealthGateway {
    override fun status(): SamsungHealthPlatformStatus = SamsungHealthPlatformStatus(reason)

    override fun grantedPermissions(): Set<SamsungHealthMetric> = emptySet()

    override fun requestPermissions(
        activity: Activity,
        metrics: Set<SamsungHealthMetric>,
    ): CompletableFuture<Set<SamsungHealthMetric>> =
        CompletableFuture.failedFuture(IllegalStateException("Samsung Health Data SDK is unavailable"))

    override fun readToday(metric: SamsungHealthMetric): SamsungHealthReading =
        throw IllegalStateException("Samsung Health Data SDK is unavailable")
}

class SamsungHealthPluginActor(
    private val gateway: SamsungHealthGateway,
) : AutoCloseable {
    private val actor: ExecutorService = Executors.newSingleThreadExecutor { runnable ->
        Thread(runnable, "zara-samsung-health").apply { isDaemon = true }
    }
    @Volatile private var closed = false

    fun dispatch(terms: List<String>): CompletableFuture<SamsungHealthPluginReply?> {
        val actions = try {
            terms.mapNotNull(SamsungHealthPrologCodec::decode)
        } catch (error: Throwable) {
            return CompletableFuture.failedFuture(error)
        }
        if (actions.isEmpty()) return CompletableFuture.completedFuture(null)
        if (actions.size != 1) {
            return CompletableFuture.failedFuture(
                IllegalArgumentException("A Prolog turn may emit only one Samsung Health effect"),
            )
        }
        val action = actions.single()
        if (action is SamsungHealthAction.LocalError) {
            return CompletableFuture.completedFuture(localError(action))
        }
        return submit { execute(action) }
    }

    private fun execute(action: SamsungHealthAction): SamsungHealthPluginReply = when (action) {
        SamsungHealthAction.Status -> {
            val status = gateway.status()
            SamsungHealthPluginReply(
                text = statusText(status.availability),
                success = status.availability == SamsungHealthAvailability.READY,
                status = status,
            )
        }
        SamsungHealthAction.Permissions -> {
            val status = gateway.status()
            if (status.availability != SamsungHealthAvailability.READY) {
                SamsungHealthPluginReply(statusText(status.availability), false, status = status)
            } else {
                val granted = gateway.grantedPermissions()
                val names = if (granted.isEmpty()) "none" else granted.sortedBy { it.atom }.joinToString(", ") { it.atom }
                SamsungHealthPluginReply("Samsung Health read permissions: $names", true, status = status)
            }
        }
        is SamsungHealthAction.ReadToday -> readToday(action.metric)
        is SamsungHealthAction.LocalError -> localError(action)
    }

    private fun readToday(metric: SamsungHealthMetric): SamsungHealthPluginReply {
        val status = gateway.status()
        if (status.availability != SamsungHealthAvailability.READY) {
            return SamsungHealthPluginReply(statusText(status.availability), false, status = status)
        }
        val granted = gateway.grantedPermissions()
        if (metric !in granted) {
            return SamsungHealthPluginReply(
                "Samsung Health read permission is required for ${metric.atom}.",
                false,
                status = status,
            )
        }
        val reading = gateway.readToday(metric)
        require(reading.metric == metric) { "Samsung Health gateway returned the wrong metric" }
        val fields = reading.values.entries.joinToString(", ") { (key, value) -> "$key=$value" }
        return SamsungHealthPluginReply(
            text = "Samsung Health ${metric.atom} today: ${fields.ifEmpty { "no data" }}",
            success = true,
            status = status,
            reading = reading,
        )
    }

    private fun localError(error: SamsungHealthAction.LocalError): SamsungHealthPluginReply =
        SamsungHealthPluginReply(
            text = when (error.code) {
                "unknown_metric" -> "Unknown Samsung Health metric: ${error.subject}"
                else -> "Samsung Health rule rejected ${error.subject}: ${error.code}"
            },
            success = false,
        )

    private fun statusText(availability: SamsungHealthAvailability): String = when (availability) {
        SamsungHealthAvailability.SDK_MISSING -> "Samsung Health Data SDK is not bundled in this build."
        SamsungHealthAvailability.PLATFORM_NOT_INSTALLED -> "Samsung Health is not installed."
        SamsungHealthAvailability.PLATFORM_TOO_OLD -> "Samsung Health must be updated before Zara can read health data."
        SamsungHealthAvailability.PLATFORM_DISABLED -> "Samsung Health is disabled."
        SamsungHealthAvailability.PLATFORM_NOT_INITIALIZED -> "Finish Samsung Health setup before connecting Zara."
        SamsungHealthAvailability.AUTHORIZATION_REQUIRED -> "This Zara build is not authorized for Samsung Health Data SDK access."
        SamsungHealthAvailability.READY -> "Samsung Health is ready."
        SamsungHealthAvailability.ERROR -> "Samsung Health is temporarily unavailable."
    }

    private fun <T> submit(block: () -> T): CompletableFuture<T> {
        if (closed) return CompletableFuture.failedFuture(IllegalStateException("Samsung Health plugin is closed"))
        val future = CompletableFuture<T>()
        actor.execute {
            try {
                future.complete(block())
            } catch (error: Throwable) {
                future.completeExceptionally(error)
            }
        }
        return future
    }

    override fun close() {
        if (closed) return
        closed = true
        actor.shutdownNow()
    }
}

object SamsungHealthPrologPlugin {
    const val SOURCE_NAME = "samsung_health.pl"

    val source: String = """
        % Zara Android Samsung Health plugin.
        % This first slice is read-only. Samsung Health owns user data permissions.

        samsung_health_metric(steps).
        samsung_health_metric(sleep).
        samsung_health_metric(heart_rate).
        samsung_health_metric(exercise).

        samsung_health_status(Result) :-
            Result = samsung_health_action(status).

        samsung_health_permissions(Result) :-
            Result = samsung_health_action(permissions).

        samsung_health_today(Metric, Result) :-
            samsung_health_metric(Metric),
            !,
            Result = samsung_health_action(read_today(Metric)).
        samsung_health_today(Metric, Result) :-
            Result = samsung_health_action(error(unknown_metric, Metric)).

        samsung_health_steps(Result) :- samsung_health_today(steps, Result).
        samsung_health_sleep(Result) :- samsung_health_today(sleep, Result).
        samsung_health_heart_rate(Result) :- samsung_health_today(heart_rate, Result).
        samsung_health_exercise(Result) :- samsung_health_today(exercise, Result).

        expert_activation(samsung_health_steps, steps).
        expert_activation(samsung_health_sleep, sleep).
        expert_activation(samsung_health_heart_rate, heart_rate).
        expert_activation(samsung_health_exercise, exercise).
    """.trimIndent() + "\n"

    fun install(workspace: PrologWorkspace) {
        if (workspace.listSources().none { it.name == SOURCE_NAME }) {
            workspace.saveSource(SOURCE_NAME, source)
        }
    }
}