package ai.zara.wear.health

import ai.zara.ui.health.SamsungHealthSensorTracker
import ai.zara.ui.health.WatchHealthCapabilities
import ai.zara.wear.BuildConfig
import android.content.Context
import android.os.Build
import java.util.concurrent.CompletableFuture

enum class WearHealthAvailability {
    SDK_MISSING,
    CONNECTING,
    READY,
    AUTHORIZATION_REQUIRED,
    SERVICE_UNAVAILABLE,
    ERROR,
}

data class WearHealthSnapshot(
    val availability: WearHealthAvailability,
    val capabilities: WatchHealthCapabilities = WatchHealthCapabilities.resolve(
        apiLevel = Build.VERSION.SDK_INT,
        reported = emptySet(),
    ),
)

interface WearHealthGateway : AutoCloseable {
    fun supportedTrackers(): CompletableFuture<Set<SamsungHealthSensorTracker>>
    override fun close()
}

class UnavailableWearHealthGateway : WearHealthGateway {
    override fun supportedTrackers(): CompletableFuture<Set<SamsungHealthSensorTracker>> =
        CompletableFuture.failedFuture(WearHealthUnavailableException(WearHealthAvailability.SDK_MISSING))

    override fun close() = Unit
}

class WearHealthUnavailableException(
    val availability: WearHealthAvailability,
) : IllegalStateException("Wear health unavailable: ${availability.name.lowercase()}")

object WearHealthGatewayLoader {
    private const val SDK_GATEWAY = "ai.zara.wear.health.sdk.SamsungHealthSensorGateway"

    fun create(context: Context): WearHealthGateway {
        if (!BuildConfig.HAS_SAMSUNG_HEALTH_SENSOR_SDK) return UnavailableWearHealthGateway()
        return try {
            val constructor = Class.forName(SDK_GATEWAY).getConstructor(Context::class.java)
            constructor.newInstance(context.applicationContext) as? WearHealthGateway
                ?: UnavailableWearHealthGateway()
        } catch (_: ReflectiveOperationException) {
            UnavailableWearHealthGateway()
        }
    }
}

class WearHealthController(
    private val gateway: WearHealthGateway,
    private val apiLevel: Int = Build.VERSION.SDK_INT,
) : AutoCloseable {
    fun refresh(): CompletableFuture<WearHealthSnapshot> =
        gateway.supportedTrackers().thenApply { supported ->
            WearHealthSnapshot(
                availability = WearHealthAvailability.READY,
                capabilities = WatchHealthCapabilities.resolve(apiLevel, supported),
            )
        }.exceptionally { failure ->
            val cause = generateSequence(failure as Throwable?) { it.cause }.last()
            WearHealthSnapshot(
                availability = (cause as? WearHealthUnavailableException)?.availability
                    ?: WearHealthAvailability.ERROR,
                capabilities = WatchHealthCapabilities.resolve(apiLevel, emptySet()),
            )
        }

    override fun close() {
        gateway.close()
    }
}
