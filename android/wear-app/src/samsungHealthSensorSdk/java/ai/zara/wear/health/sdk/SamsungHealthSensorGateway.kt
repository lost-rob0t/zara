package ai.zara.wear.health.sdk

import ai.zara.ui.health.SamsungHealthSensorTracker
import ai.zara.wear.health.WearHealthAvailability
import ai.zara.wear.health.WearHealthGateway
import ai.zara.wear.health.WearHealthUnavailableException
import android.content.Context
import com.samsung.android.service.health.tracking.HealthTrackerException
import com.samsung.android.service.health.tracking.HealthTrackerType
import com.samsung.android.service.health.tracking.HealthTrackingService
import java.util.concurrent.CompletableFuture

class SamsungHealthSensorGateway(context: Context) : WearHealthGateway {
    private val capability = CompletableFuture<Set<SamsungHealthSensorTracker>>()
    private val service = HealthTrackingService(
        object : HealthTrackingService.ConnectionListener {
            override fun onConnectionSuccess() {
                val supported = service.trackingCapability.supportHealthTrackerTypes
                    .mapNotNullTo(linkedSetOf()) { it.toZaraTracker() }
                capability.complete(supported)
            }

            override fun onConnectionEnded() {
                capability.completeExceptionally(
                    WearHealthUnavailableException(WearHealthAvailability.SERVICE_UNAVAILABLE),
                )
            }

            override fun onConnectionFailed(error: HealthTrackerException) {
                capability.completeExceptionally(
                    WearHealthUnavailableException(WearHealthAvailability.AUTHORIZATION_REQUIRED),
                )
            }
        },
        context.applicationContext,
    ).also { it.connectService() }

    override fun supportedTrackers(): CompletableFuture<Set<SamsungHealthSensorTracker>> = capability

    override fun close() {
        service.disconnectService()
    }

    private fun HealthTrackerType.toZaraTracker(): SamsungHealthSensorTracker? =
        SamsungHealthSensorTracker.fromAtom(name.lowercase())
}
