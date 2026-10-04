package ai.zara.wear

import ai.zara.ui.health.SamsungHealthSensorTracker
import ai.zara.wear.health.WearHealthAvailability
import ai.zara.wear.health.WearHealthController
import ai.zara.wear.health.WearHealthGateway
import ai.zara.wear.health.WearHealthUnavailableException
import java.util.concurrent.CompletableFuture
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class WearHealthControllerTest {
    @Test
    fun resolvesReportedTrackersAgainstRuntimeApiLevel() {
        val gateway = Gateway(
            CompletableFuture.completedFuture(
                setOf(
                    SamsungHealthSensorTracker.HEART_RATE_CONTINUOUS,
                    SamsungHealthSensorTracker.SKIN_TEMPERATURE_CONTINUOUS,
                ),
            ),
        )

        val snapshot = WearHealthController(gateway, apiLevel = 32).refresh().get()

        assertEquals(WearHealthAvailability.READY, snapshot.availability)
        assertTrue(SamsungHealthSensorTracker.HEART_RATE_CONTINUOUS in snapshot.capabilities.available)
        assertTrue(SamsungHealthSensorTracker.SKIN_TEMPERATURE_CONTINUOUS in snapshot.capabilities.updateRequired)
    }

    @Test
    fun preservesActionableVendorAvailabilityFailureAndClosesGateway() {
        val gateway = Gateway(
            CompletableFuture.failedFuture(
                WearHealthUnavailableException(WearHealthAvailability.AUTHORIZATION_REQUIRED),
            ),
        )
        val controller = WearHealthController(gateway, apiLevel = 36)

        assertEquals(WearHealthAvailability.AUTHORIZATION_REQUIRED, controller.refresh().get().availability)
        controller.close()
        assertTrue(gateway.closed)
    }

    private class Gateway(
        private val response: CompletableFuture<Set<SamsungHealthSensorTracker>>,
    ) : WearHealthGateway {
        var closed = false

        override fun supportedTrackers(): CompletableFuture<Set<SamsungHealthSensorTracker>> = response

        override fun close() {
            closed = true
        }
    }
}
