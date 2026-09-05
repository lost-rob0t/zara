package ai.zara.app.device

import ai.zara.app.runtime.DeviceActionHandler
import ai.zara.app.runtime.DeviceCapability
import ai.zara.app.runtime.DeviceServerMessage

class RegistryDeviceActionHandler(
    private val registry: DeviceCapabilityRegistry,
) : DeviceActionHandler {
    override fun availableCapabilities(): Set<DeviceCapability> =
        registry.availableCapabilities()

    override fun execute(request: DeviceServerMessage.Request): DeviceActionResult =
        try {
            registry.execute(request.capability, request.arguments)
        } catch (_: DeviceCapabilityUnavailableException) {
            DeviceActionResult.Error(DeviceActionErrorCode.Unavailable)
        }

    override fun cancel(cancel: DeviceServerMessage.Cancel) {
        // Current #174 adapters complete synchronously. Recording the terminal action id
        // remains owned by ZaraTextClientActor; future long-running adapters must add an
        // explicit cancellation token before they can be advertised.
    }
}
