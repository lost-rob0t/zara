package ai.zara.app.runtime

import ai.zara.app.device.DeviceActionResult

interface DeviceActionHandler {
    fun availableCapabilities(): Set<DeviceCapability>
    fun execute(request: DeviceServerMessage.Request): DeviceActionResult
    fun cancel(cancel: DeviceServerMessage.Cancel)
}
