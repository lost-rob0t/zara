package ai.zara.app.device

import ai.zara.app.runtime.DeviceCapability

interface UriLauncher {
    fun isAvailable(): Boolean
    fun open(uri: String)
}

class OpenUriAdapter(
    private val launcher: UriLauncher,
) : DeviceCapabilityAdapter {
    override val capability: DeviceCapability = DeviceCapability.OpenUri

    override fun isAvailable(): Boolean = launcher.isAvailable()

    override fun execute(arguments: DeviceActionArguments): DeviceActionResult {
        val openUri = arguments as? DeviceActionArguments.OpenUri
            ?: return DeviceActionResult.Error(DeviceActionErrorCode.InvalidArguments)
        if (!isAvailable()) {
            return DeviceActionResult.Error(DeviceActionErrorCode.Unavailable)
        }
        val normalized = try {
            OpenUriPolicy.normalize(openUri.uri)
        } catch (_: IllegalArgumentException) {
            return DeviceActionResult.Error(DeviceActionErrorCode.InvalidArguments)
        }
        return try {
            launcher.open(normalized)
            DeviceActionResult.Completed
        } catch (_: SecurityException) {
            DeviceActionResult.Error(DeviceActionErrorCode.PermissionDenied)
        } catch (_: Throwable) {
            DeviceActionResult.Error(DeviceActionErrorCode.Failed)
        }
    }
}
