package ai.zara.app.device

import ai.zara.app.runtime.DeviceCapability

interface AppLauncher {
    fun isAvailable(alias: String): Boolean
    fun launch(alias: String)
}

class OpenAppAdapter(
    private val launcher: AppLauncher,
) : DeviceCapabilityAdapter {
    override val capability = DeviceCapability.OpenApp

    override fun isAvailable(): Boolean =
        REVIEWED_ALIASES.any(launcher::isAvailable)

    override fun execute(arguments: DeviceActionArguments): DeviceActionResult {
        if (arguments !is DeviceActionArguments.OpenApp) {
            return DeviceActionResult.Error(DeviceActionErrorCode.InvalidArguments)
        }

        val alias = arguments.app.trim().lowercase()
        if (alias !in REVIEWED_ALIASES) {
            return DeviceActionResult.Error(DeviceActionErrorCode.InvalidArguments)
        }
        if (!launcher.isAvailable(alias)) {
            return DeviceActionResult.Error(DeviceActionErrorCode.Unavailable)
        }

        return try {
            launcher.launch(alias)
            DeviceActionResult.Completed
        } catch (_: SecurityException) {
            DeviceActionResult.Error(DeviceActionErrorCode.PermissionDenied)
        } catch (_: Throwable) {
            DeviceActionResult.Error(DeviceActionErrorCode.Failed)
        }
    }

    private companion object {
        val REVIEWED_ALIASES = setOf("browser", "youtube")
    }
}
