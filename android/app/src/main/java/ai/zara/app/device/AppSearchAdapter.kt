package ai.zara.app.device

import ai.zara.app.runtime.DeviceCapability

class AppSearchAdapter(
    private val launcher: AppSearchLauncher,
) : DeviceCapabilityAdapter {
    override val capability = DeviceCapability.AppSearch

    override fun isAvailable(): Boolean = try {
        SEARCHABLE_ALIASES.any(launcher::isAvailable)
    } catch (_: Throwable) {
        false
    }

    override fun execute(arguments: DeviceActionArguments): DeviceActionResult {
        if (arguments !is DeviceActionArguments.AppSearch) {
            return DeviceActionResult.Error(DeviceActionErrorCode.InvalidArguments)
        }
        val alias = arguments.app.trim().lowercase()
        val query = arguments.query.trim()
        if (alias !in SEARCHABLE_ALIASES || query.isEmpty() || query.encodeToByteArray().size > 512) {
            return DeviceActionResult.Error(DeviceActionErrorCode.InvalidArguments)
        }
        val available = try {
            launcher.isAvailable(alias)
        } catch (_: SecurityException) {
            return DeviceActionResult.Error(DeviceActionErrorCode.PermissionDenied)
        } catch (_: Throwable) {
            return DeviceActionResult.Error(DeviceActionErrorCode.Failed)
        }
        if (!available) return DeviceActionResult.Error(DeviceActionErrorCode.Unavailable)

        return try {
            launcher.search(alias, query)
            DeviceActionResult.Completed
        } catch (_: SecurityException) {
            DeviceActionResult.Error(DeviceActionErrorCode.PermissionDenied)
        } catch (_: Throwable) {
            DeviceActionResult.Error(DeviceActionErrorCode.Failed)
        }
    }

    private companion object {
        val SEARCHABLE_ALIASES = setOf("youtube", "youtube_revanced")
    }
}
