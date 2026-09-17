package ai.zara.app.device

class AppSearchAdapter(
    private val launcher: AppSearchLauncher,
) {
    fun isAvailable(): Boolean = try {
        SEARCHABLE_ALIASES.any(launcher::isAvailable)
    } catch (_: Throwable) {
        false
    }

    fun execute(arguments: DeviceActionArguments.AppSearch): DeviceActionResult {
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

    companion object {
        val SEARCHABLE_ALIASES = setOf("youtube", "youtube_revanced")
    }
}
