package ai.zara.app.device

import ai.zara.app.runtime.DeviceCapability

interface CalendarIntentLauncher {
    fun isAvailable(): Boolean
    fun insert(arguments: DeviceActionArguments.CalendarInsert)
}

class CalendarInsertAdapter(
    private val launcher: CalendarIntentLauncher,
) : DeviceCapabilityAdapter {
    override val capability = DeviceCapability.CalendarInsert

    override fun isAvailable(): Boolean = try {
        launcher.isAvailable()
    } catch (_: Throwable) {
        false
    }

    override fun execute(arguments: DeviceActionArguments): DeviceActionResult {
        val insert = arguments as? DeviceActionArguments.CalendarInsert
            ?: return DeviceActionResult.Error(DeviceActionErrorCode.InvalidArguments)
        if (!valid(insert)) {
            return DeviceActionResult.Error(DeviceActionErrorCode.InvalidArguments)
        }

        val available = try {
            launcher.isAvailable()
        } catch (_: SecurityException) {
            return DeviceActionResult.Error(DeviceActionErrorCode.PermissionDenied)
        } catch (_: Throwable) {
            return DeviceActionResult.Error(DeviceActionErrorCode.Failed)
        }
        if (!available) {
            return DeviceActionResult.Error(DeviceActionErrorCode.Unavailable)
        }

        return try {
            launcher.insert(insert)
            DeviceActionResult.Completed
        } catch (_: SecurityException) {
            DeviceActionResult.Error(DeviceActionErrorCode.PermissionDenied)
        } catch (_: Throwable) {
            DeviceActionResult.Error(DeviceActionErrorCode.Failed)
        }
    }

    private fun valid(arguments: DeviceActionArguments.CalendarInsert): Boolean {
        if (!validText(arguments.title, 512, required = true)) return false
        if (!validText(arguments.location, 2_048, required = false)) return false
        if (!validText(arguments.description, 2_048, required = false)) return false
        if (arguments.startMillis < 0 || arguments.endMillis <= arguments.startMillis) return false
        return arguments.endMillis - arguments.startMillis <= MAX_EVENT_DURATION_MILLIS
    }

    private fun validText(value: String?, maxBytes: Int, required: Boolean): Boolean {
        if (value == null) return !required
        if (required && value.isBlank()) return false
        if (value.encodeToByteArray().size > maxBytes) return false
        return value.none { it.code < 0x20 || it.code == 0x7f }
    }

    private companion object {
        const val MAX_EVENT_DURATION_MILLIS = 366L * 24 * 60 * 60 * 1000
    }
}
