package ai.zara.app.device

sealed interface DeviceActionArguments {
    data class OpenUri(val uri: String) : DeviceActionArguments
    data class OpenApp(val app: String) : DeviceActionArguments
    data class SendSms(val to: String, val text: String) : DeviceActionArguments
    data class AppSearch(val app: String, val query: String) : DeviceActionArguments
}

enum class DeviceActionErrorCode(val wireId: String) {
    PermissionDenied("permission_denied"),
    Unavailable("unavailable"),
    InvalidArguments("invalid_arguments"),
    Failed("failed"),
    Cancelled("cancelled"),
}

sealed interface DeviceActionResult {
    data object Completed : DeviceActionResult
    data class Error(
        val code: DeviceActionErrorCode,
        val message: String? = null,
    ) : DeviceActionResult
}
