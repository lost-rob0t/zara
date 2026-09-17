package ai.zara.app.device

sealed interface DeviceActionArguments {
    data class OpenUri(val uri: String) : DeviceActionArguments
    data class OpenApp(val app: String) : DeviceActionArguments
    data class AndroidRaw(
        val backend: String,
        val operation: String,
        val arguments: Map<String, String>,
    ) : DeviceActionArguments
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
    data class CompletedWithOutput(
        val backend: String,
        val identity: String,
        val output: String? = null,
    ) : DeviceActionResult
    data class Error(
        val code: DeviceActionErrorCode,
        val message: String? = null,
    ) : DeviceActionResult
}
