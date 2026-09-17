package ai.zara.app.device

import java.util.concurrent.ConcurrentHashMap

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
    data class Error(
        val code: DeviceActionErrorCode,
        val message: String? = null,
    ) : DeviceActionResult
}

data class AndroidDeviceExecutionReceipt(
    val backend: String,
    val identity: String,
    val output: String? = null,
)

object DeviceActionResultReceipts {
    private val staged = ThreadLocal<AndroidDeviceExecutionReceipt?>()
    private val completed = ConcurrentHashMap<String, AndroidDeviceExecutionReceipt>()

    fun stage(receipt: AndroidDeviceExecutionReceipt) {
        staged.set(receipt)
    }

    fun discardStaged() {
        staged.remove()
    }

    fun bindToAction(actionId: String) {
        val receipt = staged.get()
        staged.remove()
        if (receipt != null) completed[actionId] = receipt
    }

    fun take(actionId: String): AndroidDeviceExecutionReceipt? = completed.remove(actionId)
}
