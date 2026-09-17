package ai.zara.app.device

import ai.zara.app.integration.AndroidAuthorityLevel
import ai.zara.app.integration.AndroidIntegrationRuntime
import ai.zara.app.integration.AndroidOperationError
import ai.zara.app.integration.AndroidOperationRequest
import ai.zara.app.runtime.DeviceCapability
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicReference

class AndroidRawAdapter(
    private val integration: AndroidIntegrationRuntime,
) : DeviceCapabilityAdapter {
    override val capability = DeviceCapability.AndroidRaw

    override fun isAvailable(): Boolean {
        val authority = integration.authority()
        return authority.global == AndroidAuthorityLevel.UNRESTRICTED ||
            authority.backendLevels.values.any { it == AndroidAuthorityLevel.UNRESTRICTED }
    }

    override fun execute(arguments: DeviceActionArguments): DeviceActionResult {
        DeviceActionResultReceipts.discardStaged()
        val raw = arguments as? DeviceActionArguments.AndroidRaw
            ?: return DeviceActionResult.Error(DeviceActionErrorCode.InvalidArguments)
        val result = try {
            integration.execute(
                AndroidOperationRequest(
                    backend = raw.backend,
                    operation = raw.operation,
                    arguments = raw.arguments,
                ),
            ).get(MAX_EXECUTION_SECONDS, TimeUnit.SECONDS)
        } catch (_: IllegalArgumentException) {
            return DeviceActionResult.Error(DeviceActionErrorCode.InvalidArguments)
        } catch (_: Throwable) {
            return DeviceActionResult.Error(DeviceActionErrorCode.Failed)
        }
        if (result.success) {
            DeviceActionResultReceipts.stage(
                AndroidDeviceExecutionReceipt(
                    backend = result.backend ?: raw.backend,
                    identity = result.identity ?: "unknown",
                    output = result.output?.take(MAX_RESULT_CHARS),
                ),
            )
            return DeviceActionResult.Completed
        }
        DeviceActionResultReceipts.discardStaged()
        val code = when (result.error) {
            AndroidOperationError.AUTHORITY_DENIED -> DeviceActionErrorCode.PermissionDenied
            AndroidOperationError.BACKEND_UNAVAILABLE -> DeviceActionErrorCode.Unavailable
            AndroidOperationError.INVALID_BACKEND,
            AndroidOperationError.UNSUPPORTED_OPERATION -> DeviceActionErrorCode.InvalidArguments
            AndroidOperationError.CANCELLED -> DeviceActionErrorCode.Cancelled
            AndroidOperationError.FAILED,
            null -> DeviceActionErrorCode.Failed
        }
        return DeviceActionResult.Error(code, result.message?.take(MAX_ERROR_CHARS))
    }

    private companion object {
        const val MAX_EXECUTION_SECONDS = 305L
        const val MAX_RESULT_CHARS = 48 * 1024
        const val MAX_ERROR_CHARS = 1024
    }
}

object DeviceCapabilityExtensions {
    private val androidRaw = AtomicReference<DeviceCapabilityAdapter?>(null)

    fun installAndroidRaw(adapter: DeviceCapabilityAdapter) {
        require(adapter.capability == DeviceCapability.AndroidRaw) {
            "android raw extension must own android_raw capability"
        }
        androidRaw.set(adapter)
    }

    fun adapters(): List<DeviceCapabilityAdapter> = listOfNotNull(androidRaw.get())
}
