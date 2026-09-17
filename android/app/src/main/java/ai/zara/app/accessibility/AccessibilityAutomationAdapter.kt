package ai.zara.app.accessibility

import ai.zara.app.device.DeviceActionErrorCode
import ai.zara.app.device.DeviceActionResult

sealed interface AccessibilityAutomationAction {
    data class Click(val selector: AccessibilitySelector) : AccessibilityAutomationAction
    data class SetText(val selector: AccessibilitySelector, val text: String) : AccessibilityAutomationAction
    data class ScrollForward(val selector: AccessibilitySelector) : AccessibilityAutomationAction
    data class Global(val action: AccessibilityGlobalAction) : AccessibilityAutomationAction
}

class AccessibilityAutomationAdapter {
    fun isAvailable(): Boolean = ZaraAccessibilityService.connected()

    fun execute(action: AccessibilityAutomationAction): DeviceActionResult {
        val service = ZaraAccessibilityService.current()
            ?: return DeviceActionResult.Error(DeviceActionErrorCode.PermissionDenied, "accessibility service is not enabled")
        return try {
            val completed = when (action) {
                is AccessibilityAutomationAction.Click -> service.click(action.selector)
                is AccessibilityAutomationAction.SetText -> service.setText(action.selector, action.text)
                is AccessibilityAutomationAction.ScrollForward -> service.scrollForward(action.selector)
                is AccessibilityAutomationAction.Global -> service.global(action.action)
            }
            if (completed) DeviceActionResult.Completed
            else DeviceActionResult.Error(DeviceActionErrorCode.Unavailable, "accessibility target is unavailable")
        } catch (_: SecurityException) {
            DeviceActionResult.Error(DeviceActionErrorCode.PermissionDenied)
        } catch (error: IllegalArgumentException) {
            DeviceActionResult.Error(DeviceActionErrorCode.InvalidArguments, error.message)
        } catch (_: Throwable) {
            DeviceActionResult.Error(DeviceActionErrorCode.Failed)
        }
    }
}
