package ai.zara.app.assistant

import ai.zara.app.runtime.RoleOutcome

interface AssistantRolePlatform {
    fun isAvailable(): Boolean
    fun isHeld(): Boolean
}

class AssistantRoleController(
    private val platform: AssistantRolePlatform,
    private val outcomeObserver: (RoleOutcome) -> Unit,
) {
    fun assess(): RoleOutcome {
        val outcome = when {
            !platform.isAvailable() -> RoleOutcome.PLATFORM_UNAVAILABLE
            platform.isHeld() -> RoleOutcome.HELD
            else -> RoleOutcome.NOT_HELD
        }
        outcomeObserver(outcome)
        return outcome
    }

    @Suppress("UNUSED_PARAMETER")
    fun completeRequest(reportedGranted: Boolean): RoleOutcome = assess()
}
