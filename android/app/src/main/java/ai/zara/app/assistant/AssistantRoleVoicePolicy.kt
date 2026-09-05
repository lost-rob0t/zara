package ai.zara.app.assistant

import ai.zara.app.runtime.AssistantRole

enum class AssistantVoiceOwnership {
    None,
    Manual,
    Assistant,
}

fun shouldCancelForAssistantRole(
    ownership: AssistantVoiceOwnership,
    role: AssistantRole,
): Boolean = ownership == AssistantVoiceOwnership.Assistant && role != AssistantRole.Held

class AssistantRoleVoiceGuard(
    private val cancelAssistantCapture: () -> Unit,
) {
    private var ownership: AssistantVoiceOwnership = AssistantVoiceOwnership.None

    @Synchronized
    fun onCaptureStarted(newOwnership: AssistantVoiceOwnership) {
        ownership = newOwnership
    }

    @Synchronized
    fun onCaptureStopped() {
        ownership = AssistantVoiceOwnership.None
    }

    @Synchronized
    fun onRoleChanged(role: AssistantRole) {
        if (!shouldCancelForAssistantRole(ownership, role)) return
        ownership = AssistantVoiceOwnership.None
        cancelAssistantCapture()
    }
}
