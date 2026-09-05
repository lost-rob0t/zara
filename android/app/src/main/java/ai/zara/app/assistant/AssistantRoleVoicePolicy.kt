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
