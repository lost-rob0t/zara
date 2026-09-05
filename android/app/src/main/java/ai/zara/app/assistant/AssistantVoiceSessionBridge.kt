package ai.zara.app.assistant

import ai.zara.app.AndroidAppSession
import ai.zara.app.runtime.AssistantRole
import ai.zara.app.runtime.EnrollmentReadiness
import ai.zara.app.runtime.RuntimeState
import ai.zara.app.runtime.ServerConnection
import java.util.concurrent.CompletableFuture

sealed interface AssistantVoiceStartPlan {
    data object StartNow : AssistantVoiceStartPlan
    data class Reconnect(val endpoint: String) : AssistantVoiceStartPlan
    data class Reject(val reason: String) : AssistantVoiceStartPlan
}

internal fun planAssistantVoiceStart(
    state: RuntimeState,
    microphonePermissionGranted: Boolean,
): AssistantVoiceStartPlan {
    if (!microphonePermissionGranted) {
        return AssistantVoiceStartPlan.Reject("microphone permission is required")
    }
    if (state.assistantRole !is AssistantRole.Held) {
        return AssistantVoiceStartPlan.Reject("Zara does not hold the Android Assistant role")
    }
    if (state.enrollment != EnrollmentReadiness.Ready) {
        return AssistantVoiceStartPlan.Reject("Zara enrollment is not ready")
    }
    return when (state.server) {
        is ServerConnection.Connected -> {
            if (state.sessionId == null) {
                AssistantVoiceStartPlan.Reject("connected Zara state is missing a session")
            } else {
                AssistantVoiceStartPlan.StartNow
            }
        }
        ServerConnection.Disconnected, is ServerConnection.OfflineDegraded -> {
            val profile = state.configuredProfile
            if (profile == null) {
                AssistantVoiceStartPlan.Reject("Zara server is not configured")
            } else {
                AssistantVoiceStartPlan.Reconnect(profile.endpoint)
            }
        }
        is ServerConnection.Connecting, is ServerConnection.Reconnecting ->
            AssistantVoiceStartPlan.Reject("Zara connection is not ready")
    }
}

internal fun AndroidAppSession.startAssistantVoice(
    microphonePermissionGranted: Boolean,
): CompletableFuture<Unit> {
    assessAssistantRole()
    return when (val plan = planAssistantVoiceStart(state(), microphonePermissionGranted)) {
        AssistantVoiceStartPlan.StartNow -> pressToTalk(true)
        is AssistantVoiceStartPlan.Reconnect ->
            connect(plan.endpoint).thenCompose { pressToTalk(true) }
        is AssistantVoiceStartPlan.Reject -> failedFuture(IllegalStateException(plan.reason))
    }
}

private fun <T> failedFuture(error: Throwable): CompletableFuture<T> =
    CompletableFuture<T>().also { it.completeExceptionally(error) }
