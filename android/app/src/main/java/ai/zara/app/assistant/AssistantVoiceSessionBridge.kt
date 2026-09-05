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
    lifecycleFence: AssistantLifecycleFence,
    lifecycleToken: Long,
): CompletableFuture<Unit> {
    assessAssistantRole()
    val start = when (val plan = planAssistantVoiceStart(state(), microphonePermissionGranted)) {
        AssistantVoiceStartPlan.StartNow ->
            startAssistantCaptureIfCurrent(lifecycleFence, lifecycleToken)
        is AssistantVoiceStartPlan.Reconnect -> {
            if (!lifecycleFence.isCurrent(lifecycleToken)) {
                failedFuture(AssistantInvocationInvalidated())
            } else {
                connect(plan.endpoint).thenCompose {
                    startAssistantCaptureIfCurrent(lifecycleFence, lifecycleToken)
                }
            }
        }
        is AssistantVoiceStartPlan.Reject -> failedFuture(IllegalStateException(plan.reason))
    }
    return start.thenCompose {
        if (lifecycleFence.isCurrent(lifecycleToken)) {
            CompletableFuture.completedFuture(Unit)
        } else {
            cancelInvalidatedAssistantCapture()
        }
    }
}

private fun AndroidAppSession.startAssistantCaptureIfCurrent(
    lifecycleFence: AssistantLifecycleFence,
    lifecycleToken: Long,
): CompletableFuture<Unit> =
    if (lifecycleFence.isCurrent(lifecycleToken)) {
        pressAssistantToTalk(true)
    } else {
        failedFuture(AssistantInvocationInvalidated())
    }

private fun AndroidAppSession.cancelInvalidatedAssistantCapture(): CompletableFuture<Unit> {
    val result = CompletableFuture<Unit>()
    cancelPushToTalk().whenComplete { _, cancelError ->
        if (cancelError == null) {
            result.completeExceptionally(AssistantInvocationInvalidated())
        } else {
            result.completeExceptionally(
                IllegalStateException(
                    "assistant invocation was invalidated and voice cancellation failed",
                    unwrapCompletion(cancelError),
                ),
            )
        }
    }
    return result
}

private fun unwrapCompletion(error: Throwable): Throwable {
    var current = error
    while (current.cause != null && current.cause !== current) {
        current = current.cause!!
    }
    return current
}

private class AssistantInvocationInvalidated :
    IllegalStateException("assistant invocation was invalidated")

private fun <T> failedFuture(error: Throwable): CompletableFuture<T> =
    CompletableFuture<T>().also { it.completeExceptionally(error) }
