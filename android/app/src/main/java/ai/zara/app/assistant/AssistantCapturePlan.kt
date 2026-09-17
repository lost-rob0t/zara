package ai.zara.app.assistant

import ai.zara.app.runtime.AssistantRole
import ai.zara.app.runtime.LocalServerPhase
import ai.zara.app.runtime.LocalServerState
import ai.zara.app.runtime.RuntimeMode
import ai.zara.app.runtime.RuntimeState
import ai.zara.app.runtime.ServerConnection

internal sealed interface AssistantCapturePlan {
    data object Local : AssistantCapturePlan
    data object Remote : AssistantCapturePlan
    data class Reject(val reason: String) : AssistantCapturePlan
}

internal fun planAssistantCapture(
    mode: RuntimeMode,
    localState: LocalServerState,
    runtimeState: RuntimeState,
): AssistantCapturePlan {
    if (runtimeState.assistantRole !is AssistantRole.Held) {
        return AssistantCapturePlan.Reject("Zara does not hold the Android Assistant role")
    }

    val localReady = localState.phase == LocalServerPhase.READY
    val remoteReady = runtimeState.server is ServerConnection.Connected &&
        runtimeState.sessionId != null

    return when (mode) {
        RuntimeMode.Local -> if (localReady) {
            AssistantCapturePlan.Local
        } else {
            AssistantCapturePlan.Reject("Local Zara server is not ready")
        }
        RuntimeMode.Remote -> AssistantCapturePlan.Remote
        RuntimeMode.Auto -> when {
            remoteReady -> AssistantCapturePlan.Remote
            localReady -> AssistantCapturePlan.Local
            else -> AssistantCapturePlan.Remote
        }
    }
}
