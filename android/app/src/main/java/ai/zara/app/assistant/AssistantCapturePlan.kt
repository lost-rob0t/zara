package ai.zara.app.assistant

import ai.zara.app.runtime.AssistantRole
import ai.zara.app.runtime.EnrollmentReadiness
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
    remoteModelReady: Boolean = false,
): AssistantCapturePlan {
    if (runtimeState.assistantRole !is AssistantRole.Held) {
        return AssistantCapturePlan.Reject("Zara does not hold the Android Assistant role")
    }

    val localReady = localState.phase == LocalServerPhase.READY
    val connectedGeneration = (runtimeState.server as? ServerConnection.Connected)?.generation
    val remoteReady = runtimeState.enrollment == EnrollmentReadiness.Ready &&
        connectedGeneration == runtimeState.generation &&
        runtimeState.sessionId?.isNotBlank() == true

    return when (mode) {
        RuntimeMode.Symbolic, RuntimeMode.Local -> if (localReady) {
            AssistantCapturePlan.Local
        } else {
            AssistantCapturePlan.Reject("Local Zara server is not ready")
        }
        RuntimeMode.Remote -> when {
            remoteReady -> AssistantCapturePlan.Remote
            remoteModelReady && localReady -> AssistantCapturePlan.Local
            else -> AssistantCapturePlan.Reject("Remote Zara session or model provider is not ready")
        }
        RuntimeMode.Auto -> when {
            remoteReady -> AssistantCapturePlan.Remote
            localReady -> AssistantCapturePlan.Local
            else -> AssistantCapturePlan.Reject("No Zara runtime is ready")
        }
    }
}
