package ai.zara.app.ui

import ai.zara.app.runtime.EnrollmentReadiness
import ai.zara.app.runtime.LocalServerPhase
import ai.zara.app.runtime.LocalServerState
import ai.zara.app.runtime.RuntimeMode
import ai.zara.app.runtime.RuntimeState
import ai.zara.app.runtime.ServerConnection

/**
 * Compose-owned projection of canonical runtime state.
 *
 * This file contains no runtime, connection, session, or authority ownership. It only turns the
 * shared Core/Android state into truthful labels/readiness for native UI consumption.
 */
internal data class RuntimeUiProjection(
    val backendLabel: String,
    val chatReady: Boolean,
    val remoteInformational: Boolean,
)

internal fun runtimeUiProjection(
    mode: RuntimeMode,
    localState: LocalServerState,
    state: RuntimeState,
): RuntimeUiProjection {
    val localReady = localState.phase == LocalServerPhase.READY
    val connectedGeneration = (state.server as? ServerConnection.Connected)?.generation
    val remoteReady = state.enrollment == EnrollmentReadiness.Ready &&
        connectedGeneration == state.generation &&
        state.sessionId?.isNotBlank() == true

    val backend = when (mode) {
        RuntimeMode.Local -> if (localReady) "local" else "local (${localState.phase.name.lowercase()})"
        RuntimeMode.Remote -> if (remoteReady) "remote" else "remote (not ready)"
        RuntimeMode.Auto -> when {
            remoteReady -> "remote"
            localReady -> "local fallback"
            else -> "unavailable"
        }
    }

    val ready = when (mode) {
        RuntimeMode.Local -> localReady
        RuntimeMode.Remote -> remoteReady
        RuntimeMode.Auto -> remoteReady || localReady
    }

    return RuntimeUiProjection(
        backendLabel = backend,
        chatReady = ready,
        remoteInformational = mode == RuntimeMode.Local,
    )
}

internal fun activeRuntimeBackendLabel(
    mode: RuntimeMode,
    localState: LocalServerState,
    state: RuntimeState,
): String = runtimeUiProjection(mode, localState, state).backendLabel
