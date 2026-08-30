package ai.zara.app.runtime

sealed interface ServerConnection {
    data object Disconnected : ServerConnection
    data object Connecting : ServerConnection
    data object Connected : ServerConnection
    data class Reconnecting(val attempt: Int) : ServerConnection
    data class OfflineDegraded(val attempts: Int) : ServerConnection
}

sealed interface AssistantRole {
    data object NotYetAssessed : AssistantRole
    data object Held : AssistantRole
    data object NotHeld : AssistantRole
    data object PlatformUnavailable : AssistantRole
}

enum class RoleOutcome { HELD, NOT_HELD, PLATFORM_UNAVAILABLE }

data class RuntimeState(
    val server: ServerConnection,
    val assistantRole: AssistantRole,
    val maxReconnectAttempts: Int = 5,
) {
    companion object {
        fun initial(): RuntimeState =
            RuntimeState(ServerConnection.Disconnected, AssistantRole.NotYetAssessed)
    }
}

sealed interface RuntimeEvent {
    data object ConnectRequested : RuntimeEvent
    data object ConnectionEstablished : RuntimeEvent
    data object ConnectionLost : RuntimeEvent
    data class RoleAssessed(val outcome: RoleOutcome) : RuntimeEvent
}

fun reduce(state: RuntimeState, event: RuntimeEvent): RuntimeState = when (event) {
    RuntimeEvent.ConnectRequested -> when (state.server) {
        is ServerConnection.Disconnected, is ServerConnection.OfflineDegraded ->
            state.copy(server = ServerConnection.Connecting)
        else -> state
    }

    RuntimeEvent.ConnectionEstablished -> when (state.server) {
        is ServerConnection.Connecting -> state.copy(server = ServerConnection.Connected)
        else -> state
    }

    RuntimeEvent.ConnectionLost -> when (val server = state.server) {
        is ServerConnection.Connected -> state.copy(server = ServerConnection.Reconnecting(1))
        is ServerConnection.Reconnecting ->
            if (server.attempt >= state.maxReconnectAttempts) {
                state.copy(server = ServerConnection.OfflineDegraded(server.attempt))
            } else {
                state.copy(server = ServerConnection.Reconnecting(server.attempt + 1))
            }
        else -> state
    }

    is RuntimeEvent.RoleAssessed -> when (event.outcome) {
        RoleOutcome.HELD -> state.copy(assistantRole = AssistantRole.Held)
        RoleOutcome.NOT_HELD -> state.copy(assistantRole = AssistantRole.NotHeld)
        RoleOutcome.PLATFORM_UNAVAILABLE -> state.copy(assistantRole = AssistantRole.PlatformUnavailable)
    }
}
