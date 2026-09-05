package ai.zara.app.runtime

sealed interface ServerConnection {
    data object Disconnected : ServerConnection
    data class Connecting(val generation: Long) : ServerConnection
    data class Connected(val generation: Long) : ServerConnection
    data class Reconnecting(val generation: Long, val attempt: Int) : ServerConnection
    data class OfflineDegraded(val attempts: Int, val reason: String) : ServerConnection
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
    val generation: Long = 0,
    val sessionId: String? = null,
    val selectedConversationId: String? = null,
    val maxReconnectAttempts: Int = 5,
) {
    init {
        require(generation >= 0) { "generation must be non-negative" }
        require(maxReconnectAttempts > 0) { "maxReconnectAttempts must be positive" }
    }

    companion object {
        fun initial(): RuntimeState =
            RuntimeState(ServerConnection.Disconnected, AssistantRole.NotYetAssessed)

        fun fromRestored(restored: RestorableClientState): RuntimeState =
            initial().copy(selectedConversationId = restored.selectedConversationId)
    }
}

sealed interface RuntimeEvent {
    data object ConnectRequested : RuntimeEvent
    data class HelloAccepted(val generation: Long, val sessionId: String) : RuntimeEvent
    data class ConnectionLost(val generation: Long, val reason: String) : RuntimeEvent
    data class ConnectionFailed(val generation: Long, val reason: String) : RuntimeEvent
    data class RoleAssessed(val outcome: RoleOutcome) : RuntimeEvent
}

fun reconnectDelayMillis(attempt: Int): Long {
    require(attempt > 0) { "attempt must be positive" }
    val shift = (attempt - 1).coerceAtMost(4)
    return (250L shl shift).coerceAtMost(4_000L)
}

fun reduce(state: RuntimeState, event: RuntimeEvent): RuntimeState = when (event) {
    RuntimeEvent.ConnectRequested -> when (state.server) {
        is ServerConnection.Disconnected, is ServerConnection.OfflineDegraded -> {
            val nextGeneration = state.generation + 1
            state.copy(
                server = ServerConnection.Connecting(nextGeneration),
                generation = nextGeneration,
                sessionId = null,
            )
        }
        else -> state
    }

    is RuntimeEvent.HelloAccepted -> {
        if (event.generation != state.generation || event.sessionId.isBlank()) {
            state
        } else {
            when (state.server) {
                is ServerConnection.Connecting, is ServerConnection.Reconnecting ->
                    state.copy(
                        server = ServerConnection.Connected(state.generation),
                        sessionId = event.sessionId,
                    )
                else -> state
            }
        }
    }

    is RuntimeEvent.ConnectionLost -> {
        if (event.generation != state.generation) {
            state
        } else {
            when (state.server) {
                is ServerConnection.Connected -> beginReconnect(state, 1, event.reason)
                is ServerConnection.Reconnecting ->
                    failReconnect(state, state.server.attempt, event.reason)
                else -> state
            }
        }
    }

    is RuntimeEvent.ConnectionFailed -> {
        if (event.generation != state.generation) {
            state
        } else {
            when (state.server) {
                is ServerConnection.Connecting -> beginReconnect(state, 1, event.reason)
                is ServerConnection.Reconnecting ->
                    failReconnect(state, state.server.attempt, event.reason)
                else -> state
            }
        }
    }

    is RuntimeEvent.RoleAssessed -> when (event.outcome) {
        RoleOutcome.HELD -> state.copy(assistantRole = AssistantRole.Held)
        RoleOutcome.NOT_HELD -> state.copy(assistantRole = AssistantRole.NotHeld)
        RoleOutcome.PLATFORM_UNAVAILABLE -> state.copy(assistantRole = AssistantRole.PlatformUnavailable)
    }
}

private fun beginReconnect(state: RuntimeState, attempt: Int, reason: String): RuntimeState {
    if (attempt > state.maxReconnectAttempts) {
        return state.copy(
            server = ServerConnection.OfflineDegraded(state.maxReconnectAttempts, reason),
            sessionId = null,
        )
    }
    val nextGeneration = state.generation + 1
    return state.copy(
        server = ServerConnection.Reconnecting(nextGeneration, attempt),
        generation = nextGeneration,
        sessionId = null,
    )
}

private fun failReconnect(state: RuntimeState, attempt: Int, reason: String): RuntimeState {
    if (attempt >= state.maxReconnectAttempts) {
        return state.copy(
            server = ServerConnection.OfflineDegraded(attempt, reason),
            sessionId = null,
        )
    }
    return beginReconnect(state, attempt + 1, reason)
}
