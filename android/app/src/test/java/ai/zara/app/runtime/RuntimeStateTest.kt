package ai.zara.app.runtime

import org.junit.Assert.assertEquals
import org.junit.Test

class RuntimeStateTest {

    @Test fun `initial state is disconnected with unassessed role`() {
        val state = RuntimeState.initial()
        assertEquals(ServerConnection.Disconnected, state.server)
        assertEquals(AssistantRole.NotYetAssessed, state.assistantRole)
        assertEquals(0L, state.generation)
        assertEquals(null, state.sessionId)
    }

    @Test fun `connect request starts a new generation`() {
        val state = reduce(RuntimeState.initial(), RuntimeEvent.ConnectRequested)
        assertEquals(ServerConnection.Connecting(1), state.server)
        assertEquals(1L, state.generation)
    }

    @Test fun `hello accepted establishes only current generation session`() {
        val connecting = reduce(RuntimeState.initial(), RuntimeEvent.ConnectRequested)
        val stale = reduce(connecting, RuntimeEvent.HelloAccepted(0, "old-session"))
        assertEquals(ServerConnection.Connecting(1), stale.server)
        assertEquals(null, stale.sessionId)

        val connected = reduce(connecting, RuntimeEvent.HelloAccepted(1, "session-1"))
        assertEquals(ServerConnection.Connected(1), connected.server)
        assertEquals("session-1", connected.sessionId)
    }

    @Test fun `connection loss clears session and schedules bounded reconnect`() {
        var state = reduce(RuntimeState.initial(), RuntimeEvent.ConnectRequested)
        state = reduce(state, RuntimeEvent.HelloAccepted(1, "session-1"))
        state = reduce(state, RuntimeEvent.ConnectionLost(1, "network_lost"))
        assertEquals(ServerConnection.Reconnecting(2, 1), state.server)
        assertEquals(2L, state.generation)
        assertEquals(null, state.sessionId)

        state = reduce(state, RuntimeEvent.ConnectionLost(1, "stale"))
        assertEquals(ServerConnection.Reconnecting(2, 1), state.server)
    }

    @Test fun `reconnect failures advance generation and eventually degrade offline`() {
        var state = reduce(RuntimeState.initial(), RuntimeEvent.ConnectRequested)
        state = reduce(state, RuntimeEvent.HelloAccepted(1, "session-1"))
        state = reduce(state, RuntimeEvent.ConnectionLost(1, "network_lost"))

        repeat(4) {
            val generation = state.generation
            state = reduce(state, RuntimeEvent.ConnectionFailed(generation, "unavailable"))
        }
        assertEquals(ServerConnection.Reconnecting(6, 5), state.server)
        assertEquals(6L, state.generation)

        state = reduce(state, RuntimeEvent.ConnectionFailed(6, "unavailable"))
        assertEquals(ServerConnection.OfflineDegraded(5, "unavailable"), state.server)
        assertEquals(null, state.sessionId)
    }

    @Test fun `manual retry from offline starts fresh generation`() {
        val offline = RuntimeState(
            server = ServerConnection.OfflineDegraded(5, "unavailable"),
            assistantRole = AssistantRole.NotYetAssessed,
            generation = 7,
        )
        val state = reduce(offline, RuntimeEvent.ConnectRequested)
        assertEquals(ServerConnection.Connecting(8), state.server)
        assertEquals(8L, state.generation)
    }

    @Test fun `selected conversation survives reconnect but session does not`() {
        var state = RuntimeState.initial().copy(selectedConversationId = "conversation-7")
        state = reduce(state, RuntimeEvent.ConnectRequested)
        state = reduce(state, RuntimeEvent.HelloAccepted(1, "session-1"))
        state = reduce(state, RuntimeEvent.ConnectionLost(1, "network_lost"))
        assertEquals("conversation-7", state.selectedConversationId)
        assertEquals(null, state.sessionId)
    }

    @Test fun `reconnect backoff is deterministic bounded and monotonic`() {
        assertEquals(250L, reconnectDelayMillis(1))
        assertEquals(500L, reconnectDelayMillis(2))
        assertEquals(1000L, reconnectDelayMillis(3))
        assertEquals(2000L, reconnectDelayMillis(4))
        assertEquals(4000L, reconnectDelayMillis(5))
        assertEquals(4000L, reconnectDelayMillis(99))
    }

    @Test fun `role assessment updates held not-held and unavailable states`() {
        val held = reduce(RuntimeState.initial(), RuntimeEvent.RoleAssessed(RoleOutcome.HELD))
        assertEquals(AssistantRole.Held, held.assistantRole)
        val notHeld = reduce(held, RuntimeEvent.RoleAssessed(RoleOutcome.NOT_HELD))
        assertEquals(AssistantRole.NotHeld, notHeld.assistantRole)
        val unavailable = reduce(notHeld, RuntimeEvent.RoleAssessed(RoleOutcome.PLATFORM_UNAVAILABLE))
        assertEquals(AssistantRole.PlatformUnavailable, unavailable.assistantRole)
    }

    @Test fun `stale connection failure cannot mutate current generation`() {
        val connecting = reduce(RuntimeState.initial(), RuntimeEvent.ConnectRequested)
        val state = reduce(connecting, RuntimeEvent.ConnectionFailed(0, "stale"))
        assertEquals(ServerConnection.Connecting(1), state.server)
        assertEquals(1L, state.generation)
    }
}
