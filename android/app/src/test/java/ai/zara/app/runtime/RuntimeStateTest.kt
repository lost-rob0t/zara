package ai.zara.app.runtime

import org.junit.Assert.assertEquals
import org.junit.Test

class RuntimeStateTest {

    @Test fun `initial state is disconnected with unassessed role`() {
        val state = RuntimeState.initial()
        assertEquals(ServerConnection.Disconnected, state.server)
        assertEquals(AssistantRole.NotYetAssessed, state.assistantRole)
    }

    @Test fun `connect requested moves disconnected to connecting`() {
        val state = reduce(RuntimeState.initial(), RuntimeEvent.ConnectRequested)
        assertEquals(ServerConnection.Connecting, state.server)
    }

    @Test fun `connect requested is idempotent while connected`() {
        val connected = reduce(
            reduce(RuntimeState.initial(), RuntimeEvent.ConnectRequested),
            RuntimeEvent.ConnectionEstablished
        )
        val again = reduce(connected, RuntimeEvent.ConnectRequested)
        assertEquals(ServerConnection.Connected, again.server)
    }

    @Test fun `established connects only from connecting`() {
        val connecting = reduce(RuntimeState.initial(), RuntimeEvent.ConnectRequested)
        val connected = reduce(connecting, RuntimeEvent.ConnectionEstablished)
        assertEquals(ServerConnection.Connected, connected.server)
    }

    @Test fun `stale established event from disconnected is rejected`() {
        val state = reduce(RuntimeState.initial(), RuntimeEvent.ConnectionEstablished)
        assertEquals(ServerConnection.Disconnected, state.server)
    }

    @Test fun `connection loss schedules bounded reconnect attempts`() {
        val connected = reduce(
            reduce(RuntimeState.initial(), RuntimeEvent.ConnectRequested),
            RuntimeEvent.ConnectionEstablished
        )
        val lost1 = reduce(connected, RuntimeEvent.ConnectionLost)
        assertEquals(ServerConnection.Reconnecting(1), lost1.server)
        val lost2 = reduce(lost1, RuntimeEvent.ConnectionLost)
        assertEquals(ServerConnection.Reconnecting(2), lost2.server)
    }

    @Test fun `exhausted reconnect attempts degrade to offline`() {
        var state = reduce(
            reduce(RuntimeState.initial(), RuntimeEvent.ConnectRequested),
            RuntimeEvent.ConnectionEstablished
        )
        repeat(5) { state = reduce(state, RuntimeEvent.ConnectionLost) }
        assertEquals(ServerConnection.Reconnecting(5), state.server)
        state = reduce(state, RuntimeEvent.ConnectionLost)
        assertEquals(ServerConnection.OfflineDegraded(5), state.server)
    }

    @Test fun `connect requested from offline degraded retries connecting`() {
        var state = reduce(
            reduce(RuntimeState.initial(), RuntimeEvent.ConnectRequested),
            RuntimeEvent.ConnectionEstablished
        )
        repeat(6) { state = reduce(state, RuntimeEvent.ConnectionLost) }
        state = reduce(state, RuntimeEvent.ConnectRequested)
        assertEquals(ServerConnection.Connecting, state.server)
    }

    @Test fun `role assessment updates held not-held and unavailable states`() {
        val held = reduce(RuntimeState.initial(), RuntimeEvent.RoleAssessed(RoleOutcome.HELD))
        assertEquals(AssistantRole.Held, held.assistantRole)
        val notHeld = reduce(held, RuntimeEvent.RoleAssessed(RoleOutcome.NOT_HELD))
        assertEquals(AssistantRole.NotHeld, notHeld.assistantRole)
        val unavailable = reduce(notHeld, RuntimeEvent.RoleAssessed(RoleOutcome.PLATFORM_UNAVAILABLE))
        assertEquals(AssistantRole.PlatformUnavailable, unavailable.assistantRole)
    }

    @Test fun `connection events never touch role state`() {
        val state = reduce(RuntimeState.initial(), RuntimeEvent.ConnectRequested)
        assertEquals(AssistantRole.NotYetAssessed, state.assistantRole)
    }
}
