package ai.zara.app.ui

import ai.zara.app.runtime.AssistantRole
import ai.zara.app.runtime.EnrollmentReadiness
import ai.zara.app.runtime.RuntimeState
import ai.zara.app.runtime.ServerConnection
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class ZaraAppProjectionTest {
    @Test
    fun connectionLabelsRemainHonestAcrossCanonicalReducerStates() {
        assertEquals("disconnected", connectionLabel(ServerConnection.Disconnected))
        assertEquals("connecting", connectionLabel(ServerConnection.Connecting(1)))
        assertEquals("connected", connectionLabel(ServerConnection.Connected(2)))
        assertEquals(
            "reconnecting (attempt 3)",
            connectionLabel(ServerConnection.Reconnecting(4, 3)),
        )
        assertEquals(
            "offline (network unavailable)",
            connectionLabel(ServerConnection.OfflineDegraded(5, "network unavailable")),
        )
    }

    @Test
    fun enrollmentLabelsDoNotInventAuthenticatedState() {
        assertEquals("unenrolled", enrollmentLabel(EnrollmentReadiness.Unenrolled))
        assertEquals(
            "awaiting server pin",
            enrollmentLabel(EnrollmentReadiness.AwaitingServerPin),
        )
        assertEquals("ready", enrollmentLabel(EnrollmentReadiness.Ready))
        assertEquals("corrupt", enrollmentLabel(EnrollmentReadiness.Corrupt))
    }

    @Test
    fun assistantRoleProjectionIsExplicitAndOnlyMissingRoleCanRequestOnboarding() {
        assertEquals("not assessed", assistantRoleLabel(AssistantRole.NotYetAssessed))
        assertEquals("held", assistantRoleLabel(AssistantRole.Held))
        assertEquals("not held", assistantRoleLabel(AssistantRole.NotHeld))
        assertEquals("platform unavailable", assistantRoleLabel(AssistantRole.PlatformUnavailable))
        assertFalse(canRequestAssistantRole(AssistantRole.NotYetAssessed))
        assertFalse(canRequestAssistantRole(AssistantRole.Held))
        assertTrue(canRequestAssistantRole(AssistantRole.NotHeld))
        assertFalse(canRequestAssistantRole(AssistantRole.PlatformUnavailable))
    }

    @Test
    fun connectControlCannotStartParallelConnectionLifecycle() {
        assertTrue(canRequestConnect(ServerConnection.Disconnected))
        assertTrue(canRequestConnect(ServerConnection.OfflineDegraded(3, "network unavailable")))
        assertFalse(canRequestConnect(ServerConnection.Connecting(4)))
        assertFalse(canRequestConnect(ServerConnection.Connected(4)))
        assertFalse(canRequestConnect(ServerConnection.Reconnecting(5, 2)))
    }

    @Test
    fun manualVoiceRequiresPermissionAndCanonicalAuthenticatedSession() {
        val connected = RuntimeState.initial().copy(
            enrollment = EnrollmentReadiness.Ready,
            server = ServerConnection.Connected(4),
            sessionId = "session-1",
        )
        assertTrue(canStartManualVoice(connected, microphonePermissionGranted = true))
        assertFalse(canStartManualVoice(connected, microphonePermissionGranted = false))
        assertFalse(
            canStartManualVoice(
                connected.copy(sessionId = null),
                microphonePermissionGranted = true,
            )
        )
        assertFalse(
            canStartManualVoice(
                connected.copy(server = ServerConnection.Reconnecting(5, 1), sessionId = null),
                microphonePermissionGranted = true,
            )
        )
    }
}
