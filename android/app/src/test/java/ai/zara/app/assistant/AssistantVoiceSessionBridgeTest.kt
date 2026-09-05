package ai.zara.app.assistant

import ai.zara.app.runtime.AssistantRole
import ai.zara.app.runtime.EnrollmentReadiness
import ai.zara.app.runtime.RuntimeState
import ai.zara.app.runtime.ServerConnection
import ai.zara.app.runtime.ServerProfile
import org.junit.Assert.assertEquals
import org.junit.Test

class AssistantVoiceSessionBridgeTest {
    @Test
    fun `assistant voice rejects missing microphone permission`() {
        val state = readyState(ServerConnection.Connected(4), sessionId = "session-1")

        assertEquals(
            AssistantVoiceStartPlan.Reject("microphone permission is required"),
            planAssistantVoiceStart(state, microphonePermissionGranted = false),
        )
    }

    @Test
    fun `assistant voice rejects when Zara does not hold assistant role`() {
        val state = readyState(ServerConnection.Connected(4), sessionId = "session-1")
            .copy(assistantRole = AssistantRole.NotHeld)

        assertEquals(
            AssistantVoiceStartPlan.Reject("Zara does not hold the Android Assistant role"),
            planAssistantVoiceStart(state, microphonePermissionGranted = true),
        )
    }

    @Test
    fun `assistant voice starts immediately only on canonical connected session`() {
        val state = readyState(ServerConnection.Connected(4), sessionId = "session-1")

        assertEquals(
            AssistantVoiceStartPlan.StartNow,
            planAssistantVoiceStart(state, microphonePermissionGranted = true),
        )
    }

    @Test
    fun `process restored disconnected state reconnects from durable profile without fabricating session`() {
        val profile = ServerProfile.create("tcp://127.0.0.1:5555")
        val state = readyState(ServerConnection.Disconnected, sessionId = null)
            .copy(configuredProfile = profile)

        assertEquals(
            AssistantVoiceStartPlan.Reconnect(profile.endpoint),
            planAssistantVoiceStart(state, microphonePermissionGranted = true),
        )
    }

    @Test
    fun `assistant voice rejects transient reconnect rather than starting parallel connection`() {
        val state = readyState(ServerConnection.Reconnecting(5, 2), sessionId = null)

        assertEquals(
            AssistantVoiceStartPlan.Reject("Zara connection is not ready"),
            planAssistantVoiceStart(state, microphonePermissionGranted = true),
        )
    }

    private fun readyState(server: ServerConnection, sessionId: String?): RuntimeState =
        RuntimeState.initial().copy(
            enrollment = EnrollmentReadiness.Ready,
            assistantRole = AssistantRole.Held,
            server = server,
            sessionId = sessionId,
            generation = 4,
        )
}
