package ai.zara.app.assistant

import ai.zara.app.runtime.AssistantRole
import ai.zara.app.runtime.EnrollmentReadiness
import ai.zara.app.runtime.LocalServerPhase
import ai.zara.app.runtime.LocalServerState
import ai.zara.app.runtime.RuntimeMode
import ai.zara.app.runtime.RuntimeState
import ai.zara.app.runtime.ServerConnection
import org.junit.Assert.assertEquals
import org.junit.Test

class AssistantCapturePlanTest {
    @Test
    fun `remote provider can use local STT and TTS when Zara server is absent`() {
        val state = RuntimeState.initial().copy(
            enrollment = EnrollmentReadiness.Unenrolled,
            assistantRole = AssistantRole.Held,
            server = ServerConnection.Disconnected,
            sessionId = null,
        )

        assertEquals(
            AssistantCapturePlan.Local,
            planAssistantCapture(
                mode = RuntimeMode.Remote,
                localState = readyLocalState(),
                runtimeState = state,
                remoteModelReady = true,
            ),
        )
    }

    @Test
    fun `symbolic mode uses embedded runtime without model or remote requirements`() {
        val state = RuntimeState.initial().copy(
            enrollment = EnrollmentReadiness.Unenrolled,
            assistantRole = AssistantRole.Held,
            server = ServerConnection.Disconnected,
            sessionId = null,
        )

        assertEquals(
            AssistantCapturePlan.Local,
            planAssistantCapture(
                mode = RuntimeMode.Symbolic,
                localState = readyLocalState(),
                runtimeState = state,
            ),
        )
    }

    @Test
    fun `strict local mode uses embedded runtime without remote enrollment or connection`() {
        val state = RuntimeState.initial().copy(
            enrollment = EnrollmentReadiness.Unenrolled,
            assistantRole = AssistantRole.Held,
            server = ServerConnection.Disconnected,
            sessionId = null,
        )

        assertEquals(
            AssistantCapturePlan.Local,
            planAssistantCapture(
                mode = RuntimeMode.Local,
                localState = readyLocalState(),
                runtimeState = state,
            ),
        )
    }

    @Test
    fun `auto mode falls back to embedded runtime while remote is disconnected`() {
        val state = RuntimeState.initial().copy(
            assistantRole = AssistantRole.Held,
            server = ServerConnection.Disconnected,
            sessionId = null,
        )

        assertEquals(
            AssistantCapturePlan.Local,
            planAssistantCapture(
                mode = RuntimeMode.Auto,
                localState = readyLocalState(),
                runtimeState = state,
            ),
        )
    }

    @Test
    fun `auto mode keeps authenticated remote session when one is active`() {
        val state = RuntimeState.initial().copy(
            enrollment = EnrollmentReadiness.Ready,
            assistantRole = AssistantRole.Held,
            generation = 7,
            server = ServerConnection.Connected(7),
            sessionId = "session-7",
        )

        assertEquals(
            AssistantCapturePlan.Remote,
            planAssistantCapture(
                mode = RuntimeMode.Auto,
                localState = readyLocalState(),
                runtimeState = state,
            ),
        )
    }

    @Test
    fun `remote capture requires enrollment even when transport and session look connected`() {
        val state = RuntimeState.initial().copy(
            enrollment = EnrollmentReadiness.Unenrolled,
            assistantRole = AssistantRole.Held,
            generation = 7,
            server = ServerConnection.Connected(7),
            sessionId = "session-7",
        )

        assertEquals(
            AssistantCapturePlan.Reject("Remote Zara session is not ready"),
            planAssistantCapture(
                mode = RuntimeMode.Remote,
                localState = readyLocalState(),
                runtimeState = state,
            ),
        )
        assertEquals(
            AssistantCapturePlan.Local,
            planAssistantCapture(
                mode = RuntimeMode.Auto,
                localState = readyLocalState(),
                runtimeState = state,
            ),
        )
    }

    @Test
    fun `remote capture rejects blank authenticated session and auto falls back local`() {
        val state = RuntimeState.initial().copy(
            enrollment = EnrollmentReadiness.Ready,
            assistantRole = AssistantRole.Held,
            generation = 7,
            server = ServerConnection.Connected(7),
            sessionId = "   ",
        )

        assertEquals(
            AssistantCapturePlan.Reject("Remote Zara session is not ready"),
            planAssistantCapture(
                mode = RuntimeMode.Remote,
                localState = readyLocalState(),
                runtimeState = state,
            ),
        )
        assertEquals(
            AssistantCapturePlan.Local,
            planAssistantCapture(
                mode = RuntimeMode.Auto,
                localState = readyLocalState(),
                runtimeState = state,
            ),
        )
    }

    @Test
    fun `remote capture rejects stale connected generation and auto falls back local`() {
        val state = RuntimeState.initial().copy(
            enrollment = EnrollmentReadiness.Ready,
            assistantRole = AssistantRole.Held,
            generation = 8,
            server = ServerConnection.Connected(7),
            sessionId = "session-7",
        )

        assertEquals(
            AssistantCapturePlan.Reject("Remote Zara session is not ready"),
            planAssistantCapture(
                mode = RuntimeMode.Remote,
                localState = readyLocalState(),
                runtimeState = state,
            ),
        )
        assertEquals(
            AssistantCapturePlan.Local,
            planAssistantCapture(
                mode = RuntimeMode.Auto,
                localState = readyLocalState(),
                runtimeState = state,
            ),
        )
    }

    @Test
    fun `remote mode rejects capture without authenticated remote session`() {
        val state = RuntimeState.initial().copy(
            enrollment = EnrollmentReadiness.Unenrolled,
            assistantRole = AssistantRole.Held,
            server = ServerConnection.Disconnected,
            sessionId = null,
        )

        assertEquals(
            AssistantCapturePlan.Reject("Remote Zara session is not ready"),
            planAssistantCapture(
                mode = RuntimeMode.Remote,
                localState = readyLocalState(),
                runtimeState = state,
            ),
        )
    }

    @Test
    fun `auto mode rejects capture when neither backend is ready`() {
        val state = RuntimeState.initial().copy(
            enrollment = EnrollmentReadiness.Unenrolled,
            assistantRole = AssistantRole.Held,
            server = ServerConnection.Disconnected,
            sessionId = null,
        )

        assertEquals(
            AssistantCapturePlan.Reject("No Zara runtime is ready"),
            planAssistantCapture(
                mode = RuntimeMode.Auto,
                localState = LocalServerState(
                    phase = LocalServerPhase.STARTING,
                    generation = 0,
                    loadedSources = emptyList(),
                ),
                runtimeState = state,
            ),
        )
    }

    @Test
    fun `strict local mode fails honestly when embedded runtime is not ready`() {
        val state = RuntimeState.initial().copy(assistantRole = AssistantRole.Held)

        assertEquals(
            AssistantCapturePlan.Reject("Local Zara server is not ready"),
            planAssistantCapture(
                mode = RuntimeMode.Local,
                localState = LocalServerState(
                    phase = LocalServerPhase.STARTING,
                    generation = 0,
                    loadedSources = emptyList(),
                ),
                runtimeState = state,
            ),
        )
    }

    @Test
    fun `assistant role remains required for local capture`() {
        val state = RuntimeState.initial().copy(assistantRole = AssistantRole.NotHeld)

        assertEquals(
            AssistantCapturePlan.Reject("Zara does not hold the Android Assistant role"),
            planAssistantCapture(
                mode = RuntimeMode.Local,
                localState = readyLocalState(),
                runtimeState = state,
            ),
        )
    }

    private fun readyLocalState() = LocalServerState(
        phase = LocalServerPhase.READY,
        generation = 1,
        loadedSources = listOf("core", "workspace"),
    )
}
