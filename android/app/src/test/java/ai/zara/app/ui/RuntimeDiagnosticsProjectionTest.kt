package ai.zara.app.ui

import ai.zara.app.runtime.AssistantRole
import ai.zara.app.runtime.EnrollmentReadiness
import ai.zara.app.runtime.LocalServerPhase
import ai.zara.app.runtime.LocalServerState
import ai.zara.app.runtime.RuntimeMode
import ai.zara.app.runtime.RuntimeState
import ai.zara.app.runtime.ServerConnection
import java.io.File
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class RuntimeDiagnosticsProjectionTest {
    private val localReady = LocalServerState(
        phase = LocalServerPhase.READY,
        generation = 1,
        loadedSources = listOf("core", "workspace", "user"),
    )

    @Test
    fun `strict local mode reports local backend while remote is disconnected`() {
        val remote = RuntimeState.initial().copy(
            enrollment = EnrollmentReadiness.Ready,
            assistantRole = AssistantRole.NotHeld,
            server = ServerConnection.Disconnected,
            sessionId = null,
        )

        val projection = runtimeUiProjection(RuntimeMode.Local, localReady, remote)

        assertEquals("local", projection.backendLabel)
        assertTrue(projection.chatReady)
        assertTrue(projection.remoteInformational)
    }

    @Test
    fun `strict local mode never borrows authenticated remote readiness while local runtime is starting`() {
        val localStarting = localReady.copy(phase = LocalServerPhase.STARTING)
        val remote = RuntimeState.initial().copy(
            enrollment = EnrollmentReadiness.Ready,
            generation = 7,
            server = ServerConnection.Connected(7),
            sessionId = "remote-session",
        )

        val projection = runtimeUiProjection(RuntimeMode.Local, localStarting, remote)

        assertEquals("local (starting)", projection.backendLabel)
        assertFalse(projection.chatReady)
        assertTrue(projection.remoteInformational)
    }

    @Test
    fun `auto mode reports local fallback when remote is disconnected`() {
        val remote = RuntimeState.initial().copy(
            enrollment = EnrollmentReadiness.Ready,
            server = ServerConnection.Disconnected,
        )

        val projection = runtimeUiProjection(RuntimeMode.Auto, localReady, remote)

        assertEquals("local fallback", projection.backendLabel)
        assertTrue(projection.chatReady)
        assertFalse(projection.remoteInformational)
    }

    @Test
    fun `auto mode falls back locally when connected remote generation is stale`() {
        val remote = RuntimeState.initial().copy(
            enrollment = EnrollmentReadiness.Ready,
            generation = 12,
            server = ServerConnection.Connected(11),
            sessionId = "session-11",
        )

        val projection = runtimeUiProjection(RuntimeMode.Auto, localReady, remote)

        assertEquals("local fallback", projection.backendLabel)
        assertTrue(projection.chatReady)
        assertFalse(projection.remoteInformational)
    }

    @Test
    fun `auto mode reports remote only for a canonical authenticated session`() {
        val remote = RuntimeState.initial().copy(
            enrollment = EnrollmentReadiness.Ready,
            generation = 8,
            server = ServerConnection.Connected(8),
            sessionId = "session-8",
        )

        val projection = runtimeUiProjection(RuntimeMode.Auto, localReady, remote)

        assertEquals("remote", projection.backendLabel)
        assertTrue(projection.chatReady)
    }

    @Test
    fun `remote mode never becomes ready from local readiness alone`() {
        val remote = RuntimeState.initial().copy(
            enrollment = EnrollmentReadiness.Ready,
            server = ServerConnection.Disconnected,
            sessionId = null,
        )

        val projection = runtimeUiProjection(RuntimeMode.Remote, localReady, remote)

        assertEquals("remote (not ready)", projection.backendLabel)
        assertFalse(projection.chatReady)
    }

    @Test
    fun `connected socket without authenticated session is not projected ready`() {
        val remote = RuntimeState.initial().copy(
            enrollment = EnrollmentReadiness.Ready,
            generation = 9,
            server = ServerConnection.Connected(9),
            sessionId = null,
        )

        val projection = runtimeUiProjection(RuntimeMode.Remote, localReady, remote)

        assertEquals("remote (not ready)", projection.backendLabel)
        assertFalse(projection.chatReady)
    }

    @Test
    fun `connected authenticated session without enrollment readiness is not projected ready`() {
        val remote = RuntimeState.initial().copy(
            enrollment = EnrollmentReadiness.AwaitingServerPin,
            generation = 9,
            server = ServerConnection.Connected(9),
            sessionId = "session-9",
        )

        val projection = runtimeUiProjection(RuntimeMode.Remote, localReady, remote)

        assertEquals("remote (not ready)", projection.backendLabel)
        assertFalse(projection.chatReady)
    }

    @Test
    fun `blank authenticated session id is not projected ready`() {
        val remote = RuntimeState.initial().copy(
            enrollment = EnrollmentReadiness.Ready,
            generation = 10,
            server = ServerConnection.Connected(10),
            sessionId = "   ",
        )

        val projection = runtimeUiProjection(RuntimeMode.Remote, localReady, remote)

        assertEquals("remote (not ready)", projection.backendLabel)
        assertFalse(projection.chatReady)
    }

    @Test
    fun `stale connected generation is not projected ready`() {
        val remote = RuntimeState.initial().copy(
            enrollment = EnrollmentReadiness.Ready,
            generation = 11,
            server = ServerConnection.Connected(10),
            sessionId = "session-10",
        )

        val projection = runtimeUiProjection(RuntimeMode.Remote, localReady, remote)

        assertEquals("remote (not ready)", projection.backendLabel)
        assertFalse(projection.chatReady)
    }

    @Test
    fun `chat consumes canonical runtime projection instead of reconstructing readiness`() {
        val source = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val chat = source.substringAfter("private fun ChatSurface(")
            .substringBefore("private fun CompactComposer(")

        assertTrue(chat.contains("runtimeUiProjection(runtimeMode, localServerState, state)"))
        assertTrue(chat.contains("projection.chatReady"))
        assertTrue(chat.contains("projection.backendLabel"))
        assertFalse(chat.contains("val remoteReady ="))
    }

    @Test
    fun `diagnostics separates local and remote cards instead of presenting remote disconnect as runtime failure`() {
        val source = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val diagnostics = source.substringAfter("private fun DiagnosticsSurface(")
            .substringBefore("private fun ThemesSurface(")

        assertTrue(diagnostics.contains("runtimeUiProjection(runtimeMode, localServerState, state)"))
        assertTrue(diagnostics.contains("SectionCard(\"RUNTIME\")"))
        assertTrue(diagnostics.contains("KeyValueRow(\"active backend\""))
        assertTrue(diagnostics.contains("SectionCard(\"LOCAL\")"))
        assertTrue(diagnostics.contains("SectionCard(\"REMOTE\")"))
        assertTrue(diagnostics.contains("Remote state is informational in Local mode"))
    }

    @Test
    fun `strict local mode ignores a genuinely authenticated current remote session`() {
        val remote = RuntimeState.initial().copy(
            enrollment = EnrollmentReadiness.Ready,
            generation = 42,
            server = ServerConnection.Connected(42),
            sessionId = "remote-session",
        )

        val projection = runtimeUiProjection(RuntimeMode.Local, localReady, remote)

        assertEquals("local", projection.backendLabel)
        assertTrue(projection.chatReady)
        assertTrue(projection.remoteInformational)
    }

    @Test
    fun `runtime settings keeps Auto Local and Remote user switchable and persisted`() {
        val source = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val runtimeSettings = source.substringAfter("AppRoute.Runtime ->")
            .substringBefore("AppRoute.Permissions ->")
        val activity = File("src/main/java/ai/zara/app/MainActivity.kt").readText()

        assertTrue(runtimeSettings.contains("RuntimeMode.entries.forEach"))
        assertTrue(runtimeSettings.contains("onSelectRuntimeMode(mode)"))
        assertTrue(runtimeSettings.contains("Runtime mode \${mode.name}; \${if (mode == runtimeMode) \"selected\" else \"not selected\"}"))
        assertTrue(runtimeSettings.contains("No account, server, or cloud fallback"))
        assertTrue(runtimeSettings.contains("fail closed when unavailable"))
        assertTrue(activity.contains("runtimeModeStore.save(mode)"))
        assertTrue(activity.contains("appSession.setRuntimeMode(mode)"))
    }

    @Test
    fun `runtime settings renders canonical local model truth without invented readiness`() {
        val source = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val runtimeSettings = source.substringAfter("AppRoute.Runtime ->")
            .substringBefore("AppRoute.Permissions ->")
        val activity = File("src/main/java/ai/zara/app/MainActivity.kt").readText()

        assertTrue(runtimeSettings.contains("val localModel = localAiState?.model"))
        assertTrue(runtimeSettings.contains("localAiState?.phase?.name?.lowercase() ?: \"unavailable\""))
        assertTrue(runtimeSettings.contains("localModel?.format?.wireName ?: \"none\""))
        assertTrue(runtimeSettings.contains("localModel?.quantization?.wireName ?: \"none\""))
        assertTrue(runtimeSettings.contains("localModel?.backend?.name?.lowercase() ?: \"none\""))
        assertTrue(runtimeSettings.contains("Zara never invents local-model readiness"))
        assertTrue(activity.contains("appSession.localAiState()"))
        assertTrue(activity.contains("onRefreshLocalAiState = ::refreshLocalAiState"))
    }

    @Test
    fun `chat status distinguishes symbolic local model remote and degraded states`() {
        val source = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val chat = source.substringAfter("private fun ChatSurface(")
            .substringBefore("private fun CompactComposer(")

        assertTrue(chat.contains("Offline · Symbolic"))
        assertTrue(chat.contains("Offline · Local model"))
        assertTrue(chat.contains("Online · Remote"))
        assertTrue(chat.contains("Connecting…"))
        assertTrue(chat.contains("Degraded"))
        assertTrue(chat.contains("localAiState?.model"))
        assertTrue(chat.contains("Runtime status \$runtimeStatus"))
    }
}
