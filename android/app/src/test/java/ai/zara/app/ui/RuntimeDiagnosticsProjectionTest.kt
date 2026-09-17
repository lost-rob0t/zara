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
import org.junit.Assert.assertTrue
import org.junit.Test

class RuntimeDiagnosticsProjectionTest {
    @Test
    fun `strict local mode reports local backend while remote is disconnected`() {
        val local = LocalServerState(
            phase = LocalServerPhase.READY,
            generation = 1,
            loadedSources = listOf("core", "workspace", "user"),
        )
        val remote = RuntimeState.initial().copy(
            enrollment = EnrollmentReadiness.Ready,
            assistantRole = AssistantRole.NotHeld,
            server = ServerConnection.Disconnected,
            sessionId = null,
        )

        assertEquals("local", activeRuntimeBackendLabel(RuntimeMode.Local, local, remote))
    }

    @Test
    fun `auto mode reports local fallback when remote is disconnected`() {
        val local = LocalServerState(
            phase = LocalServerPhase.READY,
            generation = 1,
            loadedSources = listOf("core"),
        )
        val remote = RuntimeState.initial().copy(
            enrollment = EnrollmentReadiness.Ready,
            server = ServerConnection.Disconnected,
        )

        assertEquals("local fallback", activeRuntimeBackendLabel(RuntimeMode.Auto, local, remote))
    }

    @Test
    fun `auto mode reports remote only for a canonical authenticated session`() {
        val local = LocalServerState(
            phase = LocalServerPhase.READY,
            generation = 1,
            loadedSources = listOf("core"),
        )
        val remote = RuntimeState.initial().copy(
            enrollment = EnrollmentReadiness.Ready,
            server = ServerConnection.Connected(8),
            sessionId = "session-8",
        )

        assertEquals("remote", activeRuntimeBackendLabel(RuntimeMode.Auto, local, remote))
    }

    @Test
    fun `diagnostics separates local and remote cards instead of presenting remote disconnect as runtime failure`() {
        val source = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val diagnostics = source.substringAfter("private fun DiagnosticsSurface(")
            .substringBefore("private fun ThemesSurface(")

        assertTrue(diagnostics.contains("SectionCard(\"RUNTIME\")"))
        assertTrue(diagnostics.contains("KeyValueRow(\"active backend\""))
        assertTrue(diagnostics.contains("SectionCard(\"LOCAL\")"))
        assertTrue(diagnostics.contains("SectionCard(\"REMOTE\")"))
        assertTrue(diagnostics.contains("Remote state is informational in Local mode"))
    }
}
