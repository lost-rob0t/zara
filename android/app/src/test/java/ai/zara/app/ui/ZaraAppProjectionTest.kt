package ai.zara.app.ui

import ai.zara.app.runtime.AssistantRole
import ai.zara.app.runtime.EnrollmentReadiness
import ai.zara.app.runtime.RuntimeState
import ai.zara.app.runtime.ServerConnection
import java.io.File
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
    fun samsungAssistantGuidanceIsBoundedAndDoesNotClaimHardwareProof() {
        val guidance = samsungAssistantSetupGuidance()

        assertTrue(guidance.contains("Settings > Apps > Choose default apps > Digital assistant app"))
        assertTrue(guidance.contains("Settings > Advanced features > Side button > Long press"))
        assertTrue(guidance.contains("supported One UI"))
        assertTrue(guidance.contains("hardware verification remains pending"))
    }

    @Test
    fun assistantSetupGuidanceIsActuallyRenderedInSettingsWhenRoleIsMissing() {
        val source = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val missingRoleBlock = source.substringAfter("AssistantRole.NotHeld -> {")
            .substringBefore("AssistantRole.PlatformUnavailable")

        assertTrue(missingRoleBlock.contains("samsungAssistantSetupGuidance()"))
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

    @Test
    fun frozenDrawerRouteInventoryReplacesBottomNavigation() {
        val source = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val expected = listOf(
            "Chat", "Logic", "Voice", "Projects", "Remote", "Scheduled",
            "Plugins", "Themes", "Diagnostics", "Settings", "About",
        )
        var cursor = -1
        expected.forEach { label ->
            val next = source.indexOf("\"$label\"", cursor + 1)
            assertTrue("missing or out-of-order drawer route: $label", next > cursor)
            cursor = next
        }
        assertTrue(source.contains("ModalNavigationDrawer"))
        assertFalse(source.contains("NavigationBarItem"))
    }

    @Test
    fun outrunShellUsesSemanticTokensAndCompactComposer() {
        val source = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()

        assertTrue(source.contains("data class ZaraSemanticTokens"))
        assertTrue(source.contains("accentMagenta"))
        assertTrue(source.contains("accentCyan"))
        assertTrue(source.contains("ambientGlow"))
        assertTrue(source.contains("CompactComposer"))
        assertTrue(source.contains("ZaraSigil"))
    }

    @Test
    fun settingsDoesNotDumpLongAssistantSetupTextByDefault() {
        val source = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val settings = source.substringAfter("private fun SettingsSurface(")
            .substringBefore("private fun DiagnosticsSurface(")

        assertTrue(settings.contains("showAssistantHelp"))
        assertTrue(settings.contains("SelectionContainer"))
    }

    @Test
    fun androidHostDoesNotRenderPlatformLightActionBarOverComposeShell() {
        val manifest = File("src/main/AndroidManifest.xml").readText()

        assertFalse(manifest.contains("Theme.DeviceDefault.Light"))
        assertTrue(manifest.contains("Theme.DeviceDefault.NoActionBar"))
    }
}
