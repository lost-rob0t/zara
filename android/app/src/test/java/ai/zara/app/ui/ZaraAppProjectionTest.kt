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
    @Test
    fun allFrozenThemesResolveThroughOneSemanticHierarchy() {
        assertEquals(listOf("Outrun", "StarIntel", "Midnight", "Terminal", "Light", "System"),
            ZaraTheme.entries.map { it.name })
        ZaraTheme.entries.forEach { theme ->
            val tokens = themeTokens(theme, systemDark = true, reducedGlow = false)
            assertTrue(tokens.text != tokens.background)
            assertTrue(tokens.textMuted != tokens.surface)
            assertTrue(tokens.success != tokens.error)
            assertTrue(tokens.focus != tokens.background)
        }
        assertEquals(themeTokens(ZaraTheme.Outrun, true, false), themeTokens(ZaraTheme.System, true, false))
        assertEquals(themeTokens(ZaraTheme.Light, false, false), themeTokens(ZaraTheme.System, false, false))
    }

    @Test
    fun reducedGlowPreservesFocusAndSelectionContrast() {
        ZaraTheme.entries.forEach { theme ->
            val normal = themeTokens(theme, true, false)
            val reduced = themeTokens(theme, true, true)
            assertEquals(androidx.compose.ui.graphics.Color.Transparent, reduced.ambientGlow)
            assertEquals(normal.focus, reduced.focus)
            assertEquals(normal.borderActive, reduced.borderActive)
            assertEquals(normal.text, reduced.text)
        }
    }

    @Test
    fun themesSurfaceRendersAppearancePreviewCardsForAllFrozenThemes() {
        val source = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val themes = source.substringAfter("private fun ThemesSurface(")
            .substringBefore("private fun GatedSurface(")

        assertTrue(themes.contains("\"Appearance\""))
        assertTrue(themes.contains("ZaraTheme.entries"))
        assertTrue(themes.contains("themeTokens(theme"))
        ZaraTheme.entries.forEach { theme ->
            assertTrue("preview card missing selection for ${theme.name}", themes.contains("onSelectTheme"))
        }
        assertTrue(source.contains("AppSurface.Themes -> ThemesSurface("))
    }

    @Test
    fun activeShellResolvesTokensFromSelectionInsteadOfHardcodedOutrun() {
        val source = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val shell = source.substringAfter("fun ZaraApp(")
            .substringBefore("private fun ZaraTopBar(")

        assertTrue(shell.contains("themeTokens("))
        assertTrue(shell.contains("isSystemInDarkTheme()"))
        assertTrue(shell.contains("LocalZaraTokens provides"))
        assertTrue(shell.contains("selectedTheme"))

        val sectionCard = source.substringAfter("private fun SectionCard(")
            .substringBefore("private fun KeyValueRow(")
        assertFalse(sectionCard.contains("OutrunTokens"))
        assertTrue(sectionCard.contains("LocalZaraTokens"))
    }

    @Test
    fun hostRestoresThemePreferenceAcrossProcessRecreation() {
        val source = File("src/main/java/ai/zara/app/MainActivity.kt").readText()

        assertTrue(source.contains("ThemePreferenceStore"))
        assertTrue(source.contains("selectedTheme"))
        assertTrue(source.contains("onSelectTheme"))
    }

    @Test
    fun shellHonorsSystemBarsInsteadOfDrawingUnderTheStatusBar() {
        val source = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val topBar = source.substringAfter("private fun ZaraTopBar(")
            .substringBefore("private fun ZaraDrawer(")

        assertTrue(topBar.contains("WindowInsets.statusBars"))
        assertTrue(topBar.contains("windowInsetsPadding"))
    }

    @Test
    fun drawerNavigationScrollsSoEveryRouteStaysReachable() {
        val source = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val drawer = source.substringAfter("private fun ZaraDrawer(")
            .substringBefore("private fun DrawerDividerLabel(")

        assertTrue(drawer.contains("verticalScroll(rememberScrollState())"))
    }

    @Test
    fun systemBarIconContrastFollowsResolvedThemeDarkness() {
        val source = File("src/main/java/ai/zara/app/MainActivity.kt").readText()

        assertTrue(source.contains("enableEdgeToEdge"))
        assertTrue(source.contains("SystemBarStyle"))
        assertTrue(source.contains("resolvedSystemBarDark"))
        assertTrue(source.contains("ZaraTheme.System -> systemDark"))
        assertTrue(source.contains("ZaraTheme.Light -> false"))
    }
}
