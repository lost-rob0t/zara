package ai.zara.ui.theme

import androidx.compose.ui.graphics.Color
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotEquals
import org.junit.Test

class ZaraThemeTest {
    @Test
    fun frozenThemeInventoryAndSystemResolutionStayStable() {
        assertEquals(
            listOf("Outrun", "StarIntel", "Midnight", "Terminal", "Light", "System"),
            ZaraTheme.entries.map { it.name },
        )
        assertEquals(
            themeTokens(ZaraTheme.Outrun, systemDark = true, reducedGlow = false),
            themeTokens(ZaraTheme.System, systemDark = true, reducedGlow = false),
        )
        assertEquals(
            themeTokens(ZaraTheme.Light, systemDark = false, reducedGlow = false),
            themeTokens(ZaraTheme.System, systemDark = false, reducedGlow = false),
        )
    }

    @Test
    fun everyThemeMaintainsSemanticContrastInDarkSystemContext() {
        ZaraTheme.entries.forEach { theme ->
            val tokens = themeTokens(theme, systemDark = true, reducedGlow = false)
            assertNotEquals(tokens.text, tokens.background)
            assertNotEquals(tokens.textMuted, tokens.surface)
            assertNotEquals(tokens.success, tokens.error)
            assertNotEquals(tokens.focus, tokens.background)
        }
    }

    @Test
    fun reducedGlowOnlyRemovesAmbientGlow() {
        ZaraTheme.entries.forEach { theme ->
            val normal = themeTokens(theme, systemDark = true, reducedGlow = false)
            val reduced = themeTokens(theme, systemDark = true, reducedGlow = true)
            assertEquals(Color.Transparent, reduced.ambientGlow)
            assertEquals(normal.focus, reduced.focus)
            assertEquals(normal.borderActive, reduced.borderActive)
            assertEquals(normal.text, reduced.text)
            assertEquals(normal.background, reduced.background)
        }
    }

    @Test
    fun outrunAuthorityKeepsFrozenCoreColors() {
        val tokens = themeTokens(ZaraTheme.Outrun, systemDark = true, reducedGlow = false)
        assertEquals(Color(0xFF02040B), tokens.background)
        assertEquals(Color(0xFFE21CF2), tokens.primary)
        assertEquals(Color(0xFF16D9FF), tokens.secondary)
        assertEquals(Color(0xFFF000FF), tokens.accentMagenta)
        assertEquals(Color(0xFF00D7FF), tokens.accentCyan)
    }
}
