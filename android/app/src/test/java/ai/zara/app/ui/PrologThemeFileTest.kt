package ai.zara.app.ui

import ai.zara.ui.theme.ZaraTheme
import ai.zara.ui.theme.themeTokens
import androidx.compose.ui.graphics.Color
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class PrologThemeFileTest {
    @Test
    fun makerProducesImportableEmacsStyleDerivedTheme() {
        val source = PrologThemeFile.make(
            id = "red_room",
            displayName = "Red Room",
            parent = ZaraTheme.OutrunOled,
        ) + "\nzara_theme_face(red_room, primary, '#ff003c').\n"

        val imported = PrologThemeFile.import(source)
        val parent = themeTokens(ZaraTheme.OutrunOled, systemDark = true, reducedGlow = false)

        assertEquals("red_room", imported.id)
        assertEquals("Red Room", imported.displayName)
        assertEquals(ZaraTheme.OutrunOled, imported.parent)
        assertEquals(Color.Black, imported.tokens.background)
        assertEquals(Color(0xFFFF003C), imported.tokens.primary)
        assertEquals(parent.secondary, imported.tokens.secondary)
        assertEquals(parent.text, imported.tokens.text)
    }

    @Test
    fun importerReadsOnlyThemeExportsAndAllowsOtherPrologCode() {
        val source = """
            helper(X) :- X = ok.
            zara_theme(my_theme, 'My Theme').
            zara_theme_parent(my_theme, terminal_oled).
            zara_theme_face(my_theme, accent_cyan, '#12aaff').
            another_fact(42).
        """.trimIndent()

        val imported = PrologThemeFile.import(source)

        assertEquals(ZaraTheme.TerminalOled, imported.parent)
        assertEquals(Color(0xFF12AAFF), imported.tokens.accentCyan)
        assertEquals(Color.Black, imported.tokens.background)
    }

    @Test
    fun importerSupportsArgbForTransparentAmbientGlow() {
        val imported = PrologThemeFile.import(
            """
                zara_theme(blackout, 'Blackout').
                zara_theme_parent(blackout, midnight_oled).
                zara_theme_face(blackout, ambient_glow, '#00000000').
            """.trimIndent()
        )

        assertEquals(Color.Transparent, imported.tokens.ambientGlow)
    }

    @Test(expected = IllegalArgumentException::class)
    fun importerRejectsUnknownFaces() {
        PrologThemeFile.import(
            """
                zara_theme(bad, 'Bad').
                zara_theme_parent(bad, outrun).
                zara_theme_face(bad, giant_glow_everywhere, '#ffffff').
            """.trimIndent()
        )
    }

    @Test(expected = IllegalArgumentException::class)
    fun importerRejectsSystemAsNonDeterministicParent() {
        PrologThemeFile.import(
            """
                zara_theme(bad, 'Bad').
                zara_theme_parent(bad, system).
            """.trimIndent()
        )
    }

    @Test
    fun makerEscapesQuotedDisplayNames() {
        val source = PrologThemeFile.make("coder", "Coder's OLED")
        assertTrue(source.contains("zara_theme(coder, 'Coder''s OLED')."))
        assertEquals("Coder's OLED", PrologThemeFile.import(source).displayName)
    }
}
