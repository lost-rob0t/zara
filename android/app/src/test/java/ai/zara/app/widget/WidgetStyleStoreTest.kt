package ai.zara.app.widget

import ai.zara.ui.theme.ZaraTheme
import java.io.File
import java.nio.file.Files
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class WidgetStyleStoreTest {
    private val compiler = WidgetStyleCompiler(WidgetSemanticPaletteResolver { _, _ -> 0xFF010203.toInt() })

    @Test
    fun `validated Prolog import survives a new store instance`() {
        val root = Files.createTempDirectory("zara-widget-style").toFile()
        val file = File(root, "widget-style.pl")
        val source = stylesheet("operator")
        val store = WidgetStyleStore(file, compiler)

        val imported = store.import(source, ZaraTheme.Outrun, systemDark = true)

        assertEquals("operator", imported.name)
        assertEquals(source, WidgetStyleStore(file, compiler).exportSource())
        assertEquals("operator", WidgetStyleStore(file, compiler).load(defaultStyle, ZaraTheme.Outrun, true).name)
    }

    @Test
    fun `invalid replacement never destroys the last green stylesheet`() {
        val root = Files.createTempDirectory("zara-widget-style-rollback").toFile()
        val file = File(root, "widget-style.pl")
        val store = WidgetStyleStore(file, compiler)
        val valid = stylesheet("green")
        store.import(valid, ZaraTheme.Outrun, systemDark = true)

        runCatching {
            store.import("zara_widget_stylesheet(1). widget_style(bad) :- halt.", ZaraTheme.Outrun, true)
        }

        assertEquals(valid, file.readText())
        assertEquals("green", store.load(defaultStyle, ZaraTheme.Outrun, true).name)
    }

    @Test
    fun `missing or corrupt persisted source falls back without rewriting user data`() {
        val root = Files.createTempDirectory("zara-widget-style-fallback").toFile()
        val file = File(root, "widget-style.pl")
        val store = WidgetStyleStore(file, compiler)

        assertEquals("builtin", store.load(defaultStyle, ZaraTheme.Outrun, true).name)
        file.writeText("not prolog")
        assertEquals("builtin", store.load(defaultStyle, ZaraTheme.Outrun, true).name)
        assertEquals("not prolog", file.readText())
    }

    @Test
    fun `reset removes only imported stylesheet`() {
        val root = Files.createTempDirectory("zara-widget-style-reset").toFile()
        val file = File(root, "widget-style.pl")
        val store = WidgetStyleStore(file, compiler)
        store.import(stylesheet("custom"), ZaraTheme.Outrun, true)

        assertTrue(store.hasImport())
        store.reset()
        assertFalse(store.hasImport())
        assertEquals("builtin", store.load(defaultStyle, ZaraTheme.Outrun, true).name)
    }

    private fun stylesheet(name: String) = """
        zara_widget_stylesheet(1).
        widget_style($name).
        widget_theme($name, current).
    """.trimIndent()

    private val defaultStyle = stylesheet("builtin")
}
