package ai.zara.app.widget

import ai.zara.ui.theme.ZaraTheme
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

class WidgetStyleCompilerTest {
    private val semanticColors = WidgetSemanticPaletteResolver { _, role ->
        0xFF000000.toInt() or (role.ordinal + 1)
    }
    private val compiler = WidgetStyleCompiler(semanticColors)

    @Test
    fun `complete Prolog stylesheet controls every exposed visual property`() {
        val sheet = compiler.compile(
            source = completeStyle,
            selectedTheme = ZaraTheme.Outrun,
            systemDark = true,
        )

        assertEquals("night_ops", sheet.name)
        assertEquals(ZaraTheme.Midnight, sheet.theme)
        val assistant = sheet.forWidget(WidgetKind.ASSISTANT)
        assertEquals(0xCC010203.toInt(), assistant.colors.background)
        assertEquals(semanticColors.resolve(ZaraTheme.Midnight, WidgetSemanticColorRole.TEXT), assistant.colors.title)
        assertEquals(19, assistant.metrics.outerPaddingDp)
        assertEquals(23, assistant.metrics.cornerRadiusDp)
        assertEquals(2, assistant.metrics.borderWidthDp)
        assertEquals(17, assistant.metrics.titleSp)
        assertEquals(11, assistant.metrics.bodySp)
        assertEquals(10, assistant.metrics.labelSp)
        assertEquals(12, assistant.metrics.actionSp)
        assertEquals(38, assistant.metrics.sigilSizeDp)
        assertEquals(14, assistant.metrics.actionCornerRadiusDp)
        assertEquals("PRIVATE CORE", assistant.text.eyebrow)
        assertEquals("Night operator", assistant.text.title)
        assertEquals("Rules before models", assistant.text.subtitle)
        assertEquals("ASK", assistant.text.primaryLabel)
        assertFalse(assistant.visibility.showSubtitle)
        assertTrue(assistant.visibility.showSigil)
        assertEquals(WidgetAlignment.CENTER, assistant.alignment)
        assertEquals(WidgetRoute.CHAT, assistant.actions.primary)
        assertEquals(WidgetRoute.VOICE, assistant.actions.secondary)
        assertEquals(WidgetRoute.LOGIC, assistant.actions.tertiary)

        val runtime = sheet.forWidget(WidgetKind.RUNTIME)
        assertEquals(0xFF111213.toInt(), runtime.colors.background)
        assertEquals("Runtime", runtime.text.title)
        assertTrue(runtime.visibility.showStatus)
        assertEquals(WidgetAlignment.START, runtime.alignment)
    }

    @Test
    fun `all selector provides defaults and widget selector overrides only its target`() {
        val source = """
            zara_widget_stylesheet(1).
            widget_style(shared).
            widget_theme(shared, current).
            widget_color(shared, all, background, '#FF010203').
            widget_color(shared, runtime, background, '#FF112233').
            widget_text(shared, all, title, 'Shared').
            widget_text(shared, actions, title, 'Actions only').
        """.trimIndent()

        val sheet = compiler.compile(source, ZaraTheme.Terminal, systemDark = true)

        assertEquals(0xFF010203.toInt(), sheet.forWidget(WidgetKind.ASSISTANT).colors.background)
        assertEquals(0xFF112233.toInt(), sheet.forWidget(WidgetKind.RUNTIME).colors.background)
        assertEquals("Shared", sheet.forWidget(WidgetKind.ASSISTANT).text.title)
        assertEquals("Actions only", sheet.forWidget(WidgetKind.ACTIONS).text.title)
        assertEquals(ZaraTheme.Terminal, sheet.theme)
    }

    @Test
    fun `stylesheet rejects executable Prolog and unknown or duplicate declarations`() {
        listOf(
            "zara_widget_stylesheet(1). widget_style(x) :- shell(evil).",
            "zara_widget_stylesheet(1). :- initialization(main). widget_style(x).",
            "zara_widget_stylesheet(1). widget_style(x). unknown_fact(x).",
            "zara_widget_stylesheet(1). widget_style(x). widget_style(y).",
            "zara_widget_stylesheet(2). widget_style(x).",
            "zara_widget_stylesheet(1). widget_style(x). widget_action(x, all, primary, shell).",
            "zara_widget_stylesheet(1). widget_style(x). widget_metric(x, all, title_sp, 500).",
            "zara_widget_stylesheet(1). widget_style(x). widget_color(x, all, text, red).",
        ).forEach { source ->
            try {
                compiler.compile(source, ZaraTheme.Outrun, systemDark = true)
                fail("Expected stylesheet rejection: $source")
            } catch (_: IllegalArgumentException) {
                Unit
            }
        }
    }

    @Test
    fun `comments quoted punctuation and alpha colors remain valid Prolog data`() {
        val source = """
            % portable user-owned style
            zara_widget_stylesheet(1).
            widget_style(quoted).
            widget_theme(quoted, system).
            widget_color(quoted, all, background, '#7F102030').
            widget_text(quoted, assistant, subtitle, 'Local, private. It\'s yours.').
        """.trimIndent()

        val style = compiler.compile(source, ZaraTheme.Light, systemDark = false)
            .forWidget(WidgetKind.ASSISTANT)

        assertEquals(0x7F102030, style.colors.background)
        assertEquals("Local, private. It's yours.", style.text.subtitle)
    }

    private val completeStyle = """
        zara_widget_stylesheet(1).
        widget_style(night_ops).
        widget_theme(night_ops, midnight).
        widget_color(night_ops, all, background, '#FF111213').
        widget_color(night_ops, assistant, background, '#CC010203').
        widget_color(night_ops, all, surface, semantic(surface)).
        widget_color(night_ops, all, border, semantic(border_active)).
        widget_color(night_ops, all, title, semantic(text)).
        widget_color(night_ops, all, body, semantic(text_muted)).
        widget_color(night_ops, all, label, semantic(accent_cyan)).
        widget_color(night_ops, all, status, semantic(success)).
        widget_color(night_ops, all, action_background, semantic(surface_elevated)).
        widget_color(night_ops, all, action_text, semantic(text)).
        widget_color(night_ops, all, sigil, semantic(accent_magenta)).
        widget_metric(night_ops, all, outer_padding_dp, 19).
        widget_metric(night_ops, all, content_gap_dp, 7).
        widget_metric(night_ops, all, corner_radius_dp, 23).
        widget_metric(night_ops, all, border_width_dp, 2).
        widget_metric(night_ops, all, title_sp, 17).
        widget_metric(night_ops, all, body_sp, 11).
        widget_metric(night_ops, all, label_sp, 10).
        widget_metric(night_ops, all, action_sp, 12).
        widget_metric(night_ops, all, sigil_size_dp, 38).
        widget_metric(night_ops, all, action_corner_radius_dp, 14).
        widget_text(night_ops, assistant, eyebrow, 'PRIVATE CORE').
        widget_text(night_ops, assistant, title, 'Night operator').
        widget_text(night_ops, assistant, subtitle, 'Rules before models').
        widget_text(night_ops, assistant, primary_label, 'ASK').
        widget_flag(night_ops, assistant, show_sigil, true).
        widget_flag(night_ops, assistant, show_subtitle, false).
        widget_flag(night_ops, all, show_status, true).
        widget_alignment(night_ops, all, start).
        widget_alignment(night_ops, assistant, center).
        widget_action(night_ops, assistant, primary, chat).
        widget_action(night_ops, assistant, secondary, voice).
        widget_action(night_ops, assistant, tertiary, logic).
    """.trimIndent()
}
