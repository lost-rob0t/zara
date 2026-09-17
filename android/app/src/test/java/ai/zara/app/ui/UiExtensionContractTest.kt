package ai.zara.app.ui

import ai.zara.app.ui.extensions.PortablePythonUiInitParser
import ai.zara.app.ui.extensions.UiContribution
import ai.zara.app.ui.extensions.UiContributionKind
import ai.zara.app.ui.extensions.UiExtensionRegistry
import ai.zara.app.ui.extensions.UiPlatform
import ai.zara.app.ui.extensions.UiSlot
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFailsWith
import org.junit.Test

class UiExtensionContractTest {
    @Test
    fun registryLayersOwnersAndFiltersAndroidDeterministically() {
        val registry = UiExtensionRegistry()
        registry.replaceOwner(
            "plugin:notes",
            listOf(
                UiContribution(
                    id = "notes",
                    slot = UiSlot.DRAWER,
                    kind = UiContributionKind.SURFACE,
                    label = "Notes",
                    action = "submit:open notes",
                    priority = 40,
                    platforms = setOf(UiPlatform.DESKTOP, UiPlatform.ANDROID),
                ),
                UiContribution(
                    id = "desktop-only",
                    slot = UiSlot.CHAT_TOP,
                    kind = UiContributionKind.TEXT,
                    label = "desktop",
                    priority = 1,
                    platforms = setOf(UiPlatform.DESKTOP),
                ),
            ),
        )

        assertEquals(listOf("notes"), registry.forPlatform(UiPlatform.ANDROID).map { it.id })
        assertEquals(
            listOf("desktop-only", "notes"),
            registry.forPlatform(UiPlatform.DESKTOP).map { it.id },
        )
    }

    @Test
    fun portablePythonInitUsesDesktopUiAddSyntax() {
        val source = """
            def register(ui):
                ui.add("logic", "drawer", "surface", "My Logic", "route:logic", 20, ["desktop", "android"])
                ui.add("query", "chat.top", "button", "Query", "submit:? - demo(Result)", 5, ["android"])
        """.trimIndent()

        val parsed = PortablePythonUiInitParser.parse(source)
        assertEquals(listOf("query", "logic"), parsed.map { it.id })
        assertEquals(setOf(UiPlatform.ANDROID), parsed.first().platforms)
    }

    @Test
    fun portablePythonInitRejectsArbitraryExecutablePythonOnAndroid() {
        val source = """
            def register(ui):
                import os
                ui.add("bad", "drawer", "text", "Bad", "", 10, ["android"])
        """.trimIndent()

        assertFailsWith<IllegalArgumentException> {
            PortablePythonUiInitParser.parse(source)
        }
    }

    @Test
    fun invalidActionSchemeFailsClosed() {
        assertFailsWith<IllegalArgumentException> {
            UiContribution(
                id = "bad",
                slot = UiSlot.DRAWER,
                kind = UiContributionKind.BUTTON,
                label = "Bad",
                action = "shell:rm -rf /",
            )
        }
    }
}
