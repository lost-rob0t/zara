package ai.zara.app.ui.extensions

import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Assert.assertTrue
import org.junit.Test

class UiExtensionAbiCoreTest {
    @Test
    fun registryReplacementIsDeterministicAndFailureAtomic() {
        val registry = UiExtensionRegistry()
        registry.replaceOwner(
            "plugin:notes",
            listOf(
                UiContribution(
                    id = "open",
                    slot = UiSlot.DRAWER,
                    kind = UiContributionKind.BUTTON,
                    label = "Open notes",
                    action = "route:plugins",
                    priority = 20,
                    platforms = setOf(UiPlatform.ANDROID),
                ),
            ),
        )

        assertThrows(IllegalArgumentException::class.java) {
            registry.replaceOwner(
                "plugin:notes",
                listOf(
                    UiContribution(
                        id = "duplicate",
                        slot = UiSlot.SETTINGS,
                        kind = UiContributionKind.TEXT,
                        label = "one",
                    ),
                    UiContribution(
                        id = "duplicate",
                        slot = UiSlot.SETTINGS,
                        kind = UiContributionKind.TEXT,
                        label = "two",
                    ),
                ),
            )
        }

        val retained = registry.forPlatform(UiPlatform.ANDROID)
        assertEquals(listOf("open"), retained.map { it.id })
        assertEquals(listOf("plugin:notes"), retained.map { it.owner })
    }

    @Test
    fun portablePythonParserAcceptsOnlyBoundedDeclarativeUiAddCalls() {
        val source = """
            def register(ui):
                ui.add("query", "chat.top", "button", "Query", "submit:demo(Result)", 5, ["android"])
                ui.add("logic", "drawer", "surface", "Logic", "route:logic", 20, ["desktop", "android"])
        """.trimIndent()

        val parsed = PortablePythonUiInitParser.parse(source)

        assertEquals(listOf("query", "logic"), parsed.map { it.id })
        assertEquals(setOf(UiPlatform.ANDROID), parsed.first().platforms)
        assertTrue(parsed.all { it.owner == "user:init.py" })

        assertThrows(IllegalArgumentException::class.java) {
            PortablePythonUiInitParser.parse(
                """
                def register(ui):
                    import os
                    ui.add("bad", "drawer", "text", "bad", "", 1, ["android"])
                """.trimIndent(),
            )
        }
    }

    @Test
    fun contributionValidationFailsClosedOnUnknownActionsAndInteractiveItemsWithoutActions() {
        assertThrows(IllegalArgumentException::class.java) {
            UiContribution(
                id = "shell",
                slot = UiSlot.DRAWER,
                kind = UiContributionKind.BUTTON,
                label = "Nope",
                action = "shell:rm -rf /",
            )
        }
        assertThrows(IllegalArgumentException::class.java) {
            UiContribution(
                id = "missing-action",
                slot = UiSlot.DRAWER,
                kind = UiContributionKind.SURFACE,
                label = "Missing",
            )
        }
    }

    @Test
    fun prologUiParserAcceptsUiSevenAndRejectsMalformedOrUnbalancedTerms() {
        val contribution = PrologUiTermParser.parse(
            "ui(notes, 'chat.bottom', button, 'Open notes', 'route:plugins', 15, [android, desktop]).",
        )

        assertEquals("notes", contribution.id)
        assertEquals(UiSlot.CHAT_BOTTOM, contribution.slot)
        assertEquals(UiContributionKind.BUTTON, contribution.kind)
        assertEquals(setOf(UiPlatform.ANDROID, UiPlatform.DESKTOP), contribution.platforms)

        listOf(
            "ui(notes, drawer, text).",
            "ui(notes, drawer, text, 'broken, '', 1, [android]).",
            "surface(notes, drawer, text, Notes, '', 1, [android]).",
        ).forEach { raw ->
            assertThrows(IllegalArgumentException::class.java) {
                PrologUiTermParser.parse(raw)
            }
        }
    }

    @Test
    fun prologUiParserRejectsUnknownEscapesAndPreservesLastGoodRegistryState() {
        val registry = UiExtensionRegistry()
        val accepted = PrologUiTermParser.parse(
            "ui(last_good, drawer, text, 'Last good', '', 1, [android]).",
        )
        registry.replaceOwner("plugin:notes", listOf(accepted))

        listOf(
            """ui(bad_action, drawer, button, 'Bad action', 'route:plug\qins', 1, [android]).""",
            """ui(bad_label, drawer, text, 'Bad\qlabel', '', 1, [android]).""",
        ).forEach { malformed ->
            assertThrows(IllegalArgumentException::class.java) {
                registry.replaceOwner(
                    "plugin:notes",
                    listOf(PrologUiTermParser.parse(malformed)),
                )
            }

            val retained = registry.forPlatform(UiPlatform.ANDROID)
            assertEquals(listOf("last_good"), retained.map { it.id })
            assertEquals(listOf("Last good"), retained.map { it.label })
            assertEquals(listOf("plugin:notes"), retained.map { it.owner })
        }
    }
}
