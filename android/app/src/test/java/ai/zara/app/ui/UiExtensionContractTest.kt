package ai.zara.app.ui

import ai.zara.app.runtime.LocalQueryResult
import ai.zara.app.ui.extensions.AndroidPluginUiProjection
import ai.zara.app.ui.extensions.AndroidUiExtensionRepository
import ai.zara.app.ui.extensions.PortablePythonUiInitParser
import ai.zara.app.ui.extensions.UiContribution
import ai.zara.app.ui.extensions.UiContributionKind
import ai.zara.app.ui.extensions.UiExtensionRegistry
import ai.zara.app.ui.extensions.UiPlatform
import ai.zara.app.ui.extensions.UiSlot
import java.io.File
import java.nio.file.Files
import java.util.concurrent.CompletableFuture
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertThrows
import org.junit.Assert.assertTrue
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

        assertThrows(IllegalArgumentException::class.java) {
            PortablePythonUiInitParser.parse(source)
        }
    }

    @Test
    fun androidPluginUiRequiresTrustedEnabledHostProjection() {
        val root = Files.createTempDirectory("zara-ui-contract").toFile()
        val contribution = UiContribution(
            id = "settings",
            slot = UiSlot.SETTINGS,
            kind = UiContributionKind.BUTTON,
            label = "Open settings",
            action = "plugin:settings",
            platforms = setOf(UiPlatform.ANDROID),
        )
        try {
            val repository = AndroidUiExtensionRepository(
                root = root,
                prologQuery = { query ->
                    CompletableFuture.completedFuture(LocalQueryResult(query, emptyList(), 1))
                },
                pluginProjectionProvider = {
                    listOf(
                        AndroidPluginUiProjection("disabled", true, false, 4, listOf(contribution)),
                        AndroidPluginUiProjection("untrusted", false, true, 7, listOf(contribution)),
                        AndroidPluginUiProjection("ready", true, true, 9, listOf(contribution)),
                    )
                },
            )

            val projected = repository.load().get()
            assertEquals(listOf("plugin:ready"), projected.map { it.owner })
            assertEquals(listOf("settings"), projected.map { it.id })
        } finally {
            root.deleteRecursively()
        }
    }

    @Test
    fun brokenPortablePythonDoesNotHideTrustedPluginProjection() {
        val root = Files.createTempDirectory("zara-ui-isolation").toFile()
        root.resolve("init.py").writeText(
            """
            def register(ui):
                import os
            """.trimIndent(),
        )
        val contribution = UiContribution(
            id = "status",
            slot = UiSlot.PLUGINS,
            kind = UiContributionKind.STATUS,
            label = "Ready",
            platforms = setOf(UiPlatform.ANDROID),
        )
        try {
            val repository = AndroidUiExtensionRepository(
                root = root,
                prologQuery = { query ->
                    CompletableFuture.completedFuture(LocalQueryResult(query, emptyList(), 1))
                },
                pluginProjectionProvider = {
                    listOf(AndroidPluginUiProjection("notes", true, true, 3, listOf(contribution)))
                },
            )

            val projected = repository.load().get()
            assertEquals(listOf("plugin:notes"), projected.map { it.owner })
            assertEquals(listOf("status"), projected.map { it.id })
        } finally {
            root.deleteRecursively()
        }
    }

    @Test
    fun typedPluginActionsStayDisabledUntilHostDispatcherExists() {
        val pluginAction = UiContribution(
            id = "sync",
            slot = UiSlot.SETTINGS,
            kind = UiContributionKind.BUTTON,
            label = "Sync",
            action = "plugin:sync",
        )
        val routeAction = pluginAction.copy(id = "settings", action = "route:settings")

        assertFalse(pluginAction.isUiActionEnabled())
        assertTrue(routeAction.isUiActionEnabled())
    }

    @Test
    fun androidToggleRendererDoesNotOwnCanonicalPluginState() {
        val source = File("src/main/java/ai/zara/app/ui/AndroidUiExtensions.kt").readText()

        assertFalse(source.contains("rememberSaveable(contribution.owner"))
        assertFalse(source.contains("enabled = !enabled"))
        assertTrue(source.contains("STATE UNAVAILABLE"))
    }

    @Test
    fun invalidActionSchemeFailsClosed() {
        assertThrows(IllegalArgumentException::class.java) {
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
