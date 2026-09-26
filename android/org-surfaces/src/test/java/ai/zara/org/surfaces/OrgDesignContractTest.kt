package ai.zara.org.surfaces

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class OrgDesignContractTest {
    private val mainSources: List<File> =
        File("src/main/java").walkTopDown().filter { it.isFile && it.extension == "kt" }.toList()

    private fun source(name: String): String =
        mainSources.first { it.name == name }.readText()

    @Test fun `surfaces never hard-code theme colors`() {
        val offenders = mainSources.filter { "Color(0x" in it.readText() }.map { it.name }
        assertTrue(
            "org surfaces must consume the shared semantic token layer, found hard-coded colors in: $offenders",
            offenders.isEmpty(),
        )
    }

    @Test fun `org theme consumes the canonical zara token layer`() {
        val theme = source("OrgTheme.kt")
        assertTrue(theme.contains("themeTokens("))
        assertTrue(theme.contains("ZaraSemanticTokens"))
        assertTrue(theme.contains("LocalOrgTokens"))
        assertTrue(theme.contains("reducedGlow"))
    }

    @Test fun `org theme maps every semantic role into the material scheme`() {
        val theme = source("OrgTheme.kt")
        for (role in listOf(
            "background", "surface", "surfaceElevated", "surfaceInput",
            "primary", "secondary", "text", "textMuted", "error",
        )) {
            assertTrue("role $role missing from org color scheme mapping", "tokens.$role" in theme)
        }
    }

    @Test fun `shared surfaces render the luminous outline language`() {
        val components = source("OrgComponents.kt")
        assertTrue(components.contains("fun OrgPanel("))
        assertTrue(components.contains("fun OrgStatusDot("))
        assertTrue(components.contains("tokens.border"))
        assertTrue(components.contains("tokens.borderActive"))
        val panelUsage = Regex("\\bOrgPanel[\\s({]")
        for (surface in listOf("TodoSurface.kt", "RemindersSurface.kt", "TimersSurface.kt")) {
            assertTrue(
                "${surface} must render rows as OrgPanel blocks",
                panelUsage.containsMatchIn(source(surface)),
            )
        }
    }

    @Test fun `status colors come from semantic roles not raw material roles`() {
        assertTrue(source("RemindersSurface.kt").contains("tokens.accentMagenta"))
        assertTrue(source("TimersSurface.kt").contains("tokens.success"))
        assertTrue(source("GraphSurface.kt").contains("tokens.border"))
    }

    @Test fun `workspace establishes readable themed content color`() {
        val workspace = source("OrgWorkspaceScreen.kt")
        assertTrue(workspace.contains("Surface("))
        assertTrue(workspace.contains("contentColor = MaterialTheme.colorScheme.onBackground"))
    }

    @Test fun `workspace uses a compact drawer instead of a crowded tab strip`() {
        val workspace = source("OrgWorkspaceScreen.kt")
        assertTrue(workspace.contains("ModalNavigationDrawer("))
        assertTrue(workspace.contains("NavigationDrawerItem("))
        assertFalse(workspace.contains("horizontalScroll("))
    }

    @Test fun `workspace content stays inside safe system bounds`() {
        val workspace = source("OrgWorkspaceScreen.kt")
        assertTrue(workspace.contains("windowInsetsPadding(WindowInsets.safeDrawing)"))
    }

    @Test fun `editor provides full page block raw and config workspaces`() {
        val editor = source("EditorSurface.kt")
        assertTrue(editor.contains("OrgPageParser.parse("))
        assertTrue(editor.contains("EditorMode.PAGE"))
        assertTrue(editor.contains("EditorMode.RAW"))
        assertTrue(editor.contains("EditorMode.CONFIG"))
        assertTrue(editor.contains("OrgAppPolicy.parse("))
        assertTrue(editor.contains("fillMaxSize()"))
    }
}
