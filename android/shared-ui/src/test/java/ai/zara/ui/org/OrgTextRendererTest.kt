package ai.zara.ui.org

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class OrgTextRendererTest {
    @Test
    fun preservesStarsAndScalesHeadingsLikeAnOutline() {
        val document = OrgRenderDocument(
            title = "Alpha Project",
            nodes = listOf(
                OrgRenderNode(
                    id = "alpha-renderer",
                    level = 1,
                    title = "Build renderer",
                    todo = "TODO",
                    tags = listOf("ui", "org"),
                    project = "zara",
                    body = "Keep literal stars while changing heading font size.",
                ),
                OrgRenderNode(
                    id = "alpha-backlinks",
                    level = 2,
                    title = "Add backlinks",
                    todo = "NEXT",
                    tags = listOf("graph"),
                    body = "Show incoming links and project context.",
                    backlinks = listOf("beta-memory"),
                ),
            ),
        )

        val rendered = OrgTextRenderer.render(document, baseFontSp = 16f)

        assertTrue(rendered.text.contains("* TODO Build renderer :ui:org:"))
        assertTrue(rendered.text.contains("** NEXT Add backlinks :graph:"))
        assertTrue(rendered.text.contains(":PROJECT: zara"))
        assertTrue(rendered.text.contains("Backlinks: beta-memory"))
        assertTrue(OrgTextRenderer.headingFontSp(1, 16f) > OrgTextRenderer.headingFontSp(2, 16f))
        assertEquals(16f, OrgTextRenderer.headingFontSp(8, 16f), 0.001f)
    }

    @Test
    fun typedSnapshotControlsFontsPropertiesAndBacklinks() {
        val document = OrgRenderDocument(
            "Configured",
            listOf(
                OrgRenderNode(
                    id = "node",
                    level = 1,
                    title = "Configured heading",
                    project = "zara",
                    backlinks = listOf("parent"),
                )
            ),
        )
        val config = OrgRenderConfig(
            baseFontSp = 18f,
            headingScales = listOf(2f, 1.25f),
            showBacklinks = false,
            showProperties = false,
        )

        val rendered = OrgTextRenderer.render(document, config)

        assertEquals(36f, OrgTextRenderer.headingFontSp(1, 18f, config.headingScales), 0.001f)
        assertFalse(rendered.text.contains(":PROPERTIES:"))
        assertFalse(rendered.text.contains("Backlinks:"))
        assertTrue(rendered.text.contains("Project: zara"))
    }

    @Test
    fun projectionCarriesOrgRoamAndMemoryContextWithoutOwningStorage() {
        val node = OrgRenderNode(
            id = "node-1",
            level = 1,
            title = "Memory hook",
            project = "zara",
            body = "Promote this node into symbolic memory.",
            backlinks = listOf("node-0"),
            memoryKinds = listOf("symbolic-memory", "project-context", "fact"),
        )

        val rendered = OrgTextRenderer.render(OrgRenderDocument("Memory", listOf(node)))

        assertTrue(rendered.text.contains("Memory: symbolic-memory, project-context, fact"))
        assertTrue(rendered.text.contains("Promote this node into symbolic memory."))
    }
}
