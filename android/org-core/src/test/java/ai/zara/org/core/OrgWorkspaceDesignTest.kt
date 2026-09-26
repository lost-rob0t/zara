package ai.zara.org.core

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class OrgWorkspaceDesignTest {
    @Test fun `outline exposes lossless blocks and ignores headings inside source blocks`() {
        val source = """
            #+title: Project Atlas
            * TODO First block
            Body with [[id:second][a link]].
            ** Child block
            #+begin_src prolog
            * not_a_heading.
            #+end_src
            * Second block
        """.trimIndent()

        val document = OrgPageParser.parse(source)

        assertEquals("Project Atlas", document.title)
        assertEquals(listOf("TODO First block", "Child block", "Second block"), document.blocks.map { it.heading })
        assertEquals(listOf(1, 2, 1), document.blocks.map { it.depth })
        assertTrue(document.blocks[1].raw.contains("* not_a_heading."))
        assertEquals(source, document.source)
    }

    @Test fun `block replacement changes only its exact source range`() {
        val source = "#+title: Keep\r\n* One\r\nBody one\r\n* Two\r\nBody two"
        val block = OrgPageParser.parse(source).blocks.first()

        val updated = OrgPageParser.replaceBlock(source, block, "* One edited\r\nBody one\r\n")

        assertEquals("#+title: Keep\r\n* One edited\r\nBody one\r\n* Two\r\nBody two", updated)
    }

    @Test fun `config pl accepts bounded full app policy facts`() {
        val config = OrgAppPolicy.parse(
            """
            app_policy(appearance_theme, outrun).
            app_policy(appearance_density, compact).
            app_policy(editor_mode, blocks).
            app_policy(save_mode, explicit).
            app_policy(sync_mode, manual).
            app_policy(automation_policy, approval_required).
            app_policy(plugin_policy, disabled).
            app_policy(runtime_mode, local).
            """.trimIndent(),
        )

        assertEquals("blocks", config.values.getValue("editor_mode"))
        assertEquals("approval_required", config.values.getValue("automation_policy"))
        assertFalse(config.permitsUnapprovedEffects)
    }

    @Test fun `config pl rejects rules directives unknown policy and effect widening`() {
        for (source in listOf(
            ":- initialization(main).",
            "app_policy(editor_mode, blocks) :- true.",
            "app_policy(secret_policy, enabled).",
            "app_policy(plugin_policy, unrestricted).",
        )) {
            val failure = runCatching { OrgAppPolicy.parse(source) }.exceptionOrNull()
            assertTrue("must reject $source", failure is IllegalArgumentException)
        }
    }
}
