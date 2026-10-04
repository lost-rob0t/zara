package ai.zara.org.core

import java.time.LocalDate
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class OrgCoreTest {
    @Test
    fun profileDoesNotEmbedOperatorSpecificOrgRoot() {
        assertFalse(DoomOrgProfile.orgRoot.startsWith("~"))
        assertFalse(DoomOrgProfile.orgRoot.contains("Documents/Notes/org"))
    }

    @Test
    fun parsesFileDeclaredTodoKeywordsWithoutOperatorProfile() {
        val source = """
            #+TODO: NEXT(n) BLOCKED(b@/!) | SHIPPED(s!)
            * NEXT Build arbitrary workflow support
            * BLOCKED Wait on another task
            * SHIPPED Release it
        """.trimIndent()

        val tasks = OrgParser.parse(source, "custom/workflow.org").tasks
        assertEquals(listOf("NEXT", "BLOCKED", "SHIPPED"), tasks.map { it.state })
        assertEquals(
            listOf(
                "Build arbitrary workflow support",
                "Wait on another task",
                "Release it",
            ),
            tasks.map { it.title },
        )
    }

    @Test
    fun todoMutationPreservesCrLfAndTerminalNewline() {
        val source = "#+TODO: TODO | DONE\r\n* TODO Keep formatting\r\nBody\r\n"
        val task = OrgParser.parse(source, "nested/work.org").tasks.single()

        val mutation = OrgParser.cycleTodoState(source, task)

        assertEquals("DONE", mutation.state)
        assertEquals("#+TODO: TODO | DONE\r\n* DONE Keep formatting\r\nBody\r\n", mutation.source)
    }

    @Test
    fun todoMutationPreservesMissingTerminalNewline() {
        val source = "* TODO Keep eof\nBody without newline"
        val task = OrgParser.parse(source, "work.org").tasks.single()

        val mutation = OrgParser.cycleTodoState(source, task)

        assertEquals("DONE", mutation.state)
        assertEquals("* DONE Keep eof\nBody without newline", mutation.source)
    }

    @Test
    fun parsesDoomTodoMetadataAndAgendaGroup() {
        val source = """
            * TODO [#A] Ship parser :StarIntel:org_parser:
            :PROPERTIES:
            :Effort: 1:30
            :CATEGORY: Work
            :END:
            SCHEDULED: <2026-09-17 Thu>
            DEADLINE: <2026-09-18 Fri>
        """.trimIndent()

        val task = OrgParser.parse(source, "agenda/work.org").tasks.single()
        assertEquals("Ship parser", task.title)
        assertEquals('A', task.priority)
        assertEquals("1:30", task.effort)
        assertEquals("Work", task.category)
        assertEquals(LocalDate.of(2026, 9, 17), task.scheduled)
        assertEquals(AgendaGroup.TODAY, DoomAgenda.group(task, LocalDate.of(2026, 9, 17)))
    }

    @Test
    fun tanglesPythonAndPrologUsingOrgHeaderArgs() {
        val source = """
            #+title: Literate Zara config
            #+property: header-args :results none
            #+property: header-args:python :tangle config.py
            #+property: header-args:prolog :tangle config.pl

            * Python
            #+begin_src python
            def configure(zara):
                zara.theme = "outrun"
            #+end_src

            * Prolog
            #+begin_src prolog
            zara_theme(outrun).
            #+end_src
        """.trimIndent()

        val result = OrgTangler.tangle(source, "zara.org")
        assertEquals(listOf("config.py", "config.pl"), result.outputs.map { it.path })
        assertTrue(result.outputs.first().content.contains("def configure"))
        assertTrue(result.outputs.last().content.contains("zara_theme(outrun)."))
        assertEquals(0, result.skippedBlocks)
    }

    @Test
    fun refusesTangleTraversal() {
        val source = """
            #+begin_src python :tangle ../escape.py
            print("no")
            #+end_src
        """.trimIndent()
        val failure = runCatching { OrgTangler.tangle(source) }.exceptionOrNull()
        assertTrue(failure is IllegalArgumentException)
    }

    @Test
    fun captureTemplateMatchesDoomFields() {
        val captured = DoomOrgProfile.captureTodo(
            title = "Review PR",
            effort = "0:30",
            category = "Work",
            scheduled = "2026-09-17 Thu",
            deadline = "2026-09-18 Fri",
        )
        assertTrue(captured.startsWith("* TODO Review PR"))
        assertTrue(captured.contains(":Effort: 0:30"))
        assertTrue(captured.contains(":CATEGORY: Work"))
        assertTrue(captured.contains("SCHEDULED: <2026-09-17 Thu>"))
        assertFalse(captured.contains("null"))
    }
}
