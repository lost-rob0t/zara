package ai.zara.app.prolog

import java.io.File
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder

class PrologStudioTest {
    @get:Rule
    val temporary = TemporaryFolder()

    @Test
    fun queryPolicyAllowsDataQueriesAndRejectsEffectfulMetaPredicates() {
        assertEquals(
            "ancestor(alice, Result)",
            PrologQueryPolicy.requireSafe(" ?- ancestor(alice, Result). "),
        )
        listOf(
            "consult('/sdcard/payload.pl')",
            "call(shell('id'))",
            "open('/data/local/tmp/x', write, S)",
            "process_create(path(sh), ['-c','id'], [])",
            "assertz(owner(root))",
            "abolish(user:goal/1)",
        ).forEach { query ->
            try {
                PrologQueryPolicy.requireSafe(query)
                throw AssertionError("unsafe query was accepted: $query")
            } catch (_: IllegalArgumentException) {
            }
        }
    }

    @Test
    fun queryPolicyRequiresBoundedResultVariable() {
        try {
            PrologQueryPolicy.requireSafe("ancestor(alice, bob)")
            throw AssertionError("query without Result was accepted")
        } catch (error: IllegalArgumentException) {
            assertTrue(error.message.orEmpty().contains("Result"))
        }
    }

    @Test
    fun analyzerBuildsFactAndRuleGraphWithLineProvenance() {
        val source = """
            parent(alice, bob).
            parent(bob, charlie).
            ancestor(X, Y) :- parent(X, Y).
            ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
        """.trimIndent()

        val document = PrologSourceAnalyzer.analyze("family.pl", source)

        assertTrue(document.diagnostics.isEmpty())
        assertEquals(4, document.clauses.size)
        assertEquals(2, document.clauses.count { it.kind == PrologClauseKind.FACT })
        assertEquals(2, document.clauses.count { it.kind == PrologClauseKind.RULE })
        assertTrue(document.graph.nodes.any { it.id == "predicate:ancestor/2" })
        assertTrue(document.graph.nodes.any { it.id == "predicate:parent/2" })
        assertTrue(document.graph.edges.any {
            it.from == "predicate:ancestor/2" && it.to == "predicate:parent/2"
        })
        assertTrue(document.graph.nodes.all { it.source == "family.pl" })
        assertTrue(document.graph.nodes.any { it.line == 4 })
    }

    @Test
    fun analyzerReportsUnterminatedAndUnbalancedSourceWithoutCrashing() {
        val document = PrologSourceAnalyzer.analyze(
            "broken.pl",
            "parent(alice, bob.\nrule(X) :- parent(X, Y)",
        )

        assertFalse(document.diagnostics.isEmpty())
        assertTrue(document.diagnostics.any { it.message.contains("Unbalanced") })
        assertTrue(document.diagnostics.any { it.message.contains("period") })
    }

    @Test
    fun workspacePersistsOnlyPrivatePlFilesAndRejectsTraversal() {
        val workspace = PrologWorkspace(temporary.newFolder("workspace"))

        workspace.saveSource("family.pl", "parent(alice, bob).\n")

        assertEquals(listOf("family.pl"), workspace.listSources().map { it.name })
        assertEquals("parent(alice, bob).\n", workspace.readSource("family.pl").text)
        listOf("../escape.pl", "/sdcard/escape.pl", "notes.txt", ".hidden.pl").forEach { name ->
            try {
                workspace.saveSource(name, "x.")
                throw AssertionError("unsafe workspace name was accepted: $name")
            } catch (_: IllegalArgumentException) {
            }
        }
    }

    @Test
    fun workspaceSeedsExamplesWithoutOverwritingUserChanges() {
        val workspace = PrologWorkspace(temporary.newFolder("examples"))

        workspace.seedExamples(PrologExampleCatalog.examples)
        val first = workspace.readSource("family.pl").text
        workspace.saveSource("family.pl", "% user version\nparent(me, prolog).\n")
        workspace.seedExamples(PrologExampleCatalog.examples)

        assertTrue(first.contains("ancestor"))
        assertEquals("% user version\nparent(me, prolog).\n", workspace.readSource("family.pl").text)
        assertTrue(workspace.listSources().size >= 3)
    }

    @Test
    fun workspaceCanRollbackANewSourceWithoutEscapingItsRoot() {
        val workspace = PrologWorkspace(temporary.newFolder("rollback"))
        workspace.saveSource("temporary.pl", "temporary(ok).\n")

        assertTrue(workspace.deleteSource("temporary.pl"))
        assertTrue(workspace.listSources().isEmpty())
    }
}
