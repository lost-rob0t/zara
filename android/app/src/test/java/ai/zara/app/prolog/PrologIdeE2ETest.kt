package ai.zara.app.prolog

import java.io.File
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class PrologIdeE2ETest {
    @Test
    fun decisionAndExplainPredicatesAreBothCallableExpertEntries() {
        val catalog = PrologWorkspaceCatalog.from(
            listOf(
                PrologSource(
                    "expert.pl",
                    """
                        expert_activation(triage, inspect).
                        triage_decision(Entity, review).
                        triage_explain(Entity, Result) :- triage_decision(Entity, Result).
                    """.trimIndent(),
                ),
            ),
        )

        assertEquals(
            listOf("triage_decision/2", "triage_explain/2"),
            catalog.experts.map { it.indicator },
        )
        assertEquals(
            "triage_decision(alice, Result)",
            LocalPrologCommand.parse("/expert triage_decision alice", catalog).query,
        )
        assertEquals(
            "triage_explain(alice, Result)",
            LocalNaturalLanguageExpertRouter.query("inspect alice", catalog),
        )

        val decisionOnly = PrologWorkspaceCatalog.from(
            listOf(
                PrologSource(
                    "decision.pl",
                    "expert_activation(review, inspect).\nreview_decision(Entity, accept).\n",
                ),
            ),
        )
        assertEquals(
            "review_decision(alice, Result)",
            LocalNaturalLanguageExpertRouter.query("inspect alice", decisionOnly),
        )
    }

    @Test
    fun boundedReplaceSupportsCursorAwareNextAndReplaceAll() {
        val source = "fact(one).\nfact(two).\nFACT(three).\n"
        val next = PrologReplace.replaceNext(source, "fact", "signal", cursor = 6)

        assertEquals(1, next.replacements)
        assertEquals("fact(one).\nsignal(two).\nFACT(three).\n", next.text)
        assertEquals(next.text.indexOf("signal") + "signal".length, next.cursor)

        val all = PrologReplace.replaceAll(next.text, "fact", "signal")
        assertEquals(2, all.replacements)
        assertEquals("signal(one).\nsignal(two).\nsignal(three).\n", all.text)
    }

    @Test
    fun composeStudioWiresCursorCompletionBuildersNavigationCancelAndIndependentMiniBoxes() {
        val studio = File("src/main/java/ai/zara/app/ui/PrologStudioSurface.kt").readText()
        val builder = File("src/main/java/ai/zara/app/ui/PrologStructuredBuilderPane.kt").readText()

        assertTrue(studio.contains("TextFieldValue(draft, TextRange(draft.length))"))
        assertTrue(studio.contains("PrologCompletionEngine.complete(editorValue.text, cursor, completionDocuments)"))
        assertTrue(studio.contains("KeyValueRow(\"cursor\""))
        assertTrue(studio.contains("KeyValueRow(\"buffer\", if (dirty) \"dirty\" else \"saved\")"))
        assertTrue(studio.contains("PrologReplace.replaceNext"))
        assertTrue(studio.contains("PrologReplace.replaceAll"))
        assertTrue(studio.contains("PrologStructuredBuilderPane("))
        assertTrue(studio.contains("PrologAuthorityPolicy.validate(analyzed)"))
        assertTrue(studio.contains("moveCursorToLine(diagnostic.line)"))
        assertTrue(studio.contains("GraphPane(workspaceDocuments)"))
        assertTrue(studio.contains("onNavigate(clause.source, clause.line)"))
        assertTrue(studio.contains("LocalZaraServer.CANCEL_QUERY_COMMAND"))
        assertTrue(studio.contains("var queryPending by rememberSaveable"))
        assertTrue(studio.contains("SecondaryAction(\"Cancel query\""))
        assertTrue(studio.contains("result.cancelled -> \"cancelled.\""))
        assertTrue(studio.contains("var boxOneHistory by rememberSaveable"))
        assertTrue(studio.contains("var boxTwoHistory by rememberSaveable"))
        assertTrue(studio.contains("LaunchedEffect(queryResult)"))
        assertTrue(builder.contains("PrologFormBuilder.schema"))
        assertTrue(builder.contains("PrologFormBuilder.fact"))
        assertTrue(builder.contains("PrologFormBuilder.rule"))
        assertTrue(builder.contains("Append, validate & reload"))
    }
}
