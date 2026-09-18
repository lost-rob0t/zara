package ai.zara.app.prolog

import org.junit.Test

class PrologGraphExplorerTest {
    private fun clause(
        source: String, name: String, line: Int = 1,
        kind: PrologClauseKind = PrologClauseKind.FACT,
        refs: List<String> = emptyList(), text: String = "$name(x).",
    ) = PrologClause(kind, PredicateRef(name, 1), refs.map { PredicateRef(it, 1) }, source, line, text)

    private fun document(name: String, vararg clauses: PrologClause) =
        PrologDocument(name, clauses.joinToString("\n") { it.text }, clauses.toList(), emptyList(), LogicGraph(emptyList(), emptyList()))

    @Test fun allPredicatesSurviveBeyondOldCanvasLimit() {
        val clauses = (0 until 101).map { clause("all.pl", "p$it", it + 1) }
        val graph = PrologGraphExplorerModel.build(listOf(document("all.pl", *clauses.toTypedArray())))
        check(graph.nodes.size == 101)
        check(PrologGraphExplorerModel.layout(graph).size == 101)
        check(graph.nodes.any { it.label == "p100/1" })
    }

    @Test fun definitionsRetainEveryFileAndClause() {
        val graph = PrologGraphExplorerModel.build(listOf(
            document("a.pl", clause("a.pl", "p", 2), clause("a.pl", "p", 4)),
            document("b.pl", clause("b.pl", "p", 7)),
        ))
        val node = graph.nodes.single()
        check(node.locations.map { it.source to it.line } == listOf("a.pl" to 2, "a.pl" to 4, "b.pl" to 7))
        check(node.kind == ExplorerNodeKind.FACT)
    }

    @Test fun unresolvedReferencesNeverPretendToHaveDefinitions() {
        val graph = PrologGraphExplorerModel.build(listOf(document("rules.pl",
            clause("rules.pl", "p", kind = PrologClauseKind.RULE, refs = listOf("missing")),
        )))
        val missing = graph.nodes.single { it.label == "missing/1" }
        check(missing.kind == ExplorerNodeKind.REFERENCE)
        check(missing.locations.isEmpty())
        check(graph.edges.single().from == "predicate:p/1")
        check(graph.edges.single().to == missing.id)
        check(graph.edges.single().label == "references")
    }

    @Test fun factsRulesAndMixedPredicatesAreSemanticNotPositional() {
        val graph = PrologGraphExplorerModel.build(listOf(document("k.pl",
            clause("k.pl", "fact"),
            clause("k.pl", "rule", kind = PrologClauseKind.RULE),
            clause("k.pl", "mixed"), clause("k.pl", "mixed", 2, PrologClauseKind.RULE),
        )))
        check(graph.nodes.single { it.label == "fact/1" }.kind == ExplorerNodeKind.FACT)
        check(graph.nodes.single { it.label == "rule/1" }.kind == ExplorerNodeKind.RULE)
        check(graph.nodes.single { it.label == "mixed/1" }.kind == ExplorerNodeKind.MIXED)
    }

    @Test fun directivesAreInspectableButNotFakePredicates() {
        val documents = listOf(document("syntax.pl", clause("syntax.pl", "directive", kind = PrologClauseKind.DIRECTIVE,
            refs = listOf("op"), text = ":- op(600, xfx, because).")))
        check(PrologGraphExplorerModel.build(documents).nodes.isEmpty())
        val graph = PrologGraphExplorerModel.build(documents, includeClauses = true)
        check(graph.nodes.single().kind == ExplorerNodeKind.DIRECTIVE)
        check(graph.edges.isEmpty())
    }

    @Test fun fileScopeKeepsCrossFileDefinitionsWithoutUnrelatedNodes() {
        val graph = PrologGraphExplorerModel.build(listOf(
            document("a.pl", clause("a.pl", "caller", kind = PrologClauseKind.RULE, refs = listOf("target"))),
            document("b.pl", clause("b.pl", "target", 8), clause("b.pl", "unrelated", 9)),
        ), source = "a.pl")
        check(graph.nodes.map { it.label }.toSet() == setOf("caller/1", "target/1"))
        check(graph.nodes.single { it.label == "target/1" }.locations.single().source == "b.pl")
        check(graph.edges.size == 1)
    }

    @Test fun clauseViewPreservesSameLineClausesAndReferenceOrigin() {
        val graph = PrologGraphExplorerModel.build(listOf(document("a.pl",
            clause("a.pl", "p", text = "p(a)."),
            clause("a.pl", "p", text = "p(b)."),
            clause("a.pl", "r", 2, PrologClauseKind.RULE, listOf("p"), "r(X) :- p(X)."),
        )), includeClauses = true)
        check(graph.nodes.count { it.kind == ExplorerNodeKind.FACT_CLAUSE } == 2)
        check(graph.nodes.map { it.id }.distinct().size == graph.nodes.size)
        check(graph.edges.count { it.label == "defines" } == 3)
        check(graph.edges.single { it.label == "references" }.from.startsWith("clause:"))
    }

    @Test fun searchAndFocusKeepOnlyImmediateContext() {
        val graph = PrologGraphExplorerModel.build(listOf(document("a.pl",
            clause("a.pl", "a", kind = PrologClauseKind.RULE, refs = listOf("b")),
            clause("a.pl", "b", kind = PrologClauseKind.RULE, refs = listOf("c")),
            clause("a.pl", "c", kind = PrologClauseKind.RULE, refs = listOf("d")),
            clause("a.pl", "d"),
        )))
        val focused = PrologGraphExplorerModel.filter(graph, focus = "predicate:b/1")
        check(focused.nodes.map { it.label }.toSet() == setOf("a/1", "b/1", "c/1"))
        check(focused.edges.size == 2)
        check(PrologGraphExplorerModel.filter(graph, query = "B/1").nodes == focused.nodes)
        check(PrologGraphExplorerModel.filter(graph, query = "not present").nodes.isEmpty())
        check(PrologGraphExplorerModel.filter(graph, focus = "removed").nodes.isEmpty())
    }

    @Test fun recursiveReferenceSurvivesDeduplication() {
        val graph = PrologGraphExplorerModel.build(listOf(document("a.pl",
            clause("a.pl", "p", kind = PrologClauseKind.RULE, refs = listOf("p", "p")),
        )))
        check(graph.nodes.size == 1)
        check(graph.edges.size == 1)
        check(graph.edges.single().from == graph.edges.single().to)
    }

    @Test fun layoutIsStableCompleteAndNonOverlapping() {
        val docs = listOf(document("b.pl", clause("b.pl", "b")), document("a.pl", clause("a.pl", "a")))
        val graph = PrologGraphExplorerModel.build(docs)
        check(graph == PrologGraphExplorerModel.build(docs.reversed()))
        val boxes = PrologGraphExplorerModel.layout(graph)
        check(boxes == PrologGraphExplorerModel.layout(PrologGraphExplorerModel.build(docs.reversed())))
        val a = boxes.values.first()
        val b = boxes.values.last()
        check(a.bottom <= b.top || b.bottom <= a.top || a.right <= b.left || b.right <= a.left)
        check(PrologGraphExplorerModel.hitTest(boxes, b.left + 1f, b.top + 1f) == boxes.keys.last())
        check(PrologGraphExplorerModel.hitTest(boxes, -10f, -10f) == null)
    }

    @Test fun sourceLinesNormalizeCrLfPreserveUnicodeAndFinalBlankLine() {
        val text = "p('😀').\r\nq(x).\n"
        val ranges = PrologGraphExplorerModel.lineRanges(text)
        check(ranges.size == 3)
        check(text.substring(ranges[0].first, ranges[0].last + 1) == "p('😀').")
        check(text.substring(ranges[1].first, ranges[1].last + 1) == "q(x).")
        check(ranges[2].isEmpty())
        check(PrologGraphExplorerModel.lineRanges("").single().isEmpty())
        check(PrologGraphExplorerModel.lineRanges("one").single() == 0..2)
    }

    @Test fun emptyAndMissingFileScopesAreWellDefined() {
        check(PrologGraphExplorerModel.build(emptyList()).nodes.isEmpty())
        check(PrologGraphExplorerModel.layout(ExplorerGraph(emptyList(), emptyList())).isEmpty())
        check(PrologGraphExplorerModel.build(listOf(document("a.pl", clause("a.pl", "p"))), source = "gone.pl").nodes.isEmpty())
    }
}
