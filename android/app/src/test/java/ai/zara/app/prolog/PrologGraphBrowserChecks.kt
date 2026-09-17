package ai.zara.app.prolog

object PrologGraphBrowserChecks {
    private fun document(name: String, count: Int = 1): PrologDocument {
        val clauses = (1..count).map { index ->
            PrologClause(
                PrologClauseKind.FACT, PredicateRef("fact$index", 1), emptyList(),
                name, index, "fact$index(value).",
            )
        }
        val nodes = clauses.flatMapIndexed { index, clause ->
            listOf(
                LogicGraphNode("predicate:${clause.predicate.indicator}", clause.predicate.indicator,
                    LogicNodeKind.PREDICATE, name, clause.line),
                LogicGraphNode("clause:$name:${clause.line}:$index", clause.text,
                    LogicNodeKind.CLAUSE, name, clause.line),
            )
        }
        val edges = nodes.chunked(2).map { (predicate, clause) ->
            LogicGraphEdge(predicate.id, clause.id, "defines")
        }
        return PrologDocument(name, clauses.joinToString("\n") { it.text }, clauses,
            emptyList(), LogicGraph(nodes, edges))
    }

    fun allNodesAndFilesRemainAvailable() {
        val empty = document("notes.pl", 0).copy(text = "% notes only\n")
        val browser = PrologGraphBrowser.from(listOf(document("config.pl", 40), document("rules.pl", 40), empty))
        check(browser.documents.size == 3)
        check(browser.graph.nodes.size == 120) { "Must retain 40 predicates and 80 clauses, not 14 nodes" }
        check(browser.graph.edges.size == 80)
        check(browser.visibleGraph().nodes.size == 120)
        check(browser.visibleGraph(predicatesOnly = true).nodes.size == 40)
        check(browser.visibleGraph(source = "notes.pl").nodes.isEmpty())
        check(browser.documents.last().text == "% notes only\n")
    }

    fun everyDefinitionIsPreservedAcrossFiles() {
        val browser = PrologGraphBrowser.from(listOf(document("one.pl"), document("two.pl")))
        val origins = browser.locations("predicate:fact1/1")
        check(origins.map { it.source } == listOf("one.pl", "two.pl"))
        check(origins.all { it.definition && it.line == 1 })
        check(browser.locations("clause:two.pl:1:0").single().source == "two.pl")
        check(browser.locations("missing").isEmpty())
    }

    fun definitionWinsOverAnEarlierReference() {
        val caller = document("caller.pl").let { original ->
            original.copy(
                clauses = listOf(original.clauses.single().copy(
                    kind = PrologClauseKind.RULE,
                    bodyPredicates = listOf(PredicateRef("target", 1)),
                )),
                graph = original.graph.copy(
                    nodes = original.graph.nodes + LogicGraphNode("predicate:target/1", "target/1",
                        LogicNodeKind.PREDICATE, "caller.pl", 1),
                    edges = original.graph.edges + LogicGraphEdge("predicate:fact1/1", "predicate:target/1", "calls"),
                ),
            )
        }
        val target = PrologDocument("target.pl", "% heading\ntarget(ok).",
            listOf(PrologClause(PrologClauseKind.FACT, PredicateRef("target", 1), emptyList(),
                "target.pl", 2, "target(ok).")), emptyList(),
            LogicGraph(listOf(LogicGraphNode("predicate:target/1", "target/1",
                LogicNodeKind.PREDICATE, "target.pl", 2)), emptyList()))
        val browser = PrologGraphBrowser.from(listOf(caller, target))
        val origin = browser.locations("predicate:target/1").single()
        check(origin == PrologGraphLocation("target.pl", 2, true))
        check(browser.graph.nodes.single { it.id == "predicate:target/1" }.source == "target.pl")
        val external = PrologGraphBrowser.from(listOf(caller)).locations("predicate:target/1").single()
        check(external == PrologGraphLocation("caller.pl", 1, false))
    }

    fun filtersKeepOnlyEdgesWithVisibleEndpoints() {
        val browser = PrologGraphBrowser.from(listOf(document("one.pl", 30), document("two.pl", 30)))
        val filtered = browser.visibleGraph(source = "two.pl", query = "FACT30")
        check(filtered.nodes.size == 2)
        check(filtered.edges.size == 1)
        val ids = filtered.nodes.map { it.id }.toSet()
        check(filtered.edges.all { it.from in ids && it.to in ids })
        check(browser.visibleGraph(query = "not-here").nodes.isEmpty())
        check(browser.visibleGraph(source = "missing.pl").nodes.isEmpty())
        check(browser.visibleGraph(source = "two.pl").nodes.size == 60)
    }

    fun emptyAndInvalidSourceTextIsStillInspectable() {
        val invalid = document("invalid.pl", 0).copy(text = "broken(",
            diagnostics = listOf(PrologDiagnostic(1, "unbalanced")))
        val browser = PrologGraphBrowser.from(listOf(invalid))
        check(browser.documents.single().text == "broken(")
        check(browser.graph.nodes.isEmpty())
        check(PrologGraphBrowser.from(emptyList()).documents.isEmpty())
        check(runCatching { PrologGraphBrowser.from(listOf(invalid, invalid)) }.isFailure)
    }

    fun lineOffsetsMatchOriginalUnicodeAndCrLfText() {
        val text = "% 😀\r\nhello(世界).\n\n"
        val index = PrologSourceLines(text)
        check(index.ranges.size == 4)
        check(index.ranges.map { text.substring(it.start, it.endExclusive) } ==
            listOf("% 😀", "hello(世界).", "", ""))
        check(index.clampLine(-10) == 1)
        check(index.clampLine(Int.MAX_VALUE) == 4)
        check(PrologSourceLines("").ranges.single() == PrologLineRange(0, 0))
        check(PrologSourceLines("last").ranges.single() == PrologLineRange(0, 4))
    }

    fun layoutIncludesEveryNodeAndRejectsGaps() {
        val nodes = PrologGraphBrowser.from(listOf(document("many.pl", 120))).graph.nodes
        val layout = PrologGraphLayout(nodes)
        check(layout.positions.size == 240)
        layout.positions.forEach { (id, position) ->
            check(layout.hitTest(position.x + 5, position.y + 5) == id)
            check(position.x >= 0 && position.y >= 0)
            check(position.x + layout.nodeWidth <= layout.width)
            check(position.y + layout.nodeHeight <= layout.height)
        }
        check(layout.hitTest(-1f, -1f) == null)
        check(layout.hitTest(layout.width, layout.height) == null)
        val first = layout.positions.values.first()
        check(layout.hitTest(first.x + layout.nodeWidth + 1, first.y + 1) == null)
        check(PrologGraphLayout(emptyList()).hitTest(0f, 0f) == null)
    }

    fun navigationNeverDiscardsADirtyBuffer() {
        check(prologCanOpenSource("a.pl", "edited", "saved", "a.pl"))
        check(!prologCanOpenSource("a.pl", "edited", "saved", "b.pl"))
        check(prologCanOpenSource("a.pl", "saved", "saved", "b.pl"))
    }

    fun runAll() {
        allNodesAndFilesRemainAvailable()
        everyDefinitionIsPreservedAcrossFiles()
        definitionWinsOverAnEarlierReference()
        filtersKeepOnlyEdgesWithVisibleEndpoints()
        emptyAndInvalidSourceTextIsStillInspectable()
        lineOffsetsMatchOriginalUnicodeAndCrLfText()
        layoutIncludesEveryNodeAndRejectsGaps()
        navigationNeverDiscardsADirtyBuffer()
    }
}

fun main() {
    PrologGraphBrowserChecks.runAll()
    println("PASS: 8 graph/source contract groups")
}
