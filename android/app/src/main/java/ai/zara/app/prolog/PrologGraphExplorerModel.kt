package ai.zara.app.prolog

enum class ExplorerNodeKind(val label: String) {
    FACT("FACT"), RULE("RULE"), MIXED("FACT + RULE"), REFERENCE("REFERENCE"),
    FACT_CLAUSE("FACT CLAUSE"), RULE_CLAUSE("RULE CLAUSE"), DIRECTIVE("DIRECTIVE"),
}

data class ExplorerLocation(val source: String, val line: Int, val text: String)

data class ExplorerNode(
    val id: String,
    val label: String,
    val kind: ExplorerNodeKind,
    val locations: List<ExplorerLocation>,
) {
    val provenance: String
        get() = when (locations.size) {
            0 -> "No workspace definition"
            1 -> "${locations.first().source}:${locations.first().line}"
            else -> "${locations.size} clauses · ${locations.map { it.source }.distinct().size} files"
        }
}

data class ExplorerEdge(val from: String, val to: String, val label: String)
data class ExplorerGraph(val nodes: List<ExplorerNode>, val edges: List<ExplorerEdge>)
data class ExplorerBox(val left: Float, val top: Float, val right: Float, val bottom: Float)

object PrologGraphExplorerModel {
    fun build(
        documents: List<PrologDocument>,
        includeClauses: Boolean = false,
        source: String? = null,
    ): ExplorerGraph {
        val ordered = documents.sortedBy { it.source }
        val definitions = ordered.flatMap { it.clauses }
            .filter { it.kind != PrologClauseKind.DIRECTIVE }
            .groupBy { it.predicate.indicator }
        val nodes = linkedMapOf<String, ExplorerNode>()
        val edges = linkedSetOf<ExplorerEdge>()

        fun predicate(ref: PredicateRef): String {
            val id = "predicate:${ref.indicator}"
            if (id in nodes) return id
            val clauses = definitions[ref.indicator].orEmpty()
            val kinds = clauses.map { it.kind }.toSet()
            val kind = when {
                clauses.isEmpty() -> ExplorerNodeKind.REFERENCE
                kinds.size > 1 -> ExplorerNodeKind.MIXED
                PrologClauseKind.RULE in kinds -> ExplorerNodeKind.RULE
                else -> ExplorerNodeKind.FACT
            }
            nodes[id] = ExplorerNode(id, ref.indicator, kind, clauses.map { it.location() })
            return id
        }

        ordered.filter { source == null || it.source == source }.forEach { document ->
            document.clauses.forEachIndexed { index, clause ->
                val clauseId = "clause:${document.source}:${clause.line}:$index"
                if (clause.kind == PrologClauseKind.DIRECTIVE) {
                    if (includeClauses) {
                        nodes[clauseId] = ExplorerNode(clauseId, clause.text, ExplorerNodeKind.DIRECTIVE, listOf(clause.location()))
                    }
                    return@forEachIndexed
                }
                val predicateId = predicate(clause.predicate)
                val origin = if (includeClauses) {
                    val kind = if (clause.kind == PrologClauseKind.FACT) ExplorerNodeKind.FACT_CLAUSE else ExplorerNodeKind.RULE_CLAUSE
                    nodes[clauseId] = ExplorerNode(clauseId, clause.text, kind, listOf(clause.location()))
                    edges += ExplorerEdge(predicateId, clauseId, "defines")
                    clauseId
                } else predicateId
                clause.bodyPredicates.forEach { ref ->
                    edges += ExplorerEdge(origin, predicate(ref), "references")
                }
            }
        }
        return ExplorerGraph(
            nodes.values.sortedBy { it.id },
            edges.sortedWith(compareBy({ it.from }, { it.to }, { it.label })),
        )
    }

    fun filter(graph: ExplorerGraph, query: String = "", focus: String? = null): ExplorerGraph {
        val term = query.trim()
        if (term.isEmpty() && focus == null) return graph
        val matches = graph.nodes.filter { node ->
            (focus == null || node.id == focus) && (term.isEmpty() ||
                node.label.contains(term, ignoreCase = true) ||
                node.kind.label.contains(term, ignoreCase = true) ||
                node.locations.any { it.source.contains(term, ignoreCase = true) || it.text.contains(term, ignoreCase = true) })
        }.map { it.id }.toSet()
        val visible = matches.toMutableSet()
        graph.edges.forEach { edge ->
            if (edge.from in matches || edge.to in matches) {
                visible += edge.from
                visible += edge.to
            }
        }
        return ExplorerGraph(
            graph.nodes.filter { it.id in visible },
            graph.edges.filter { it.from in visible && it.to in visible },
        )
    }

    fun layout(graph: ExplorerGraph): Map<String, ExplorerBox> {
        val groups = graph.nodes.groupBy { node ->
            when (node.kind) {
                ExplorerNodeKind.RULE, ExplorerNodeKind.MIXED -> 0
                ExplorerNodeKind.FACT_CLAUSE, ExplorerNodeKind.RULE_CLAUSE, ExplorerNodeKind.DIRECTIVE -> 1
                ExplorerNodeKind.FACT -> 2
                ExplorerNodeKind.REFERENCE -> 3
            }
        }.toSortedMap()
        val positions = linkedMapOf<String, ExplorerBox>()
        groups.values.forEachIndexed { column, nodes ->
            nodes.sortedWith(compareBy({ it.label }, { it.id })).forEachIndexed { row, node ->
                val left = 32f + column * 292f
                val top = 32f + row * 128f
                positions[node.id] = ExplorerBox(left, top, left + 232f, top + 96f)
            }
        }
        return positions
    }

    fun hitTest(positions: Map<String, ExplorerBox>, x: Float, y: Float): String? =
        positions.entries.firstOrNull { (_, box) ->
            x >= box.left && x <= box.right && y >= box.top && y <= box.bottom
        }?.key

    fun lineRanges(text: String): List<IntRange> {
        val ranges = mutableListOf<IntRange>()
        var start = 0
        text.forEachIndexed { index, character ->
            if (character == '\n') {
                val end = if (index > start && text[index - 1] == '\r') index - 1 else index
                ranges += start until end
                start = index + 1
            }
        }
        ranges += start until text.length
        return ranges
    }

    private fun PrologClause.location() = ExplorerLocation(source, line, text)
}
