package ai.zara.app.prolog

import kotlin.math.ceil
import kotlin.math.sqrt

data class PrologGraphLocation(val source: String, val line: Int, val definition: Boolean)

class PrologGraphBrowser private constructor(
    val documents: List<PrologDocument>,
    val graph: LogicGraph,
    private val origins: Map<String, List<PrologGraphLocation>>,
) {
    fun locations(nodeId: String): List<PrologGraphLocation> = origins[nodeId].orEmpty()

    fun visibleGraph(
        source: String? = null,
        predicatesOnly: Boolean = false,
        query: String = "",
    ): LogicGraph {
        val scope = source?.let { name ->
            documents.firstOrNull { it.source == name }?.graph?.nodes.orEmpty().map { it.id }.toSet()
        }
        val needle = query.trim()
        val nodes = graph.nodes.filter { node ->
            (scope == null || node.id in scope) &&
                (!predicatesOnly || node.kind == LogicNodeKind.PREDICATE) &&
                (needle.isEmpty() || node.label.contains(needle, ignoreCase = true) ||
                    locations(node.id).any { it.source.contains(needle, ignoreCase = true) })
        }
        val ids = nodes.map { it.id }.toSet()
        return LogicGraph(nodes, graph.edges.filter { it.from in ids && it.to in ids })
    }

    companion object {
        fun from(documents: List<PrologDocument>): PrologGraphBrowser {
            require(documents.map { it.source }.distinct().size == documents.size) {
                "Graph source names must be unique"
            }
            val definitions = linkedMapOf<String, MutableList<PrologGraphLocation>>()
            val references = linkedMapOf<String, MutableList<PrologGraphLocation>>()
            documents.forEach { document ->
                document.clauses.forEach { clause ->
                    val id = "predicate:${clause.predicate.indicator}"
                    definitions.getOrPut(id) { mutableListOf() }.add(
                        PrologGraphLocation(clause.source, clause.line, true),
                    )
                    clause.bodyPredicates.forEach { predicate ->
                        references.getOrPut("predicate:${predicate.indicator}") { mutableListOf() }.add(
                            PrologGraphLocation(clause.source, clause.line, false),
                        )
                    }
                }
            }
            val grouped = documents.flatMap { it.graph.nodes }.groupBy { it.id }
            val origins = grouped.mapValues { (id, nodes) ->
                (definitions[id] ?: references[id] ?: nodes.map { node ->
                    PrologGraphLocation(node.source, node.line, node.kind == LogicNodeKind.CLAUSE)
                }).distinct()
            }
            val nodes = grouped.map { (id, candidates) ->
                val origin = origins.getValue(id).first()
                candidates.first().copy(source = origin.source, line = origin.line)
            }
            val ids = nodes.map { it.id }.toSet()
            val edges = documents.flatMap { it.graph.edges }.distinct()
                .filter { it.from in ids && it.to in ids }
            return PrologGraphBrowser(documents.toList(), LogicGraph(nodes, edges), origins)
        }
    }
}

data class PrologLineRange(val start: Int, val endExclusive: Int)

class PrologSourceLines(text: String) {
    val ranges: List<PrologLineRange> = buildList {
        var start = 0
        text.forEachIndexed { index, character ->
            if (character == '\n') {
                val end = if (index > start && text[index - 1] == '\r') index - 1 else index
                add(PrologLineRange(start, end))
                start = index + 1
            }
        }
        add(PrologLineRange(start, text.length))
    }

    fun clampLine(line: Int): Int = line.coerceIn(1, ranges.size)
}

data class PrologGraphPosition(val x: Float, val y: Float)

class PrologGraphLayout(nodes: List<LogicGraphNode>, fontScale: Float = 1f) {
    val nodeWidth = 208f
    val nodeHeight = 80f * fontScale.coerceIn(1f, 4f)
    private val gap = 32f
    private val padding = 16f
    private val columns = ceil(sqrt(nodes.size.coerceAtLeast(1).toDouble())).toInt()
    private val rows = (nodes.size + columns - 1) / columns
    private val nodeIds = nodes.map { it.id }
    val width = 2 * padding + columns * (nodeWidth + gap) - gap
    val height = 2 * padding + rows.coerceAtLeast(1) * (nodeHeight + gap) - gap
    val positions: Map<String, PrologGraphPosition> = nodes.mapIndexed { index, node ->
        node.id to PrologGraphPosition(
            padding + (index % columns) * (nodeWidth + gap),
            padding + (index / columns) * (nodeHeight + gap),
        )
    }.toMap()

    fun hitTest(x: Float, y: Float): String? {
        if (!x.isFinite() || !y.isFinite() || x < padding || y < padding) return null
        val column = ((x - padding) / (nodeWidth + gap)).toInt()
        val row = ((y - padding) / (nodeHeight + gap)).toInt()
        if (column !in 0 until columns || row !in 0 until rows) return null
        val id = nodeIds.getOrNull(row * columns + column) ?: return null
        val position = positions.getValue(id)
        return id.takeIf { x <= position.x + nodeWidth && y <= position.y + nodeHeight }
    }
}

fun prologCanOpenSource(
    selectedName: String,
    draft: String,
    savedText: String,
    targetName: String,
): Boolean = targetName == selectedName || draft == savedText
