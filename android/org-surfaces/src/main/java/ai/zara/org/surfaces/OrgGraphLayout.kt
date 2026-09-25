package ai.zara.org.surfaces

import ai.zara.org.core.OrgRoamGraph
import kotlin.math.PI
import kotlin.math.cos
import kotlin.math.sin

object OrgGraphLayout {
    data class NodePosition(val id: String, val x: Float, val y: Float)

    fun arrange(
        graph: OrgRoamGraph,
        width: Float,
        height: Float,
        padding: Float = 48f,
    ): List<NodePosition> {
        val ids = graph.nodes.keys.sorted()
        if (ids.isEmpty()) return emptyList()
        val centerX = width / 2f
        val centerY = height / 2f
        val radiusX = (width / 2f) - padding
        val radiusY = (height / 2f) - padding
        return ids.mapIndexed { index, id ->
            val angle = 2.0 * PI * index / ids.size
            NodePosition(
                id = id,
                x = centerX + (radiusX * cos(angle)).toFloat(),
                y = centerY + (radiusY * sin(angle)).toFloat(),
            )
        }
    }

    fun edges(graph: OrgRoamGraph): List<Pair<String, String>> {
        val seen = linkedSetOf<Pair<String, String>>()
        for (node in graph.nodes.values) {
            for (link in node.links) {
                if (link.sourceId in graph.nodes && link.targetId in graph.nodes) {
                    seen += link.sourceId to link.targetId
                }
            }
        }
        return seen.sortedWith(compareBy({ it.first }, { it.second }))
    }
}
