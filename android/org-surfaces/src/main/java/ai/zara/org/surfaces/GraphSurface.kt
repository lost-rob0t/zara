package ai.zara.org.surfaces

import androidx.compose.foundation.Canvas
import androidx.compose.foundation.gestures.detectTapGestures
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.unit.dp
import kotlin.math.sqrt

@Composable
fun GraphSurface(model: OrgWorkspaceModel) {
    GraphContent(model.projection.roam)
}

@Composable
fun GraphContent(graph: ai.zara.org.core.OrgRoamGraph) {
    var selectedId by rememberSaveable { mutableStateOf<String?>(null) }
    val selected = selectedId?.let(graph.nodes::get)
    val nodeColor = MaterialTheme.colorScheme.secondary
    val edgeColor = MaterialTheme.colorScheme.onSurfaceVariant.copy(alpha = 0.4f)
    val selectedColor = MaterialTheme.colorScheme.primary

    Column(verticalArrangement = Arrangement.spacedBy(8.dp)) {
        if (graph.nodes.isEmpty()) {
            Text(
                "No Org-roam nodes yet. Give headings ID or CUSTOM_ID properties and link them with id: links.",
                color = MaterialTheme.colorScheme.onSurfaceVariant,
            )
        }
        Canvas(
            modifier = Modifier
                .fillMaxWidth()
                .height(320.dp)
                .pointerInput(graph) {
                    detectTapGestures { tap ->
                        val positions = OrgGraphLayout.arrange(graph, size.width.toFloat(), size.height.toFloat())
                        selectedId = positions
                            .map { position -> position to distance(position, tap) }
                            .minByOrNull { (_, distance) -> distance }
                            ?.takeIf { (_, distance) -> distance <= 48f }
                            ?.first
                            ?.id
                    }
                },
        ) {
            val positions = OrgGraphLayout.arrange(graph, size.width, size.height)
            val coordinates = positions.associate { it.id to Offset(it.x, it.y) }
            OrgGraphLayout.edges(graph).forEach { (source, target) ->
                val from = coordinates[source]
                val to = coordinates[target]
                if (from != null && to != null) {
                    drawLine(color = edgeColor, start = from, end = to, strokeWidth = 3f)
                }
            }
            positions.forEach { position ->
                drawCircle(
                    color = if (position.id == selectedId) selectedColor else nodeColor,
                    radius = if (position.id == selectedId) 26f else 18f,
                    center = Offset(position.x, position.y),
                )
            }
        }
        if (selected != null) {
            RoamDetail(selected, graph) { selectedId = it }
        } else {
            Text(
                "${graph.nodes.size} nodes · ${OrgGraphLayout.edges(graph).size} id: links · tap a node for backlinks",
                color = MaterialTheme.colorScheme.onSurfaceVariant,
            )
        }
    }
}

private fun distance(position: OrgGraphLayout.NodePosition, offset: Offset): Float {
    val deltaX = position.x - offset.x
    val deltaY = position.y - offset.y
    return sqrt(deltaX * deltaX + deltaY * deltaY)
}
