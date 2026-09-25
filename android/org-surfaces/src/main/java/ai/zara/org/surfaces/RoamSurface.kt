package ai.zara.org.surfaces

import ai.zara.org.core.OrgRoamGraph
import ai.zara.org.core.OrgRoamNode
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp

@Composable
fun RoamSurface(model: OrgWorkspaceModel) {
    RoamContent(model.projection.roam)
}

@Composable
fun RoamContent(graph: OrgRoamGraph) {
    var query by rememberSaveable { mutableStateOf("") }
    var selectedId by rememberSaveable { mutableStateOf<String?>(null) }
    val visible = remember(graph, query) { graph.search(query) }
    val selected = selectedId?.let(graph.nodes::get)

    LaunchedEffect(graph) {
        if (selectedId != null && selectedId !in graph.nodes) selectedId = null
    }

    Column(verticalArrangement = Arrangement.spacedBy(8.dp)) {
        OutlinedTextField(
            value = query,
            onValueChange = { query = it },
            modifier = Modifier.fillMaxWidth(),
            label = { Text("Search Org-roam nodes") },
            singleLine = true,
        )
        if (selected == null) {
            LazyColumn(verticalArrangement = Arrangement.spacedBy(6.dp)) {
                items(visible, key = { it.id }) { node -> RoamRow(node) { selectedId = node.id } }
            }
        } else {
            RoamDetail(selected, graph) { selectedId = it }
        }
    }
}

@Composable
private fun RoamRow(node: OrgRoamNode, onSelect: () -> Unit) {
    Column(
        modifier = Modifier
            .fillMaxWidth()
            .background(MaterialTheme.colorScheme.surface)
            .clickable(onClick = onSelect)
            .padding(10.dp),
    ) {
        Text(node.title)
        Text(
            "${node.path}:${node.line}",
            color = MaterialTheme.colorScheme.onSurfaceVariant,
            style = MaterialTheme.typography.labelSmall,
        )
    }
}

@Composable
internal fun RoamDetail(node: OrgRoamNode, graph: OrgRoamGraph, onSelect: (String?) -> Unit) {
    val backlinks = remember(graph, node.id) { graph.backlinks(node.id) }
    Column(verticalArrangement = Arrangement.spacedBy(8.dp)) {
        TextButton(onClick = { onSelect(null) }) { Text("Back") }
        Text(node.title, style = MaterialTheme.typography.titleLarge)
        Text("${node.path}:${node.line}", color = MaterialTheme.colorScheme.onSurfaceVariant)
        Text("Backlinks", color = MaterialTheme.colorScheme.secondary)
        backlinks.forEach { link ->
            val source = graph.nodes[link.sourceId]
            TextButton(onClick = { onSelect(link.sourceId) }) {
                Text(source?.title ?: link.sourceId)
            }
        }
    }
}
