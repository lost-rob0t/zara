package ai.zara.org.roam

import ai.zara.org.core.OrgRoam
import ai.zara.org.core.OrgRoamGraph
import ai.zara.org.core.OrgRoamNode
import ai.zara.org.storage.OrgHome
import ai.zara.org.storage.OrgHomeMode
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.compose.setContent
import androidx.activity.result.contract.ActivityResultContracts
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material3.Button
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.material3.darkColorScheme
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.unit.dp
import java.util.UUID

private val RoamScheme = darkColorScheme(
    primary = Color(0xFFFF4FD8),
    secondary = Color(0xFF45E6FF),
    background = Color(0xFF050510),
    surface = Color(0xFF0B0B1D),
    onPrimary = Color.Black,
    onSecondary = Color.Black,
    onBackground = Color(0xFFF4EEFF),
    onSurface = Color(0xFFF4EEFF),
)

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContent {
            MaterialTheme(colorScheme = RoamScheme) {
                OrgRoamApp()
            }
        }
    }
}

@Composable
private fun OrgRoamApp() {
    val context = LocalContext.current
    var homeRevision by rememberSaveable { mutableStateOf(0) }
    val homeSelection = remember(homeRevision) { OrgHome.selection(context) }
    val repository = remember(homeRevision) { runCatching { OrgHome.open(context) }.getOrNull() }

    var graph by remember { mutableStateOf(OrgRoamGraph(emptyMap(), emptySet())) }
    var query by rememberSaveable { mutableStateOf("") }
    var selectedId by rememberSaveable { mutableStateOf<String?>(null) }
    var newTitle by rememberSaveable { mutableStateOf("") }
    var status by remember { mutableStateOf("") }

    fun refresh() {
        val repo = repository
        if (repo == null) {
            graph = OrgRoamGraph(emptyMap(), emptySet())
            status = "Org home unavailable"
            return
        }

        runCatching {
            val documents = repo.listOrgFiles().associate { file ->
                file.relativePath to repo.read(file)
            }
            OrgRoam.build(documents)
        }.onSuccess {
            graph = it
            status = buildString {
                append(it.nodes.size).append(" nodes")
                if (it.duplicateIds.isNotEmpty()) {
                    append(" · ").append(it.duplicateIds.size).append(" duplicate IDs blocked")
                }
            }
            if (selectedId !in it.nodes) selectedId = null
        }.onFailure {
            status = it.message ?: "Roam index failed"
        }
    }

    LaunchedEffect(repository) { refresh() }

    val directoryPicker = rememberLauncherForActivityResult(
        ActivityResultContracts.OpenDocumentTree(),
    ) { uri ->
        if (uri != null) {
            runCatching { OrgHome.useCustomSaf(context, uri) }
                .onSuccess {
                    homeRevision += 1
                    status = "Custom Org home connected"
                }
                .onFailure { status = it.message ?: "Unable to retain Org home permission" }
        }
    }

    val visible = remember(graph, query) { graph.search(query) }
    val selected = selectedId?.let(graph.nodes::get)

    Column(
        modifier = Modifier
            .fillMaxSize()
            .background(MaterialTheme.colorScheme.background)
            .padding(12.dp),
        verticalArrangement = Arrangement.spacedBy(10.dp),
    ) {
        Row(
            modifier = Modifier.fillMaxWidth(),
            horizontalArrangement = Arrangement.SpaceBetween,
        ) {
            Column {
                Text("Org Roam", style = MaterialTheme.typography.titleLarge)
                Text(status, style = MaterialTheme.typography.labelSmall)
            }
            Row(horizontalArrangement = Arrangement.spacedBy(4.dp)) {
                TextButton(onClick = {
                    OrgHome.useShared(context)
                    homeRevision += 1
                    status = "Using shared Org home"
                }) { Text("Shared home") }
                TextButton(onClick = { directoryPicker.launch(homeSelection.customTreeUri) }) {
                    Text("Custom dir")
                }
            }
        }

        if (repository == null) {
            Text(
                if (homeSelection.mode == OrgHomeMode.SHARED) {
                    "Shared Org home is unavailable. Install/open Org Sync or choose a custom directory."
                } else {
                    "The custom Org directory is unavailable."
                },
            )
            return@Column
        }

        Row(
            modifier = Modifier.fillMaxWidth(),
            horizontalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            OutlinedTextField(
                value = query,
                onValueChange = { query = it },
                modifier = Modifier.weight(1f),
                label = { Text("Search nodes, aliases, tags, IDs") },
                singleLine = true,
            )
            TextButton(onClick = { refresh() }) { Text("Reindex") }
        }

        Row(
            modifier = Modifier.fillMaxWidth(),
            horizontalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            OutlinedTextField(
                value = newTitle,
                onValueChange = { newTitle = it },
                modifier = Modifier.weight(1f),
                label = { Text("New node title") },
                singleLine = true,
            )
            Button(
                enabled = newTitle.isNotBlank(),
                onClick = {
                    val repo = repository ?: return@Button
                    val id = UUID.randomUUID().toString()
                    val slug = newTitle.trim()
                        .lowercase()
                        .replace(Regex("[^a-z0-9]+"), "-")
                        .trim('-')
                        .ifBlank { "node" }
                    val path = "roam/$slug-${id.take(8)}.org"
                    val source = buildString {
                        append("* ").append(newTitle.trim()).append('\n')
                        append(":PROPERTIES:\n")
                        append(":ID: ").append(id).append('\n')
                        append(":END:\n")
                    }
                    runCatching { repo.writeRelative(path, source) }
                        .onSuccess {
                            newTitle = ""
                            refresh()
                            selectedId = id
                            status = "Created $path"
                        }
                        .onFailure { status = it.message ?: "Node creation failed" }
                },
            ) { Text("Create") }
        }

        if (graph.duplicateIds.isNotEmpty()) {
            Text(
                "Duplicate IDs are excluded from the active graph: ${graph.duplicateIds.sorted().joinToString()}",
                color = MaterialTheme.colorScheme.error,
                style = MaterialTheme.typography.bodySmall,
            )
        }

        if (selected != null) {
            NodeDetail(
                node = selected,
                graph = graph,
                onClose = { selectedId = null },
                onSelect = { selectedId = it },
            )
        } else {
            LazyColumn(
                modifier = Modifier.fillMaxSize(),
                verticalArrangement = Arrangement.spacedBy(6.dp),
            ) {
                items(visible, key = { it.id }) { node ->
                    NodeRow(node = node, onClick = { selectedId = node.id })
                }
            }
        }
    }
}

@Composable
private fun NodeRow(
    node: OrgRoamNode,
    onClick: () -> Unit,
) {
    Column(
        modifier = Modifier
            .fillMaxWidth()
            .background(MaterialTheme.colorScheme.surface)
            .clickable(onClick = onClick)
            .padding(10.dp),
        verticalArrangement = Arrangement.spacedBy(2.dp),
    ) {
        Text(node.title)
        Text(
            buildString {
                append(node.id)
                .append(" · ")
                .append(node.path)
                .append(':')
                .append(node.line)
                if (node.aliases.isNotEmpty()) {
                    append(" · aliases: ").append(node.aliases.joinToString())
                }
            },
            style = MaterialTheme.typography.labelSmall,
            color = MaterialTheme.colorScheme.secondary,
        )
    }
}

@Composable
private fun NodeDetail(
    node: OrgRoamNode,
    graph: OrgRoamGraph,
    onClose: () -> Unit,
    onSelect: (String) -> Unit,
) {
    val backlinks = remember(graph, node.id) { graph.backlinks(node.id) }
    Column(
        modifier = Modifier.fillMaxSize(),
        verticalArrangement = Arrangement.spacedBy(8.dp),
    ) {
        Row(
            modifier = Modifier.fillMaxWidth(),
            horizontalArrangement = Arrangement.SpaceBetween,
        ) {
            Column {
                Text(node.title, style = MaterialTheme.typography.titleLarge)
                Text(node.id, style = MaterialTheme.typography.labelSmall)
                Text("${node.path}:${node.line}", style = MaterialTheme.typography.labelSmall)
            }
            TextButton(onClick = onClose) { Text("Back") }
        }

        Text("Outlinks", style = MaterialTheme.typography.titleMedium)
        if (node.links.isEmpty()) {
            Text("No id: outlinks")
        } else {
            node.links.forEach { link ->
                TextButton(
                    onClick = { if (link.targetId in graph.nodes) onSelect(link.targetId) },
                    enabled = link.targetId in graph.nodes,
                ) {
                    Text(link.label ?: link.targetId)
                }
            }
        }

        Text("Backlinks", style = MaterialTheme.typography.titleMedium)
        if (backlinks.isEmpty()) {
            Text("No backlinks")
        } else {
            backlinks.forEach { link ->
                TextButton(
                    onClick = { if (link.sourceId in graph.nodes) onSelect(link.sourceId) },
                ) {
                    Text(graph.nodes[link.sourceId]?.title ?: link.sourceId)
                }
            }
        }
    }
}
