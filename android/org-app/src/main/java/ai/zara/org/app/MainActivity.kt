package ai.zara.org.app

import ai.zara.org.core.OrgDailyEntry
import ai.zara.org.core.OrgRoamGraph
import ai.zara.org.core.OrgRoamNode
import ai.zara.org.core.OrgTask
import ai.zara.org.core.OrgWorkspaceProjection
import ai.zara.org.core.OrgWorkspaceProjector
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

private enum class OrgSurface { TODO, ROAM, DAILY }

private val OrgScheme = darkColorScheme(
    primary = Color(0xFFFF4FD8),
    secondary = Color(0xFF45E6FF),
    background = Color(0xFF050510),
    surface = Color(0xFF0B0B1D),
    surfaceVariant = Color(0xFF15152B),
    onBackground = Color(0xFFF4EEFF),
    onSurface = Color(0xFFF4EEFF),
    onSurfaceVariant = Color(0xFFB8B2D6),
)

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContent {
            MaterialTheme(colorScheme = OrgScheme) {
                OrgWorkspace()
            }
        }
    }
}

@Composable
private fun OrgWorkspace() {
    val context = LocalContext.current
    var homeRevision by rememberSaveable { mutableStateOf(0) }
    val home = remember(homeRevision) { OrgHome.selection(context) }
    val repository = remember(homeRevision) { runCatching { OrgHome.open(context) }.getOrNull() }
    var projection by remember { mutableStateOf(OrgWorkspaceProjector.project(emptyMap())) }
    var surface by rememberSaveable { mutableStateOf(OrgSurface.TODO) }
    var status by remember { mutableStateOf("") }

    fun refresh() {
        val repo = repository ?: return
        runCatching {
            val files = repo.listOrgFiles()
            val documents = files.associate { it.relativePath to repo.read(it) }
            projection = OrgWorkspaceProjector.project(documents)
            status = "${files.size} files · ${projection.tasks.size} tasks · ${projection.roam.nodes.size} nodes"
        }.onFailure { status = it.message ?: "Unable to project Org workspace" }
    }

    fun cycle(task: OrgTask) {
        val repo = repository ?: return
        runCatching { repo.cycleTodo(task) }
            .onSuccess { refresh() }
            .onFailure { status = it.message ?: "TODO update failed" }
    }

    LaunchedEffect(repository) {
        projection = OrgWorkspaceProjector.project(emptyMap())
        if (repository != null) refresh()
    }

    val picker = rememberLauncherForActivityResult(ActivityResultContracts.OpenDocumentTree()) { uri ->
        if (uri != null) {
            runCatching { OrgHome.useCustomSaf(context, uri) }
                .onSuccess {
                    homeRevision += 1
                    status = "Custom Org workspace connected"
                }
                .onFailure { status = it.message ?: "Unable to retain Org workspace permission" }
        }
    }

    Column(
        modifier = Modifier
            .fillMaxSize()
            .background(MaterialTheme.colorScheme.background)
            .padding(14.dp),
        verticalArrangement = Arrangement.spacedBy(10.dp),
    ) {
        Text("Org", style = MaterialTheme.typography.headlineSmall)
        Text(
            status.ifBlank {
                when (home.mode) {
                    OrgHomeMode.SHARED -> "Shared canonical Org workspace"
                    OrgHomeMode.CUSTOM_SAF -> "Custom canonical Org workspace"
                }
            },
            color = MaterialTheme.colorScheme.onSurfaceVariant,
            style = MaterialTheme.typography.labelMedium,
        )

        Row(horizontalArrangement = Arrangement.spacedBy(4.dp)) {
            OrgSurface.entries.forEach { candidate ->
                TextButton(onClick = { surface = candidate }) {
                    Text(
                        candidate.name.lowercase().replaceFirstChar(Char::uppercaseChar),
                        color = if (surface == candidate) {
                            MaterialTheme.colorScheme.secondary
                        } else {
                            MaterialTheme.colorScheme.onSurfaceVariant
                        },
                    )
                }
            }
            TextButton(onClick = { refresh() }, enabled = repository != null) { Text("Refresh") }
        }

        if (repository == null) {
            WorkspaceUnavailable(
                shared = home.mode == OrgHomeMode.SHARED,
                onChooseDirectory = { picker.launch(home.customTreeUri) },
                onUseShared = {
                    OrgHome.useShared(context)
                    homeRevision += 1
                },
            )
        } else {
            when (surface) {
                OrgSurface.TODO -> TodoSurface(projection.tasks, ::cycle)
                OrgSurface.ROAM -> RoamSurface(projection.roam)
                OrgSurface.DAILY -> DailySurface(projection)
            }
        }
    }
}

@Composable
private fun WorkspaceUnavailable(
    shared: Boolean,
    onChooseDirectory: () -> Unit,
    onUseShared: () -> Unit,
) {
    Column(verticalArrangement = Arrangement.spacedBy(10.dp)) {
        Text(
            if (shared) {
                "Shared Org workspace is unavailable. Choose a directory or install/connect the canonical Org Sync provider."
            } else {
                "The selected Org directory is unavailable. Re-grant it or return to the shared workspace."
            },
        )
        Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
            Button(onClick = onChooseDirectory) { Text(if (shared) "Choose Org directory" else "Re-grant directory") }
            if (!shared) TextButton(onClick = onUseShared) { Text("Use shared workspace") }
        }
        Text(
            "Ordinary Org files remain canonical. Todo, Roam, and Daily are derived projections; this app owns no shadow note/task database.",
            color = MaterialTheme.colorScheme.onSurfaceVariant,
        )
    }
}

@Composable
private fun TodoSurface(tasks: List<OrgTask>, onCycle: (OrgTask) -> Unit) {
    LazyColumn(verticalArrangement = Arrangement.spacedBy(6.dp)) {
        item("summary") {
            Text(
                "${tasks.size} task headings from the canonical corpus",
                color = MaterialTheme.colorScheme.onSurfaceVariant,
            )
        }
        items(tasks, key = { "${it.path}:${it.line}" }) { task ->
            Row(
                modifier = Modifier
                    .fillMaxWidth()
                    .background(MaterialTheme.colorScheme.surface)
                    .padding(8.dp),
                horizontalArrangement = Arrangement.spacedBy(8.dp),
            ) {
                TextButton(onClick = { onCycle(task) }) { Text(task.state) }
                Column {
                    Text(task.title)
                    Text(
                        "${task.path}:${task.line}",
                        color = MaterialTheme.colorScheme.onSurfaceVariant,
                        style = MaterialTheme.typography.labelSmall,
                    )
                }
            }
        }
    }
}

@Composable
private fun RoamSurface(graph: OrgRoamGraph) {
    var query by rememberSaveable { mutableStateOf("") }
    var selectedId by rememberSaveable { mutableStateOf<String?>(null) }
    val visible = remember(graph, query) { graph.search(query) }
    val selected = selectedId?.let(graph.nodes::get)

    LaunchedEffect(graph) {
        if (selectedId !in graph.nodes) selectedId = null
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
private fun RoamDetail(node: OrgRoamNode, graph: OrgRoamGraph, onSelect: (String?) -> Unit) {
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

@Composable
private fun DailySurface(projection: OrgWorkspaceProjection) {
    if (projection.today == null) {
        Column(verticalArrangement = Arrangement.spacedBy(8.dp)) {
            Text("Daily view is not configured", style = MaterialTheme.typography.titleMedium)
            Text(
                "Waiting for canonical Daily configuration. No directory, filename pattern, or timezone is guessed by the Android UI.",
                color = MaterialTheme.colorScheme.onSurfaceVariant,
            )
        }
        return
    }

    LazyColumn(verticalArrangement = Arrangement.spacedBy(8.dp)) {
        item("today") {
            Text("Today · ${projection.today}", style = MaterialTheme.typography.titleLarge)
            if (projection.dailies.none { it.path == projection.todayPath }) {
                Text(
                    "No canonical file exists at ${projection.todayPath}; the UI does not synthesize one.",
                    color = MaterialTheme.colorScheme.onSurfaceVariant,
                )
            }
        }
        items(projection.dailies, key = OrgDailyEntry::path) { entry ->
            Column(
                modifier = Modifier
                    .fillMaxWidth()
                    .background(MaterialTheme.colorScheme.surface)
                    .padding(10.dp),
            ) {
                Text(entry.date.toString(), color = MaterialTheme.colorScheme.secondary)
                Text(entry.path, style = MaterialTheme.typography.labelSmall)
                Text(entry.source, maxLines = 12)
            }
        }
    }
}
