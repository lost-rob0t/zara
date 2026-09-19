package ai.zara.org.app

import ai.zara.org.core.AgendaGroup
import ai.zara.org.core.DoomAgenda
import ai.zara.org.core.DoomOrgProfile
import ai.zara.org.core.OrgRoamGraph
import ai.zara.org.core.OrgRoamNode
import ai.zara.org.core.OrgTask
import ai.zara.org.core.OrgWorkspaceProjection
import ai.zara.org.core.OrgWorkspaceProjector
import ai.zara.ui.org.OrgTextRenderer
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
import androidx.compose.foundation.layout.weight
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material3.AlertDialog
import androidx.compose.material3.Button
import androidx.compose.material3.ColorScheme
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
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.unit.dp

private enum class WorkbenchTab { AGENDA, TODO, ROAM, FILES, EDITOR }

private val OutrunScheme: ColorScheme = darkColorScheme(
    primary = Color(0xFFFF4FD8),
    secondary = Color(0xFF45E6FF),
    tertiary = Color(0xFF9D7CFF),
    background = Color(0xFF050510),
    surface = Color(0xFF0B0B1D),
    surfaceVariant = Color(0xFF15152B),
    onPrimary = Color.Black,
    onSecondary = Color.Black,
    onBackground = Color(0xFFF4EEFF),
    onSurface = Color(0xFFF4EEFF),
    onSurfaceVariant = Color(0xFFB8B2D6),
)

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContent {
            MaterialTheme(colorScheme = OutrunScheme) {
                OrgWorkbench()
            }
        }
    }
}

@Composable
private fun OrgWorkbench() {
    val context = LocalContext.current
    var homeRevision by rememberSaveable { mutableStateOf(0) }
    val homeSelection = remember(homeRevision) { OrgHome.selection(context) }
    var files by remember { mutableStateOf(emptyList<OrgFileRef>()) }
    var projection by remember {
        mutableStateOf(OrgWorkspaceProjector.project(emptyMap()))
    }
    var selected by remember { mutableStateOf<OrgFileRef?>(null) }
    var editor by rememberSaveable { mutableStateOf("") }
    var tab by rememberSaveable { mutableStateOf(WorkbenchTab.AGENDA) }
    var preview by rememberSaveable { mutableStateOf(false) }
    var status by remember { mutableStateOf("") }
    var captureOpen by remember { mutableStateOf(false) }

    val repository: OrgRepository? = remember(homeRevision) {
        runCatching { OrgHome.open(context) }.getOrNull()
    }

    fun refresh() {
        val repo = repository ?: return
        runCatching {
            val refreshedFiles = repo.listOrgFiles()
            val documents = refreshedFiles.associate { file ->
                file.relativePath to repo.read(file)
            }
            val refreshedProjection = OrgWorkspaceProjector.project(documents)
            files = refreshedFiles
            projection = refreshedProjection
            status = buildString {
                append(refreshedFiles.size).append(" Org files")
                append(" · ").append(refreshedProjection.tasks.size).append(" tasks")
                append(" · ").append(refreshedProjection.roam.nodes.size).append(" roam nodes")
            }
        }.onFailure { status = it.message ?: "Refresh failed" }
    }

    fun open(file: OrgFileRef) {
        val repo = repository ?: return
        runCatching {
            selected = file
            editor = repo.read(file)
            preview = false
            tab = WorkbenchTab.EDITOR
            status = file.relativePath
        }.onFailure { status = it.message ?: "Open failed" }
    }

    fun openPath(relativePath: String) {
        files.firstOrNull { it.relativePath == relativePath }?.let(::open)
    }

    LaunchedEffect(repository) {
        if (repository == null) {
            files = emptyList()
            projection = OrgWorkspaceProjector.project(emptyMap())
        } else {
            refresh()
        }
    }

    val picker = rememberLauncherForActivityResult(ActivityResultContracts.OpenDocumentTree()) { uri ->
        if (uri != null) {
            runCatching { OrgHome.useCustomSaf(context, uri) }
                .onSuccess {
                    homeRevision += 1
                    status = "Custom Org home connected"
                }
                .onFailure { status = it.message ?: "Unable to retain workspace permission" }
        }
    }

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
                Text("Zara Org", style = MaterialTheme.typography.titleLarge)
                Text(
                    status.ifBlank {
                        when (homeSelection.mode) {
                            OrgHomeMode.SHARED -> "Shared Org home"
                            OrgHomeMode.CUSTOM_SAF -> "Custom Org directory"
                        }
                    },
                    style = MaterialTheme.typography.labelSmall,
                )
            }
            Row(horizontalArrangement = Arrangement.spacedBy(4.dp)) {
                TextButton(onClick = {
                    OrgHome.useShared(context)
                    homeRevision += 1
                    status = "Using shared Org home"
                }) { Text("Shared home") }
                TextButton(onClick = { picker.launch(homeSelection.customTreeUri) }) { Text("Custom dir") }
            }
        }

        Row(horizontalArrangement = Arrangement.spacedBy(6.dp)) {
            WorkbenchTab.entries.forEach { destination ->
                TextButton(onClick = { tab = destination }) {
                    Text(
                        destination.name.lowercase().replaceFirstChar(Char::uppercaseChar),
                        color = if (tab == destination) {
                            MaterialTheme.colorScheme.secondary
                        } else {
                            MaterialTheme.colorScheme.onSurfaceVariant
                        },
                    )
                }
            }
            TextButton(onClick = { refresh() }, enabled = repository != null) { Text("Refresh") }
            TextButton(onClick = { captureOpen = true }, enabled = repository != null) { Text("Capture") }
        }

        if (repository == null) {
            Column(verticalArrangement = Arrangement.spacedBy(12.dp)) {
                if (homeSelection.mode == OrgHomeMode.SHARED) {
                    Text("Shared Org home is unavailable. Install/open Org Sync to provide it, or choose a custom directory for this app.")
                    Button(onClick = { picker.launch(null) }) { Text("Choose custom Org directory") }
                } else {
                    Text("The custom Org directory is unavailable. Re-grant it or return to the shared Org home.")
                    Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                        Button(onClick = { picker.launch(homeSelection.customTreeUri) }) { Text("Re-grant directory") }
                        TextButton(onClick = {
                            OrgHome.useShared(context)
                            homeRevision += 1
                        }) { Text("Use shared home") }
                    }
                }
                Text(
                    "Org files remain canonical; Todo and Roam are rebuildable projections over the same corpus.",
                    color = MaterialTheme.colorScheme.onSurfaceVariant,
                )
            }
        } else {
            when (tab) {
                WorkbenchTab.AGENDA -> AgendaView(
                    tasks = projection.tasks,
                    onCycle = { task ->
                        runCatching { repository.cycleTodo(task) }
                            .onSuccess { refresh() }
                            .onFailure { status = it.message ?: "TODO update failed" }
                    },
                    onOpen = { task -> openPath(task.path) },
                )

                WorkbenchTab.TODO -> TodoView(
                    tasks = projection.openTasks,
                    onCycle = { task ->
                        runCatching { repository.cycleTodo(task) }
                            .onSuccess { refresh() }
                            .onFailure { status = it.message ?: "TODO update failed" }
                    },
                    onOpen = { task -> openPath(task.path) },
                )

                WorkbenchTab.ROAM -> RoamView(
                    graph = projection.roam,
                    onOpen = { node -> openPath(node.path) },
                )

                WorkbenchTab.FILES -> FileView(files = files, onOpen = ::open)
                WorkbenchTab.EDITOR -> EditorView(
                    file = selected,
                    source = editor,
                    preview = preview,
                    onSourceChange = { editor = it },
                    onPreviewChange = { preview = it },
                    onSave = {
                        val file = selected
                        if (file != null) {
                            runCatching { repository.write(file, editor) }
                                .onSuccess {
                                    status = "Saved ${file.relativePath}"
                                    refresh()
                                }
                                .onFailure { status = it.message ?: "Save failed" }
                        }
                    },
                    onTangle = {
                        val file = selected
                        if (file != null) {
                            runCatching {
                                repository.write(file, editor)
                                repository.tangle(file)
                            }.onSuccess { outputs ->
                                status = "Tangled ${outputs.joinToString { it.relativePath }}"
                                refresh()
                            }.onFailure { status = it.message ?: "Tangle failed" }
                        }
                    },
                )
            }
        }
    }

    if (captureOpen && repository != null) {
        CaptureDialog(
            onDismiss = { captureOpen = false },
            onCapture = { title, effort, category, scheduled, deadline ->
                runCatching {
                    repository.appendAgendaCapture(
                        DoomOrgProfile.captureTodo(title, effort, category, scheduled, deadline),
                    )
                }.onSuccess { file ->
                    captureOpen = false
                    status = "Captured to ${file.relativePath}"
                    refresh()
                }.onFailure { status = it.message ?: "Capture failed" }
            },
        )
    }
}

@Composable
private fun AgendaView(
    tasks: List<OrgTask>,
    onCycle: (OrgTask) -> Unit,
    onOpen: (OrgTask) -> Unit,
) {
    val grouped = remember(tasks) { DoomAgenda.grouped(tasks) }
    LazyColumn(verticalArrangement = Arrangement.spacedBy(6.dp)) {
        AgendaGroup.entries.forEach { group ->
            val rows = grouped[group].orEmpty()
            if (rows.isNotEmpty()) {
                item(group) {
                    Text(
                        group.label,
                        style = MaterialTheme.typography.titleMedium,
                        color = MaterialTheme.colorScheme.secondary,
                    )
                }
                items(rows, key = { "${it.path}:${it.line}" }) { task ->
                    TaskRow(task = task, onCycle = { onCycle(task) }, onOpen = { onOpen(task) })
                }
            }
        }
    }
}

@Composable
private fun TodoView(
    tasks: List<OrgTask>,
    onCycle: (OrgTask) -> Unit,
    onOpen: (OrgTask) -> Unit,
) {
    LazyColumn(verticalArrangement = Arrangement.spacedBy(6.dp)) {
        item("todo-summary") {
            Text(
                "${tasks.size} open tasks · same canonical Org corpus",
                color = MaterialTheme.colorScheme.onSurfaceVariant,
                style = MaterialTheme.typography.labelMedium,
            )
        }
        items(tasks, key = { "todo:${it.path}:${it.line}" }) { task ->
            TaskRow(task = task, onCycle = { onCycle(task) }, onOpen = { onOpen(task) })
        }
    }
}

@Composable
private fun TaskRow(
    task: OrgTask,
    onCycle: () -> Unit,
    onOpen: () -> Unit,
) {
    Row(
        modifier = Modifier
            .fillMaxWidth()
            .background(MaterialTheme.colorScheme.surface)
            .clickable(onClick = onOpen)
            .padding(8.dp),
        horizontalArrangement = Arrangement.spacedBy(8.dp),
    ) {
        TextButton(onClick = onCycle) { Text(task.state) }
        Column {
            Text(task.title)
            Text(
                buildString {
                    append(task.path).append(':').append(task.line)
                    task.scheduled?.let { append(" · S ").append(it) }
                    task.deadline?.let { append(" · D ").append(it) }
                    task.effort?.let { append(" · ").append(it) }
                },
                style = MaterialTheme.typography.labelSmall,
                color = MaterialTheme.colorScheme.onSurfaceVariant,
            )
        }
    }
}

@Composable
private fun RoamView(
    graph: OrgRoamGraph,
    onOpen: (OrgRoamNode) -> Unit,
) {
    var query by rememberSaveable { mutableStateOf("") }
    var selectedId by rememberSaveable { mutableStateOf<String?>(null) }
    val visible = remember(graph, query) { graph.search(query) }
    val selected = selectedId?.let(graph.nodes::get)

    LaunchedEffect(graph) {
        if (selectedId !in graph.nodes) selectedId = null
    }

    Column(
        modifier = Modifier.fillMaxSize(),
        verticalArrangement = Arrangement.spacedBy(8.dp),
    ) {
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
            Text(
                "${graph.nodes.size} nodes",
                style = MaterialTheme.typography.labelMedium,
                color = MaterialTheme.colorScheme.secondary,
                modifier = Modifier.padding(top = 18.dp),
            )
        }

        if (graph.duplicateIds.isNotEmpty()) {
            Text(
                "Duplicate IDs excluded: ${graph.duplicateIds.sorted().joinToString()}",
                color = MaterialTheme.colorScheme.error,
                style = MaterialTheme.typography.bodySmall,
            )
        }

        if (selected != null) {
            RoamNodeDetail(
                node = selected,
                graph = graph,
                onClose = { selectedId = null },
                onSelect = { selectedId = it },
                onOpen = { onOpen(selected) },
            )
        } else {
            LazyColumn(
                modifier = Modifier.fillMaxSize(),
                verticalArrangement = Arrangement.spacedBy(6.dp),
            ) {
                items(visible, key = { it.id }) { node ->
                    Column(
                        modifier = Modifier
                            .fillMaxWidth()
                            .background(MaterialTheme.colorScheme.surface)
                            .clickable { selectedId = node.id }
                            .padding(10.dp),
                        verticalArrangement = Arrangement.spacedBy(2.dp),
                    ) {
                        Text(node.title)
                        Text(
                            buildString {
                                append(node.path).append(':').append(node.line)
                                if (node.aliases.isNotEmpty()) {
                                    append(" · aliases: ").append(node.aliases.joinToString())
                                }
                            },
                            style = MaterialTheme.typography.labelSmall,
                            color = MaterialTheme.colorScheme.secondary,
                        )
                    }
                }
            }
        }
    }
}

@Composable
private fun RoamNodeDetail(
    node: OrgRoamNode,
    graph: OrgRoamGraph,
    onClose: () -> Unit,
    onSelect: (String) -> Unit,
    onOpen: () -> Unit,
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
            Column(modifier = Modifier.weight(1f)) {
                Text(node.title, style = MaterialTheme.typography.titleLarge)
                Text(node.id, style = MaterialTheme.typography.labelSmall)
                Text("${node.path}:${node.line}", style = MaterialTheme.typography.labelSmall)
            }
            TextButton(onClick = onOpen) { Text("Open") }
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

@Composable
private fun FileView(files: List<OrgFileRef>, onOpen: (OrgFileRef) -> Unit) {
    LazyColumn(verticalArrangement = Arrangement.spacedBy(4.dp)) {
        items(files, key = { it.relativePath }) { file ->
            Row(
                modifier = Modifier
                    .fillMaxWidth()
                    .clickable { onOpen(file) }
                    .padding(vertical = 10.dp),
            ) {
                Text(file.relativePath, fontFamily = FontFamily.Monospace)
            }
        }
    }
}

@Composable
private fun EditorView(
    file: OrgFileRef?,
    source: String,
    preview: Boolean,
    onSourceChange: (String) -> Unit,
    onPreviewChange: (Boolean) -> Unit,
    onSave: () -> Unit,
    onTangle: () -> Unit,
) {
    if (file == null) {
        Text("Open an Org file from Files, Agenda, Todo, or Roam.")
        return
    }
    Column(verticalArrangement = Arrangement.spacedBy(8.dp)) {
        Text(file.relativePath, fontFamily = FontFamily.Monospace, style = MaterialTheme.typography.labelMedium)
        Row(horizontalArrangement = Arrangement.spacedBy(6.dp)) {
            Button(onClick = onSave) { Text("Save") }
            Button(onClick = onTangle) { Text("Tangle Python/Prolog") }
            TextButton(onClick = { onPreviewChange(!preview) }) { Text(if (preview) "Edit" else "Render") }
        }
        if (preview) {
            val rendered = remember(source) { OrgTextRenderer.renderSource(source, baseFontSp = 16f) }
            SelectionContainer {
                Text(
                    rendered.annotated,
                    fontFamily = FontFamily.Monospace,
                    modifier = Modifier.fillMaxSize(),
                )
            }
        } else {
            OutlinedTextField(
                value = source,
                onValueChange = onSourceChange,
                modifier = Modifier.fillMaxSize(),
                textStyle = MaterialTheme.typography.bodyMedium.copy(fontFamily = FontFamily.Monospace),
                label = { Text("Org source") },
            )
        }
    }
}

@Composable
private fun CaptureDialog(
    onDismiss: () -> Unit,
    onCapture: (String, String, String, String?, String?) -> Unit,
) {
    var title by rememberSaveable { mutableStateOf("") }
    var effort by rememberSaveable { mutableStateOf(DoomOrgProfile.effortChoices.first()) }
    var category by rememberSaveable { mutableStateOf(DoomOrgProfile.categoryChoices.first()) }
    var scheduled by rememberSaveable { mutableStateOf("") }
    var deadline by rememberSaveable { mutableStateOf("") }

    AlertDialog(
        onDismissRequest = onDismiss,
        title = { Text("Capture TODO") },
        text = {
            Column(verticalArrangement = Arrangement.spacedBy(6.dp)) {
                OutlinedTextField(title, { title = it }, label = { Text("Task") })
                OutlinedTextField(effort, { effort = it }, label = { Text("Effort") })
                OutlinedTextField(category, { category = it }, label = { Text("Category") })
                OutlinedTextField(scheduled, { scheduled = it }, label = { Text("Scheduled YYYY-MM-DD Day") })
                OutlinedTextField(deadline, { deadline = it }, label = { Text("Deadline YYYY-MM-DD Day") })
            }
        },
        confirmButton = {
            Button(
                enabled = title.isNotBlank(),
                onClick = {
                    onCapture(
                        title,
                        effort,
                        category,
                        scheduled.takeIf { it.isNotBlank() },
                        deadline.takeIf { it.isNotBlank() },
                    )
                },
            ) { Text("Capture") }
        },
        dismissButton = { TextButton(onClick = onDismiss) { Text("Cancel") } },
    )
}
