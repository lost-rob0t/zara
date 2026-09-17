package ai.zara.org.app

import ai.zara.org.core.AgendaGroup
import ai.zara.org.core.DoomAgenda
import ai.zara.org.core.DoomOrgProfile
import ai.zara.org.core.OrgTask
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

private enum class WorkbenchTab { AGENDA, FILES, EDITOR }

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
    var treeUri by remember { mutableStateOf(OrgTreePermission.remembered(context)) }
    var files by remember { mutableStateOf(emptyList<OrgFileRef>()) }
    var tasks by remember { mutableStateOf(emptyList<OrgTask>()) }
    var selected by remember { mutableStateOf<OrgFileRef?>(null) }
    var editor by rememberSaveable { mutableStateOf("") }
    var tab by rememberSaveable { mutableStateOf(WorkbenchTab.AGENDA) }
    var preview by rememberSaveable { mutableStateOf(false) }
    var status by remember { mutableStateOf("") }
    var captureOpen by remember { mutableStateOf(false) }

    val repository = remember(treeUri) {
        treeUri?.let { uri -> runCatching { OrgTreeRepository(context, uri) }.getOrNull() }
    }

    fun refresh() {
        val repo = repository ?: return
        runCatching {
            files = repo.listOrgFiles()
            tasks = repo.allTasks()
            status = "${files.size} Org files · ${tasks.size} tasks"
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

    LaunchedEffect(repository) {
        refresh()
    }

    val picker = rememberLauncherForActivityResult(ActivityResultContracts.OpenDocumentTree()) { uri ->
        if (uri != null) {
            runCatching { OrgTreePermission.remember(context, uri) }
                .onSuccess {
                    treeUri = uri
                    status = "Org workspace connected"
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
                Text(status.ifBlank { DoomOrgProfile.orgRoot }, style = MaterialTheme.typography.labelSmall)
            }
            TextButton(onClick = { picker.launch(treeUri) }) { Text(if (treeUri == null) "Open Org tree" else "Change tree") }
        }

        Row(horizontalArrangement = Arrangement.spacedBy(6.dp)) {
            WorkbenchTab.entries.forEach { destination ->
                TextButton(onClick = { tab = destination }) {
                    Text(
                        destination.name.lowercase().replaceFirstChar(Char::uppercaseChar),
                        color = if (tab == destination) MaterialTheme.colorScheme.secondary else MaterialTheme.colorScheme.onSurfaceVariant,
                    )
                }
            }
            TextButton(onClick = { refresh() }) { Text("Refresh") }
            TextButton(onClick = { captureOpen = true }, enabled = repository != null) { Text("Capture") }
        }

        if (repository == null) {
            Column(verticalArrangement = Arrangement.spacedBy(12.dp)) {
                Text("Pick your Org root. The app uses Android's document-tree permission and keeps the .org files as the source of truth.")
                Button(onClick = { picker.launch(null) }) { Text("Choose ~/Documents/Notes/org") }
                Text("Agenda, TODO edits, captures, and Python/Prolog tangles all write back to that tree.", color = MaterialTheme.colorScheme.onSurfaceVariant)
            }
        } else {
            when (tab) {
                WorkbenchTab.AGENDA -> AgendaView(
                    tasks = tasks,
                    onCycle = { task ->
                        runCatching { repository.cycleTodo(task) }
                            .onSuccess { refresh() }
                            .onFailure { status = it.message ?: "TODO update failed" }
                    },
                    onOpen = { task -> files.firstOrNull { it.relativePath == task.path }?.let(::open) },
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
                                .onSuccess { status = "Saved ${file.relativePath}"; refresh() }
                                .onFailure { status = it.message ?: "Save failed" }
                        }
                    },
                    onTangle = {
                        val file = selected
                        if (file != null) {
                            runCatching { repository.write(file, editor); repository.tangle(file) }
                                .onSuccess { outputs -> status = "Tangled ${outputs.joinToString { it.relativePath }}"; refresh() }
                                .onFailure { status = it.message ?: "Tangle failed" }
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
                    Text(group.label, style = MaterialTheme.typography.titleMedium, color = MaterialTheme.colorScheme.secondary)
                }
                items(rows, key = { "${it.path}:${it.line}" }) { task ->
                    Row(
                        modifier = Modifier
                            .fillMaxWidth()
                            .background(MaterialTheme.colorScheme.surface)
                            .clickable { onOpen(task) }
                            .padding(8.dp),
                        horizontalArrangement = Arrangement.spacedBy(8.dp),
                    ) {
                        TextButton(onClick = { onCycle(task) }) { Text(task.state) }
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
        Text("Open an Org file from Files or Agenda.")
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
