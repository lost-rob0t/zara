package ai.zara.org.todo

import ai.zara.org.core.AgendaGroup
import ai.zara.org.core.DoomAgenda
import ai.zara.org.core.DoomOrgProfile
import ai.zara.org.core.OrgTask
import ai.zara.org.storage.OrgTreePermission
import ai.zara.org.storage.OrgTreeRepository
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.compose.setContent
import androidx.activity.result.contract.ActivityResultContracts
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.weight
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

private val OrgTodoScheme = darkColorScheme(
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
            MaterialTheme(colorScheme = OrgTodoScheme) {
                OrgTodoApp()
            }
        }
    }
}

@Composable
private fun OrgTodoApp() {
    val context = LocalContext.current
    var treeUri by remember { mutableStateOf(OrgTreePermission.remembered(context)) }
    var tasks by remember { mutableStateOf(emptyList<OrgTask>()) }
    var status by remember { mutableStateOf("") }
    var captureTitle by rememberSaveable { mutableStateOf("") }

    val repository = remember(treeUri) {
        treeUri?.let { uri -> runCatching { OrgTreeRepository(context, uri) }.getOrNull() }
    }

    fun refresh() {
        val repo = repository ?: return
        runCatching { repo.allTasks() }
            .onSuccess {
                tasks = it
                status = "${it.size} tasks · shared org-core"
            }
            .onFailure { status = it.message ?: "Refresh failed" }
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
                Text("Org Todo", style = MaterialTheme.typography.titleLarge)
                Text(status.ifBlank { DoomOrgProfile.orgRoot }, style = MaterialTheme.typography.labelSmall)
            }
            TextButton(onClick = { picker.launch(treeUri) }) {
                Text(if (treeUri == null) "Open Org tree" else "Change tree")
            }
        }

        if (repository == null) {
            Text("Choose the same Org workspace used by the main Org app. No private todo database is created.")
            Button(onClick = { picker.launch(null) }) {
                Text("Choose Org workspace")
            }
            return@Column
        }

        Row(
            modifier = Modifier.fillMaxWidth(),
            horizontalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            OutlinedTextField(
                value = captureTitle,
                onValueChange = { captureTitle = it },
                modifier = Modifier.weight(1f),
                label = { Text("Quick capture") },
                singleLine = true,
            )
            Button(
                enabled = captureTitle.isNotBlank(),
                onClick = {
                    runCatching {
                        repository.appendAgendaCapture(DoomOrgProfile.captureTodo(captureTitle))
                    }.onSuccess {
                        captureTitle = ""
                        refresh()
                    }.onFailure { status = it.message ?: "Capture failed" }
                },
            ) {
                Text("Add")
            }
        }

        TextButton(onClick = { refresh() }) {
            Text("Refresh")
        }

        val grouped = remember(tasks) { DoomAgenda.grouped(tasks) }
        LazyColumn(
            modifier = Modifier.fillMaxSize(),
            verticalArrangement = Arrangement.spacedBy(6.dp),
        ) {
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
                        TodoRow(
                            task = task,
                            onCycle = {
                                runCatching { repository.cycleTodo(task) }
                                    .onSuccess { refresh() }
                                    .onFailure { status = it.message ?: "TODO update failed" }
                            },
                        )
                    }
                }
            }
        }
    }
}

@Composable
private fun TodoRow(
    task: OrgTask,
    onCycle: () -> Unit,
) {
    Row(
        modifier = Modifier
            .fillMaxWidth()
            .background(MaterialTheme.colorScheme.surface)
            .padding(8.dp),
        horizontalArrangement = Arrangement.spacedBy(8.dp),
    ) {
        TextButton(onClick = onCycle) {
            Text(task.state)
        }
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
            )
        }
    }
}
