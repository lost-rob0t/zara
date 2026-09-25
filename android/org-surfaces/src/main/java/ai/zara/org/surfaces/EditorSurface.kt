package ai.zara.org.surfaces

import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp

@Composable
fun EditorSurface(model: OrgWorkspaceModel) {
    val repository = model.repository ?: return
    var session by remember { mutableStateOf<OrgEditorSession?>(null) }
    var files by remember { mutableStateOf(repository.listOrgFiles()) }

    val selected = session
    Column(verticalArrangement = Arrangement.spacedBy(8.dp)) {
        if (selected == null) {
            LazyColumn(verticalArrangement = Arrangement.spacedBy(6.dp)) {
                item("summary") {
                    Text(
                        "${files.size} canonical Org files; editing stays lossless plain Org text",
                        color = MaterialTheme.colorScheme.onSurfaceVariant,
                    )
                }
                items(files, key = { it.relativePath }) { file ->
                    OrgPanel {
                        Column(modifier = Modifier.clickable {
                            runCatching { OrgEditorSessions.open(file.relativePath, repository.read(file)) }
                                .onSuccess { session = it }
                                .onFailure { failure -> model.report(failure.message ?: "Unable to open ${file.relativePath}") }
                        }) {
                            Text(file.name)
                            OrgMutedText(file.relativePath)
                        }
                    }
                }
            }
        } else {
            var draft by remember(selected.path) { mutableStateOf(selected.draft) }
            TextButton(onClick = {
                session = null
                files = repository.listOrgFiles()
            }) { Text("Back to files") }
            Text(
                selected.path + if (selected.dirty) " · unsaved" else "",
                color = MaterialTheme.colorScheme.onSurfaceVariant,
                style = MaterialTheme.typography.labelSmall,
            )
            OutlinedTextField(
                value = draft,
                onValueChange = {
                    draft = it
                    session = selected.withDraft(it)
                },
                modifier = Modifier.fillMaxWidth(),
                label = { Text("Lossless Org source") },
            )
            Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                TextButton(
                    enabled = session?.dirty == true,
                    onClick = {
                        val current = session ?: return@TextButton
                        runCatching { repository.read(repository.listOrgFiles().first { it.relativePath == current.path }) }
                            .map { diskText -> OrgEditorSessions.staleReason(current, diskText) }
                            .onSuccess { stale ->
                                if (stale != null) {
                                    model.report(stale)
                                } else {
                                    runCatching {
                                        val file = repository.listOrgFiles().first { it.relativePath == current.path }
                                        repository.write(file, current.draft)
                                    }
                                        .onSuccess {
                                            session = OrgEditorSessions.settle(current, current.draft)
                                            model.report("Saved ${current.path}")
                                        }
                                        .onFailure { failure -> model.report(failure.message ?: "Save failed") }
                                }
                            }
                            .onFailure { failure -> model.report(failure.message ?: "Unable to check ${current.path}") }
                    },
                ) { Text("Save") }
                TextButton(onClick = {
                    session = OrgEditorSessions.open(selected.path, selected.originalText)
                }) { Text("Revert") }
            }
        }
    }
}
