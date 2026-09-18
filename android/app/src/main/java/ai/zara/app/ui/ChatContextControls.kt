package ai.zara.app.ui

import ai.zara.app.context.ChatContextAttachment
import ai.zara.app.projects.ProjectContext
import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.horizontalScroll
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.rememberScrollState
import androidx.compose.material3.AlertDialog
import androidx.compose.material3.DropdownMenu
import androidx.compose.material3.DropdownMenuItem
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.semantics.contentDescription
import androidx.compose.ui.semantics.semantics
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp

@Composable
internal fun ProjectsMainShortcut(
    activeProject: ProjectContext?,
    onOpenProjects: () -> Unit,
) {
    val tokens = LocalZaraTokens.current
    Row(
        modifier = Modifier.fillMaxWidth(),
        horizontalArrangement = Arrangement.SpaceBetween,
    ) {
        TextButton(
            onClick = onOpenProjects,
            modifier = Modifier.semantics { contentDescription = "Open Projects" },
        ) {
            Text("◇ Projects", color = tokens.accentCyan)
        }
        activeProject?.let {
            Text(
                "ACTIVE · ${it.name}",
                modifier = Modifier.padding(top = 14.dp, end = 8.dp),
                color = tokens.textMuted,
                fontFamily = FontFamily.Monospace,
                style = MaterialTheme.typography.labelSmall,
            )
        }
    }
}

@Composable
internal fun ChatContextStrip(
    attachments: List<ChatContextAttachment>,
    enabled: Boolean,
    onRemove: (String) -> Unit,
) {
    if (attachments.isEmpty()) return
    val tokens = LocalZaraTokens.current
    Row(
        modifier = Modifier
            .fillMaxWidth()
            .horizontalScroll(rememberScrollState())
            .padding(bottom = 6.dp),
        horizontalArrangement = Arrangement.spacedBy(6.dp),
    ) {
        attachments.forEach { attachment ->
            Surface(
                color = tokens.surfaceElevated,
                border = BorderStroke(1.dp, tokens.borderActive),
                shape = MaterialTheme.shapes.large,
            ) {
                Row {
                    Text(
                        attachment.name,
                        modifier = Modifier.padding(start = 10.dp, top = 7.dp, bottom = 7.dp),
                        color = tokens.text,
                        maxLines = 1,
                        style = MaterialTheme.typography.labelMedium,
                    )
                    TextButton(
                        onClick = { onRemove(attachment.id) },
                        enabled = enabled,
                        modifier = Modifier.semantics {
                            contentDescription = "Remove context ${attachment.name}"
                        },
                    ) {
                        Text("×", color = tokens.textMuted)
                    }
                }
            }
        }
    }
}

@Composable
internal fun ChatPlusButton(
    projects: List<ProjectContext>,
    enabled: Boolean,
    onPickFiles: () -> Unit,
    onAddTextContext: (String) -> Unit,
    onAddChatToProject: (String) -> Unit,
    onOpenProjects: () -> Unit,
) {
    var expanded by rememberSaveable { mutableStateOf(false) }
    var showTextContext by rememberSaveable { mutableStateOf(false) }
    var showProjectPicker by rememberSaveable { mutableStateOf(false) }
    var textContext by rememberSaveable { mutableStateOf("") }
    val tokens = LocalZaraTokens.current

    androidx.compose.foundation.layout.Box {
        TextButton(
            onClick = { expanded = true },
            enabled = enabled,
            modifier = Modifier.semantics { contentDescription = "Add context or project" },
        ) {
            Text("+", color = tokens.accentCyan, fontSize = 22.sp)
        }
        DropdownMenu(
            expanded = expanded,
            onDismissRequest = { expanded = false },
        ) {
            DropdownMenuItem(
                text = { Text("Upload files") },
                onClick = {
                    expanded = false
                    onPickFiles()
                },
            )
            DropdownMenuItem(
                text = { Text("Add text context") },
                onClick = {
                    expanded = false
                    showTextContext = true
                },
            )
            DropdownMenuItem(
                text = { Text(if (projects.isEmpty()) "Create a project" else "Add chat to project") },
                onClick = {
                    expanded = false
                    if (projects.isEmpty()) onOpenProjects() else showProjectPicker = true
                },
            )
            DropdownMenuItem(
                text = { Text("Open Projects") },
                onClick = {
                    expanded = false
                    onOpenProjects()
                },
            )
        }
    }

    if (showTextContext) {
        AlertDialog(
            onDismissRequest = {
                showTextContext = false
                textContext = ""
            },
            title = { Text("Add text context") },
            text = {
                Column {
                    Text(
                        "This context stays attached to the current chat or active project until you remove it.",
                        color = tokens.textMuted,
                        style = MaterialTheme.typography.bodySmall,
                    )
                    OutlinedTextField(
                        value = textContext,
                        onValueChange = { textContext = it },
                        modifier = Modifier.fillMaxWidth().padding(top = 10.dp),
                        minLines = 4,
                        maxLines = 10,
                        placeholder = { Text("Paste notes, facts, or reference text…") },
                    )
                }
            },
            confirmButton = {
                TextButton(
                    enabled = textContext.isNotBlank(),
                    onClick = {
                        val value = textContext.trim()
                        textContext = ""
                        showTextContext = false
                        if (value.isNotEmpty()) onAddTextContext(value)
                    },
                ) {
                    Text("Add")
                }
            },
            dismissButton = {
                TextButton(
                    onClick = {
                        showTextContext = false
                        textContext = ""
                    },
                ) {
                    Text("Cancel")
                }
            },
        )
    }

    if (showProjectPicker) {
        AlertDialog(
            onDismissRequest = { showProjectPicker = false },
            title = { Text("Add chat to project") },
            text = {
                Column {
                    Text(
                        "Choose a project. Zara will bind the current remote conversation when one exists and copy the chat's explicit context into that project.",
                        color = tokens.textMuted,
                        style = MaterialTheme.typography.bodySmall,
                    )
                    Column(
                        modifier = Modifier
                            .fillMaxWidth()
                            .padding(top = 8.dp)
                            .horizontalScroll(rememberScrollState()),
                    ) {
                        projects.forEach { project ->
                            TextButton(
                                onClick = {
                                    showProjectPicker = false
                                    onAddChatToProject(project.id)
                                },
                            ) {
                                Text(project.name)
                            }
                        }
                    }
                }
            },
            confirmButton = {},
            dismissButton = {
                TextButton(onClick = { showProjectPicker = false }) {
                    Text("Cancel")
                }
            },
        )
    }
}
