package ai.zara.app.ui

import ai.zara.app.projects.ProjectContext
import ai.zara.app.projects.ProjectContextState
import ai.zara.app.projects.ProjectSourceScope
import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
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
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.unit.dp

@Composable
internal fun ProjectBreadcrumb(project: ProjectContext) {
    val tokens = LocalZaraTokens.current
    Text(
        "Chat / ${project.name}",
        modifier = Modifier.fillMaxWidth().padding(top = 10.dp, bottom = 2.dp),
        color = tokens.accentCyan,
        fontFamily = FontFamily.Monospace,
        style = MaterialTheme.typography.labelMedium,
    )
}

@Composable
internal fun ProjectsSurface(
    state: ProjectContextState,
    operationError: String?,
    operationBusy: Boolean,
    onCreateProject: (String) -> Unit,
    onSelectProject: (String?) -> Unit,
    padding: PaddingValues,
) {
    var projectName by rememberSaveable { mutableStateOf("") }
    ScreenBody(padding) {
        ScreenTitle("Projects", "Persistent app-private work contexts")
        state.loadFailure?.let(::ErrorBanner)
        operationError?.takeIf { state.loadFailure == null }?.let(::ErrorBanner)
        SectionCard("ACTIVE CONTEXT") {
            val selected = state.selectedProject
            if (selected == null) {
                MutedNotice("No project is active. Chat uses the ordinary conversation scope.")
            } else {
                KeyValueRow("project", selected.name)
                KeyValueRow("scope", selected.sourceScope.label())
                KeyValueRow("conversation", selected.conversationId ?: "new on first remote turn")
                TextButton(
                    enabled = !operationBusy && state.loadFailure == null,
                    onClick = { onSelectProject(null) },
                ) {
                    Text("Use chat without project")
                }
            }
        }
        SectionCard("REGISTERED PROJECTS") {
            if (state.projects.isEmpty()) {
                MutedNotice("No projects yet. Create one below; Zara will not scan a directory automatically.")
            }
            state.projects.forEach { project ->
                ProjectRow(
                    project = project,
                    selected = project.id == state.selectedProjectId,
                    enabled = !operationBusy && state.loadFailure == null,
                    onSelect = { onSelectProject(project.id) },
                )
            }
        }
        SectionCard("NEW PROJECT") {
            OutlinedTextField(
                value = projectName,
                onValueChange = { projectName = it },
                modifier = Modifier.fillMaxWidth(),
                label = { Text("Project name") },
                enabled = !operationBusy && state.loadFailure == null,
                singleLine = true,
            )
            PrimaryAction(
                label = "Create project",
                enabled = projectName.isNotBlank() && !operationBusy && state.loadFailure == null,
            ) {
                val name = projectName.trim()
                if (name.isNotEmpty()) {
                    projectName = ""
                    onCreateProject(name)
                }
            }
            MutedNotice(
                "This slice stores project metadata and conversation binding only. It grants no filesystem path, recursive crawl, source indexing, or ambient file access.",
            )
        }
    }
}

@Composable
private fun ProjectRow(
    project: ProjectContext,
    selected: Boolean,
    enabled: Boolean,
    onSelect: () -> Unit,
) {
    val tokens = LocalZaraTokens.current
    Surface(
        modifier = Modifier.fillMaxWidth(),
        onClick = onSelect,
        enabled = enabled,
        color = if (selected) tokens.surfaceElevated else tokens.surface,
        border = BorderStroke(1.dp, if (selected) tokens.borderActive else tokens.border),
        shape = MaterialTheme.shapes.medium,
    ) {
        Row(
            modifier = Modifier.fillMaxWidth().padding(horizontal = 12.dp, vertical = 10.dp),
            horizontalArrangement = Arrangement.spacedBy(10.dp),
            verticalAlignment = Alignment.CenterVertically,
        ) {
            Column(Modifier.weight(1f)) {
                Text(
                    project.name,
                    color = tokens.text,
                    style = MaterialTheme.typography.bodyMedium,
                )
                Text(
                    project.sourceScope.label(),
                    color = tokens.textMuted,
                    fontFamily = FontFamily.Monospace,
                    style = MaterialTheme.typography.labelSmall,
                )
            }
            Text(
                if (selected) "ACTIVE" else "OPEN",
                color = if (selected) tokens.accentCyan else tokens.textMuted,
                fontFamily = FontFamily.Monospace,
                style = MaterialTheme.typography.labelSmall,
            )
        }
    }
}

private fun ProjectSourceScope.label(): String = when (this) {
    ProjectSourceScope.AppPrivate -> "app-private · no source crawl"
}
