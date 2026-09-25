package ai.zara.org.surfaces

import ai.zara.org.core.OrgTask
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp

@Composable
fun TodoSurface(model: OrgWorkspaceModel) {
    val tasks = model.projection.tasks
    LazyColumn(verticalArrangement = Arrangement.spacedBy(6.dp)) {
        item("summary") {
            Text(
                "${tasks.size} task headings from the canonical corpus",
                color = MaterialTheme.colorScheme.onSurfaceVariant,
            )
        }
        items(tasks, key = { "${it.path}:${it.line}" }) { task ->
            TaskRow(task) { model.cycle(task) }
        }
    }
}

@Composable
internal fun TaskRow(task: OrgTask, onCycle: () -> Unit) {
    Row(
        modifier = Modifier
            .fillMaxWidth()
            .background(MaterialTheme.colorScheme.surface)
            .padding(8.dp),
        horizontalArrangement = Arrangement.spacedBy(8.dp),
    ) {
        TextButton(onClick = onCycle) { Text(task.state) }
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
