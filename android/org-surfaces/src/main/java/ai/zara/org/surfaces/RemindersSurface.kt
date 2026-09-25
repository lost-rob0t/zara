package ai.zara.org.surfaces

import ai.zara.org.core.OrgReminderKind
import ai.zara.org.core.OrgReminders
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
import androidx.compose.runtime.Composable
import androidx.compose.runtime.remember
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

private val reminderFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

@Composable
fun RemindersSurface(model: OrgWorkspaceModel) {
    val reminders = remember(model.projection) { OrgReminders.fromProjection(model.projection) }
    val now = remember(model.projection) { LocalDateTime.now() }

    LazyColumn(verticalArrangement = Arrangement.spacedBy(6.dp)) {
        item("summary") {
            Text(
                "${reminders.size} reminders derived from canonical SCHEDULED/DEADLINE timestamps",
                color = MaterialTheme.colorScheme.onSurfaceVariant,
            )
        }
        items(reminders, key = { it.stableKey }) { reminder ->
            val overdue = reminder.whenLocal.isBefore(now)
            Column(
                modifier = Modifier
                    .fillMaxWidth()
                    .background(MaterialTheme.colorScheme.surface)
                    .padding(8.dp),
            ) {
                Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                    Text(
                        reminder.kind.name,
                        color = if (reminder.kind == OrgReminderKind.DEADLINE) {
                            MaterialTheme.colorScheme.primary
                        } else {
                            MaterialTheme.colorScheme.secondary
                        },
                        style = MaterialTheme.typography.labelMedium,
                    )
                    Text(
                        reminder.whenLocal.format(reminderFormatter),
                        color = if (overdue) {
                            MaterialTheme.colorScheme.primary
                        } else {
                            MaterialTheme.colorScheme.onSurfaceVariant
                        },
                        style = MaterialTheme.typography.labelMedium,
                    )
                    if (!reminder.explicitTime) {
                        Text(
                            "default ${OrgReminders.defaultReminderTime}",
                            color = MaterialTheme.colorScheme.onSurfaceVariant,
                            style = MaterialTheme.typography.labelSmall,
                        )
                    }
                }
                Text(reminder.title)
                Text(
                    "${reminder.taskPath}:${reminder.taskLine} · ${reminder.taskState}",
                    color = MaterialTheme.colorScheme.onSurfaceVariant,
                    style = MaterialTheme.typography.labelSmall,
                )
            }
        }
    }
}
