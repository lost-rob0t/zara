package ai.zara.org.surfaces

import ai.zara.org.core.OrgDailyEntry
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp

@Composable
fun DailySurface(model: OrgWorkspaceModel) {
    val projection = model.projection
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
            OrgPanel {
                Text(entry.date.toString(), color = LocalOrgTokens.current.accentCyan)
                OrgMutedText(entry.path)
                Text(entry.source, maxLines = 12)
            }
        }
    }
}
