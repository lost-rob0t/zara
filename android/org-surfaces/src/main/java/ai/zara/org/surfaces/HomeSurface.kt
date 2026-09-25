package ai.zara.org.surfaces

import ai.zara.org.core.OrgReminders
import android.content.Context
import android.content.Intent
import android.net.Uri
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
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.remember
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.unit.dp
import java.time.LocalDate
import java.time.LocalDateTime

@Composable
fun HomeSurface(model: OrgWorkspaceModel) {
    val context = LocalContext.current
    val projection = model.projection
    val today = projection.today ?: LocalDate.now()
    val todays = remember(projection, today) {
        OrgReminders.fromProjection(projection).filter { it.whenLocal.toLocalDate() == today }
    }
    val recentDailies = remember(projection) { projection.dailies.take(3) }
    val fleet = remember { OrgFleetApps.installed(installedOrgPackages(context), exclude = context.packageName) }

    LazyColumn(verticalArrangement = Arrangement.spacedBy(8.dp)) {
        item("today") {
            Text("Today · $today", style = MaterialTheme.typography.titleLarge)
            if (todays.isEmpty()) {
                Text(
                    "Nothing scheduled or due today in the canonical corpus.",
                    color = MaterialTheme.colorScheme.onSurfaceVariant,
                )
            }
        }
        items(todays, key = { it.stableKey }) { reminder ->
            Column(
                modifier = Modifier
                    .fillMaxWidth()
                    .background(MaterialTheme.colorScheme.surface)
                    .padding(8.dp),
            ) {
                Text(
                    "${reminder.kind.name.lowercase()} · ${reminder.whenLocal.toLocalTime()}",
                    color = MaterialTheme.colorScheme.secondary,
                    style = MaterialTheme.typography.labelMedium,
                )
                Text(reminder.title)
            }
        }
        item("fleet-header") {
            Text("Org apps", style = MaterialTheme.typography.titleMedium)
        }
        item("fleet") {
            Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                fleet.forEach { packageName ->
                    val label = OrgFleetApps.labels()[packageName] ?: packageName
                    Text(
                        label,
                        color = MaterialTheme.colorScheme.primary,
                        modifier = Modifier
                            .background(MaterialTheme.colorScheme.surfaceVariant)
                            .clickable {
                                context.packageManager.getLaunchIntentForPackage(packageName)?.let { intent ->
                                    context.startActivity(intent)
                                }
                            }
                            .padding(8.dp),
                    )
                }
            }
        }
        if (recentDailies.isNotEmpty()) {
            item("dailies-header") {
                Text("Recent daily pages", style = MaterialTheme.typography.titleMedium)
            }
            items(recentDailies, key = { it.path }) { entry ->
                Column(
                    modifier = Modifier
                        .fillMaxWidth()
                        .background(MaterialTheme.colorScheme.surface)
                        .padding(8.dp),
                ) {
                    Text(entry.date.toString(), color = MaterialTheme.colorScheme.secondary)
                    Text(entry.path, style = MaterialTheme.typography.labelSmall)
                }
            }
        }
    }
}

fun installedOrgPackages(context: Context): Set<String> =
    OrgFleetApps.siblings.mapNotNull { packageName ->
        context.packageManager.getLaunchIntentForPackage(packageName)?.let { packageName }
    }.toSet()
