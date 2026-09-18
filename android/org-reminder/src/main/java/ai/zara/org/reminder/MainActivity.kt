package ai.zara.org.reminder

import ai.zara.org.core.OrgReminderSpec
import ai.zara.org.core.OrgReminders
import ai.zara.org.storage.OrgHome
import ai.zara.org.storage.OrgHomeMode
import android.Manifest
import android.app.AlarmManager
import android.content.Intent
import android.net.Uri
import android.os.Build
import android.os.Bundle
import android.provider.Settings
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
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material3.Button
import androidx.compose.material3.MaterialTheme
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

private val ReminderScheme = darkColorScheme(
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
            MaterialTheme(colorScheme = ReminderScheme) {
                OrgReminderApp()
            }
        }
    }
}

@Composable
private fun OrgReminderApp() {
    val context = LocalContext.current
    var homeRevision by rememberSaveable { mutableStateOf(0) }
    val homeSelection = remember(homeRevision) { OrgHome.selection(context) }
    val repository = remember(homeRevision) { runCatching { OrgHome.open(context) }.getOrNull() }
    var reminders by remember { mutableStateOf(emptyList<OrgReminderSpec>()) }
    var status by remember { mutableStateOf("") }

    fun refresh() {
        reminders = repository?.let { OrgReminders.fromTasks(it.allTasks()) }.orEmpty()
        status = if (repository == null) {
            "Org home unavailable"
        } else {
            "${reminders.size} derived reminders"
        }
    }

    LaunchedEffect(repository) { refresh() }

    val directoryPicker = rememberLauncherForActivityResult(
        ActivityResultContracts.OpenDocumentTree(),
    ) { uri ->
        if (uri != null) {
            runCatching { OrgHome.useCustomSaf(context, uri) }
                .onSuccess {
                    homeRevision += 1
                    status = "Custom Org home connected"
                }
                .onFailure { status = it.message ?: "Unable to retain Org home permission" }
        }
    }

    val notificationPermission = rememberLauncherForActivityResult(
        ActivityResultContracts.RequestPermission(),
    ) { granted ->
        status = if (granted) "Notifications enabled" else "Notifications denied"
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
                Text("Org Reminder", style = MaterialTheme.typography.titleLarge)
                Text(status, style = MaterialTheme.typography.labelSmall)
            }
            Row(horizontalArrangement = Arrangement.spacedBy(4.dp)) {
                TextButton(onClick = {
                    OrgHome.useShared(context)
                    homeRevision += 1
                    status = "Using shared Org home"
                }) { Text("Shared home") }
                TextButton(onClick = { directoryPicker.launch(homeSelection.customTreeUri) }) {
                    Text("Custom dir")
                }
            }
        }

        if (repository == null) {
            Text(
                if (homeSelection.mode == OrgHomeMode.SHARED) {
                    "Shared Org home is the default. Install/open Org Sync or choose a custom directory."
                } else {
                    "The custom Org directory is unavailable."
                },
            )
            return@Column
        }

        Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
            Button(onClick = {
                runCatching { OrgReminderScheduler.reconcile(context) }
                    .onSuccess { result ->
                        status =
                            "${result.future} future · ${result.exact} exact · ${result.inexact} inexact · " +
                            "${result.cancelledStale} stale cancelled"
                        refresh()
                    }
                    .onFailure { status = it.message ?: "Reminder scheduling failed" }
            }) {
                Text("Reconcile alarms")
            }

            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
                TextButton(onClick = {
                    notificationPermission.launch(Manifest.permission.POST_NOTIFICATIONS)
                }) { Text("Notifications") }
            }

            if (
                Build.VERSION.SDK_INT >= Build.VERSION_CODES.S &&
                !OrgReminderScheduler.exactAccess(context)
            ) {
                TextButton(onClick = {
                    context.startActivity(
                        Intent(
                            Settings.ACTION_REQUEST_SCHEDULE_EXACT_ALARM,
                            Uri.parse("package:${context.packageName}"),
                        ),
                    )
                }) { Text("Exact times") }
            }
        }

        Text(
            if (OrgReminderScheduler.exactAccess(context)) {
                "Explicit Org times can use exact alarms. Untimed dates use the shared 09:00 default."
            } else {
                "Exact-alarm access is off. Reminders degrade to Android's inexact idle-safe alarms."
            },
            style = MaterialTheme.typography.bodySmall,
        )

        LazyColumn(
            modifier = Modifier.fillMaxSize(),
            verticalArrangement = Arrangement.spacedBy(6.dp),
        ) {
            items(reminders, key = { it.stableKey }) { reminder ->
                ReminderRow(reminder)
            }
        }
    }
}

@Composable
private fun ReminderRow(reminder: OrgReminderSpec) {
    Column(
        modifier = Modifier
            .fillMaxWidth()
            .background(MaterialTheme.colorScheme.surface)
            .padding(10.dp),
        verticalArrangement = Arrangement.spacedBy(2.dp),
    ) {
        Text(reminder.title)
        Text(
            "${reminder.kind.name.lowercase()} · ${reminder.whenLocal}" +
                if (reminder.explicitTime) " · exact intent" else " · default time",
            style = MaterialTheme.typography.labelSmall,
            color = MaterialTheme.colorScheme.secondary,
        )
        Text(
            "${reminder.taskPath}:${reminder.taskLine}",
            style = MaterialTheme.typography.labelSmall,
        )
    }
}
