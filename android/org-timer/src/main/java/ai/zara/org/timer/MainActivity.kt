package ai.zara.org.timer

import ai.zara.org.core.OrgTimerTemplate
import ai.zara.org.core.OrgTimers
import ai.zara.org.storage.OrgHome
import ai.zara.org.storage.OrgHomeMode
import android.Manifest
import android.os.Build
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
import java.time.Duration

private val TimerScheme = darkColorScheme(
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
            MaterialTheme(colorScheme = TimerScheme) {
                OrgTimerApp()
            }
        }
    }
}

@Composable
private fun OrgTimerApp() {
    val context = LocalContext.current
    var homeRevision by rememberSaveable { mutableStateOf(0) }
    val homeSelection = remember(homeRevision) { OrgHome.selection(context) }
    val repository = remember(homeRevision) { runCatching { OrgHome.open(context) }.getOrNull() }
    var templates by remember { mutableStateOf(emptyList<OrgTimerTemplate>()) }
    var running by remember { mutableStateOf(emptyList<RunningOrgTimer>()) }
    var status by remember { mutableStateOf("") }

    fun refresh() {
        templates = repository?.let { OrgTimers.fromTasks(it.allTasks()) }.orEmpty()
        running = OrgTimerRuntime.active(context)
        status = "${templates.size} templates · ${running.size} active"
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
                Text("Org Timer", style = MaterialTheme.typography.titleLarge)
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

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
            TextButton(onClick = {
                notificationPermission.launch(Manifest.permission.POST_NOTIFICATIONS)
            }) {
                Text("Enable notifications")
            }
        }

        Text(
            if (OrgTimerRuntime.exactAccess(context)) {
                "Timer finishes use exact alarms; no permanent ticking service runs."
            } else {
                "Exact alarm access is unavailable; timer finish delivery degrades to an inexact idle-safe alarm."
            },
            style = MaterialTheme.typography.bodySmall,
        )

        if (repository == null) {
            Text(
                if (homeSelection.mode == OrgHomeMode.SHARED) {
                    "Shared Org home is unavailable. Install/open Org Sync or choose a custom directory."
                } else {
                    "The custom Org directory is unavailable."
                },
            )
            return@Column
        }

        Text("Active timers", style = MaterialTheme.typography.titleMedium)
        if (running.isEmpty()) {
            Text("No active timers.")
        } else {
            running.forEach { timer ->
                RunningTimerRow(
                    timer = timer,
                    onPause = {
                        OrgTimerRuntime.pause(context, timer.id)
                        refresh()
                    },
                    onResume = {
                        OrgTimerRuntime.resume(context, timer.id)
                        refresh()
                    },
                    onCancel = {
                        OrgTimerRuntime.cancel(context, timer.id)
                        refresh()
                    },
                )
            }
        }

        Text("Org templates", style = MaterialTheme.typography.titleMedium)
        Text(
            "Add :timer: to an Org task and set :Effort: H:MM. The Org file remains canonical.",
            style = MaterialTheme.typography.bodySmall,
        )

        LazyColumn(
            modifier = Modifier.fillMaxSize(),
            verticalArrangement = Arrangement.spacedBy(6.dp),
        ) {
            items(templates, key = { it.stableKey }) { template ->
                TimerTemplateRow(
                    template = template,
                    onStart = {
                        OrgTimerRuntime.start(context, template)
                        refresh()
                    },
                )
            }
        }
    }
}

@Composable
private fun TimerTemplateRow(
    template: OrgTimerTemplate,
    onStart: () -> Unit,
) {
    Row(
        modifier = Modifier
            .fillMaxWidth()
            .background(MaterialTheme.colorScheme.surface)
            .padding(10.dp),
        horizontalArrangement = Arrangement.SpaceBetween,
    ) {
        Column {
            Text(template.name)
            Text(
                formatDuration(template.duration),
                style = MaterialTheme.typography.labelSmall,
                color = MaterialTheme.colorScheme.secondary,
            )
        }
        Button(onClick = onStart) { Text("Start") }
    }
}

@Composable
private fun RunningTimerRow(
    timer: RunningOrgTimer,
    onPause: () -> Unit,
    onResume: () -> Unit,
    onCancel: () -> Unit,
) {
    Row(
        modifier = Modifier
            .fillMaxWidth()
            .background(MaterialTheme.colorScheme.surface)
            .padding(10.dp),
        horizontalArrangement = Arrangement.SpaceBetween,
    ) {
        Column {
            Text(timer.name)
            Text(
                if (timer.paused) {
                    "Paused · ${formatMillis(timer.remainingMs)} remaining"
                } else {
                    "Running · finishes ${java.time.Instant.ofEpochMilli(timer.finishEpochMs)}"
                },
                style = MaterialTheme.typography.labelSmall,
            )
        }
        Row(horizontalArrangement = Arrangement.spacedBy(4.dp)) {
            TextButton(onClick = if (timer.paused) onResume else onPause) {
                Text(if (timer.paused) "Resume" else "Pause")
            }
            TextButton(onClick = onCancel) { Text("Cancel") }
        }
    }
}

private fun formatDuration(duration: Duration): String = formatMillis(duration.toMillis())

private fun formatMillis(ms: Long): String {
    val totalMinutes = (ms.coerceAtLeast(0L) + 59_999L) / 60_000L
    val hours = totalMinutes / 60L
    val minutes = totalMinutes % 60L
    return if (hours > 0) "${hours}h ${minutes}m" else "${minutes}m"
}
