package ai.zara.org.surfaces

import ai.zara.org.core.OrgTimers
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
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import kotlinx.coroutines.delay

@Composable
fun TimersSurface(model: OrgWorkspaceModel) {
    val tokens = LocalOrgTokens.current
    val templates = remember(model.projection) { OrgTimers.fromProjection(model.projection) }
    var runs by remember(templates) {
        mutableStateOf(templates.map { template -> OrgTimerRun(template.stableKey, template.name, template.duration) })
    }

    LaunchedEffect(runs) {
        while (runs.any { it.running }) {
            delay(1000)
            runs = runs.map { it.tick() }
        }
    }

    LazyColumn(verticalArrangement = Arrangement.spacedBy(6.dp)) {
        item("summary") {
            Text(
                if (templates.isEmpty()) {
                    "No timers yet. Tag an open task with :timer: and an EFFORT like 0:25 to derive one from the canonical corpus."
                } else {
                    "${templates.size} timers derived from :timer:-tagged open tasks"
                },
                color = MaterialTheme.colorScheme.onSurfaceVariant,
            )
        }
        items(runs, key = { it.key }) { run ->
            OrgPanel(active = run.running) {
                Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                    OrgStatusDot(
                        color = when {
                            run.running -> tokens.accentCyan
                            run.finished -> tokens.success
                            else -> tokens.borderActive
                        },
                    )
                    Text(
                        OrgTimerFormat.format(run.remaining),
                        style = MaterialTheme.typography.headlineSmall,
                    )
                }
                Text(
                    run.name,
                    color = if (run.finished) tokens.success else MaterialTheme.colorScheme.onSurface,
                )
                Row(horizontalArrangement = Arrangement.spacedBy(4.dp)) {
                    if (run.running) {
                        TextButton(onClick = { runs = runs.map { if (it.key == run.key) it.pause() else it } }) {
                            Text("Pause")
                        }
                    } else {
                        TextButton(
                            onClick = { runs = runs.map { if (it.key == run.key) it.start() else it } },
                            enabled = !run.finished,
                        ) {
                            Text(if (run.finished) "Done" else "Start")
                        }
                    }
                    TextButton(onClick = { runs = runs.map { if (it.key == run.key) it.reset() else it } }) {
                        Text("Reset")
                    }
                }
            }
        }
    }
}
