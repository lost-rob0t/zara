package ai.zara.app.ui

import ai.zara.app.runtime.RuntimeHealth
import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.selection.selectable
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.semantics.contentDescription
import androidx.compose.ui.semantics.semantics
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.unit.dp

internal fun runtimeSettingsSelectionTarget(row: RuntimeSettingsRuntimeRow): String = row.runtimeId

internal fun runtimeSettingsEvidenceText(row: RuntimeSettingsRuntimeRow): String = buildString {
    append(row.displayName)
    append(" · id ")
    append(row.runtimeId)
    append(" · runtime ")
    append(row.runtimeVersion)
    append(" · implementation ")
    append(row.implementationVersion)
    append(" · health ")
    append(row.health.wire)
    append(" · locality ")
    append(row.locality.wire)
    append(" · profiles ")
    append(if (row.profiles.isEmpty()) "none" else row.profiles.joinToString(","))
    append(" · selectable ")
    append(row.selectable)
    append(" · selected ")
    append(row.selected)
}

@Composable
internal fun RuntimeSettingsRuntimeList(
    rows: List<RuntimeSettingsRuntimeRow>,
    onSelectRuntime: (String) -> Unit,
    modifier: Modifier = Modifier,
) {
    val tokens = LocalZaraTokens.current

    Column(modifier = modifier.fillMaxWidth()) {
        Text(
            "INSTALLED RUNTIMES",
            color = tokens.accentCyan,
            style = MaterialTheme.typography.labelSmall,
        )

        if (rows.isEmpty()) {
            Text(
                "No installed runtimes discovered.",
                modifier = Modifier.padding(vertical = 10.dp),
                color = tokens.textMuted,
                style = MaterialTheme.typography.bodySmall,
            )
            return@Column
        }

        rows.forEach { row ->
            val accent = when (row.health) {
                RuntimeHealth.READY -> tokens.success
                RuntimeHealth.STARTING, RuntimeHealth.BUSY, RuntimeHealth.DEGRADED -> tokens.warning
                RuntimeHealth.FAILED -> tokens.error
                RuntimeHealth.STOPPED -> tokens.textMuted
            }
            Surface(
                color = tokens.surface,
                border = BorderStroke(1.dp, if (row.selected) accent else tokens.border),
                shape = MaterialTheme.shapes.medium,
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(top = 8.dp)
                    .semantics { contentDescription = runtimeSettingsEvidenceText(row) }
                    .selectable(
                        selected = row.selected,
                        enabled = row.selectable,
                        onClick = { onSelectRuntime(runtimeSettingsSelectionTarget(row)) },
                    ),
            ) {
                Row(
                    modifier = Modifier.padding(horizontal = 12.dp, vertical = 10.dp),
                    verticalAlignment = Alignment.CenterVertically,
                ) {
                    Text(
                        if (row.selected) "●" else "○",
                        color = accent,
                        fontFamily = FontFamily.Monospace,
                    )
                    Column(Modifier.padding(start = 10.dp)) {
                        Text(
                            row.displayName,
                            color = if (row.selectable) tokens.text else tokens.textMuted,
                            style = MaterialTheme.typography.bodyMedium,
                        )
                        Text(
                            "${row.runtimeId} · runtime ${row.runtimeVersion} · implementation ${row.implementationVersion}",
                            color = tokens.textMuted,
                            style = MaterialTheme.typography.labelSmall,
                            fontFamily = FontFamily.Monospace,
                        )
                        Text(
                            buildString {
                                append(row.health.wire)
                                append(" · ")
                                append(row.locality.wire)
                                if (row.profiles.isNotEmpty()) {
                                    append(" · profiles ")
                                    append(row.profiles.joinToString(", "))
                                }
                            },
                            color = accent,
                            style = MaterialTheme.typography.labelSmall,
                            fontFamily = FontFamily.Monospace,
                        )
                    }
                }
            }
        }
    }
}
