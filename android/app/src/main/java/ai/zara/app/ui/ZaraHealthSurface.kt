package ai.zara.app.ui

import ai.zara.app.samsunghealth.SamsungHealthAvailability
import ai.zara.app.samsunghealth.SamsungHealthMetric
import ai.zara.app.samsunghealth.SamsungHealthUiState
import ai.zara.ui.health.HealthPrivacyClass
import ai.zara.ui.health.HealthSection
import ai.zara.ui.health.HealthGoalMetric
import ai.zara.ui.health.HealthGoalTarget
import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material3.Button
import androidx.compose.material3.ButtonDefaults
import androidx.compose.material3.LinearProgressIndicator
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.semantics.contentDescription
import androidx.compose.ui.semantics.semantics
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp

@Composable
internal fun ZaraHealthSurface(
    state: SamsungHealthUiState,
    goals: List<HealthGoalTarget>,
    gpgRecipientCount: Int,
    onRefresh: () -> Unit,
    onSetGoal: (HealthGoalTarget) -> Unit,
    onImportGpgKey: () -> Unit,
    onExportGpg: () -> Unit,
    onRequestPermission: (SamsungHealthMetric) -> Unit,
    onRead: (SamsungHealthMetric) -> Unit,
    padding: PaddingValues,
) {
    val tokens = LocalZaraTokens.current
    val sections = HealthSection.entries.filter { section ->
        SamsungHealthMetric.entries.any { it.section == section }
    }
    LazyColumn(
        modifier = Modifier.padding(padding),
        contentPadding = PaddingValues(horizontal = 16.dp, vertical = 14.dp),
        verticalArrangement = Arrangement.spacedBy(12.dp),
    ) {
        item {
            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.SpaceBetween,
                verticalAlignment = Alignment.CenterVertically,
            ) {
                Column {
                    Text(
                        "ZARA HEALTH",
                        color = tokens.accentCyan,
                        fontFamily = FontFamily.Monospace,
                        fontWeight = FontWeight.Bold,
                        fontSize = 18.sp,
                        letterSpacing = 1.8.sp,
                    )
                    Text(
                        "Private by default · fitness and wellness only",
                        color = tokens.textMuted,
                        fontSize = 12.sp,
                    )
                }
                Button(
                    onClick = onRefresh,
                    enabled = !state.refreshing && state.busyMetric == null,
                    colors = ButtonDefaults.buttonColors(containerColor = tokens.borderActive),
                ) {
                    Text(if (state.refreshing) "CHECKING" else "REFRESH")
                }
            }
        }
        item { HealthReadinessCard(state) }
        item {
            Column(verticalArrangement = Arrangement.spacedBy(8.dp)) {
                Text(
                    "GOALS",
                    color = tokens.accentMagenta,
                    fontFamily = FontFamily.Monospace,
                    fontWeight = FontWeight.SemiBold,
                    fontSize = 11.sp,
                    letterSpacing = 1.3.sp,
                )
                goals.forEach { goal ->
                    HealthGoalCard(goal, currentGoalValue(state, goal.metric), onSetGoal)
                }
            }
        }
        items(sections, key = { it.name }) { section ->
            Column(verticalArrangement = Arrangement.spacedBy(8.dp)) {
                Text(
                    section.label.uppercase(),
                    color = tokens.accentMagenta,
                    fontFamily = FontFamily.Monospace,
                    fontWeight = FontWeight.SemiBold,
                    fontSize = 11.sp,
                    letterSpacing = 1.3.sp,
                )
                SamsungHealthMetric.entries.filter { it.section == section }.forEach { metric ->
                    HealthMetricCard(
                        metric = metric,
                        state = state,
                        onRequestPermission = onRequestPermission,
                        onRead = onRead,
                    )
                }
            }
        }
        item {
            Column(verticalArrangement = Arrangement.spacedBy(8.dp)) {
                Text(
                    "Health data stays local unless you explicitly share an end-to-end OpenPGP export. Zara does not place readings in diagnostics, chat history, or remote model context.",
                    color = tokens.textMuted,
                    fontSize = 11.sp,
                    lineHeight = 16.sp,
                )
                Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                    HealthAction("ADD GPG KEY", onClick = onImportGpgKey)
                    HealthAction(
                        "SHARE GPG ($gpgRecipientCount)",
                        enabled = gpgRecipientCount > 0,
                        onClick = onExportGpg,
                    )
                }
            }
        }
    }
}

@Composable
private fun HealthGoalCard(
    goal: HealthGoalTarget,
    current: Int?,
    onSetGoal: (HealthGoalTarget) -> Unit,
) {
    val tokens = LocalZaraTokens.current
    val progress = current?.let(goal::progress)?.toFloat()?.coerceIn(0f, 1f) ?: 0f
    Surface(
        color = tokens.surfaceElevated,
        border = BorderStroke(1.dp, tokens.border),
        modifier = Modifier.fillMaxWidth().semantics {
            contentDescription = buildString {
                append("${goal.metric.label} goal ${goal.target} ${goal.metric.unit}.")
                current?.let { append(" Current $it.") }
            }
        },
    ) {
        Column(Modifier.padding(12.dp), verticalArrangement = Arrangement.spacedBy(8.dp)) {
            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.SpaceBetween,
                verticalAlignment = Alignment.CenterVertically,
            ) {
                Column {
                    Text(goal.metric.label, color = tokens.text, fontWeight = FontWeight.SemiBold)
                    Text(
                        goalValueLabel(goal.metric, goal.target),
                        color = tokens.accentCyan,
                        fontFamily = FontFamily.Monospace,
                        fontSize = 12.sp,
                    )
                }
                Row(horizontalArrangement = Arrangement.spacedBy(6.dp)) {
                    HealthAction("−", enabled = goal.target > goal.metric.minimum) {
                        onSetGoal(goal.copy(target = (goal.target - goal.metric.step).coerceAtLeast(goal.metric.minimum)))
                    }
                    HealthAction("+") {
                        onSetGoal(goal.copy(target = (goal.target + goal.metric.step).coerceAtMost(goal.metric.maximum)))
                    }
                }
            }
            LinearProgressIndicator(
                progress = { progress },
                modifier = Modifier.fillMaxWidth(),
                color = tokens.accentCyan,
                trackColor = tokens.border,
            )
            Text(
                current?.let { "${goalValueLabel(goal.metric, it)} today" } ?: "Read ${goal.metric.label.lowercase()} to show progress",
                color = tokens.textMuted,
                fontSize = 10.sp,
            )
        }
    }
}

private fun currentGoalValue(state: SamsungHealthUiState, metric: HealthGoalMetric): Int? {
    val readingMetric = when (metric) {
        HealthGoalMetric.STEPS -> SamsungHealthMetric.STEPS
        HealthGoalMetric.SLEEP -> SamsungHealthMetric.SLEEP
        HealthGoalMetric.ACTIVE_CALORIES -> null
        HealthGoalMetric.ACTIVE_TIME -> null
        HealthGoalMetric.NUTRITION -> SamsungHealthMetric.NUTRITION
        HealthGoalMetric.WATER -> SamsungHealthMetric.WATER_INTAKE
    } ?: return null
    val values = state.readings[readingMetric]?.values ?: return null
    val preferred = when (metric) {
        HealthGoalMetric.STEPS -> listOf("steps", "count")
        HealthGoalMetric.SLEEP, HealthGoalMetric.ACTIVE_TIME -> listOf("minutes", "duration_minutes")
        HealthGoalMetric.ACTIVE_CALORIES, HealthGoalMetric.NUTRITION -> listOf("kcal", "calories")
        HealthGoalMetric.WATER -> listOf("ml", "milliliters")
    }
    return preferred.firstNotNullOfOrNull { key -> values[key]?.toDoubleOrNull()?.toInt() }
}

private fun goalValueLabel(metric: HealthGoalMetric, value: Int): String =
    if (metric == HealthGoalMetric.SLEEP) {
        "${value / 60}h ${value % 60}m"
    } else {
        "$value ${metric.unit}"
    }

@Composable
private fun HealthReadinessCard(state: SamsungHealthUiState) {
    val tokens = LocalZaraTokens.current
    val (title, detail) = when (state.availability) {
        null -> "Not checked" to "Refresh to inspect the local Samsung Health connection."
        SamsungHealthAvailability.READY -> "Samsung Health ready" to
            "${state.grantedMetrics.size} of ${state.supportedMetrics.size} Zara-supported permissions granted."
        SamsungHealthAvailability.SDK_MISSING -> "Health SDK not bundled" to
            "Install a signed Zara Health build containing the official Samsung Health Data SDK."
        SamsungHealthAvailability.PLATFORM_NOT_INSTALLED -> "Samsung Health missing" to "Install Samsung Health on this phone."
        SamsungHealthAvailability.PLATFORM_TOO_OLD -> "Samsung Health update required" to "Update Samsung Health to 6.30.2 or newer."
        SamsungHealthAvailability.PLATFORM_DISABLED -> "Samsung Health disabled" to "Enable Samsung Health before reading data."
        SamsungHealthAvailability.PLATFORM_NOT_INITIALIZED -> "Samsung Health setup incomplete" to "Finish setup in Samsung Health."
        SamsungHealthAvailability.AUTHORIZATION_REQUIRED -> "Build authorization required" to
            "This package and signing identity are not registered for Samsung Health access."
        SamsungHealthAvailability.ERROR -> "Health temporarily unavailable" to "Retry after checking Samsung Health."
    }
    Surface(
        color = tokens.surfaceElevated,
        border = BorderStroke(1.dp, if (state.ready) tokens.success else tokens.border),
        modifier = Modifier.fillMaxWidth().semantics { contentDescription = "$title. $detail" },
    ) {
        Column(Modifier.padding(14.dp), verticalArrangement = Arrangement.spacedBy(4.dp)) {
            Text(title, color = tokens.text, fontWeight = FontWeight.SemiBold)
            Text(state.error ?: detail, color = if (state.error == null) tokens.textMuted else tokens.error, fontSize = 12.sp)
        }
    }
}

@Composable
private fun HealthMetricCard(
    metric: SamsungHealthMetric,
    state: SamsungHealthUiState,
    onRequestPermission: (SamsungHealthMetric) -> Unit,
    onRead: (SamsungHealthMetric) -> Unit,
) {
    val tokens = LocalZaraTokens.current
    val supported = metric in state.supportedMetrics
    val granted = metric in state.grantedMetrics
    val reading = state.readings[metric]
    val busy = state.busyMetric == metric
    val status = when {
        !state.ready -> "Health connection required"
        !supported -> "Not exposed by this Zara Health SDK adapter"
        !granted -> "Permission required"
        reading == null -> "Ready to read locally"
        reading.values.isEmpty() -> "No data today"
        else -> reading.values.entries.joinToString(" · ") { (name, value) -> "${name.replace('_', ' ')} $value" }
    }
    Surface(
        color = tokens.surface,
        border = BorderStroke(1.dp, if (reading != null) tokens.borderActive else tokens.border),
        modifier = Modifier.fillMaxWidth().semantics {
            contentDescription = "${metric.label}. $status. ${metric.privacy.accessibilityLabel()}"
        },
    ) {
        Row(
            modifier = Modifier.padding(horizontal = 14.dp, vertical = 12.dp),
            horizontalArrangement = Arrangement.spacedBy(12.dp),
            verticalAlignment = Alignment.CenterVertically,
        ) {
            Column(Modifier.weight(1f), verticalArrangement = Arrangement.spacedBy(3.dp)) {
                Text(metric.label, color = tokens.text, fontWeight = FontWeight.SemiBold)
                Text(status, color = tokens.textMuted, fontSize = 11.sp, lineHeight = 15.sp)
            }
            when {
                state.ready && supported && !granted -> HealthAction("ALLOW") { onRequestPermission(metric) }
                state.ready && supported -> HealthAction(if (busy) "READING" else "READ", enabled = !busy) { onRead(metric) }
            }
        }
    }
}

@Composable
private fun HealthAction(label: String, enabled: Boolean = true, onClick: () -> Unit) {
    val tokens = LocalZaraTokens.current
    Button(
        onClick = onClick,
        enabled = enabled,
        colors = ButtonDefaults.buttonColors(containerColor = tokens.borderActive),
    ) {
        Text(label, fontFamily = FontFamily.Monospace, fontSize = 10.sp)
    }
}

private fun HealthPrivacyClass.accessibilityLabel(): String = when (this) {
    HealthPrivacyClass.WELLNESS -> "Wellness data"
    HealthPrivacyClass.BIOMETRIC -> "Sensitive biometric data"
    HealthPrivacyClass.PROFILE -> "Sensitive profile data"
    HealthPrivacyClass.RAW_BIOSIGNAL -> "Highly sensitive raw biosignal"
}
