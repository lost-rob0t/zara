package ai.zara.wear.health

import ai.zara.ui.health.HealthCaptureMode
import ai.zara.ui.health.HealthGoalMetric
import ai.zara.ui.health.HealthGoalTarget
import ai.zara.ui.health.SamsungHealthSensorTracker
import ai.zara.ui.theme.ZaraTheme
import ai.zara.ui.theme.themeTokens
import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.Canvas
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.runtime.Composable
import androidx.compose.runtime.DisposableEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.StrokeCap
import androidx.compose.ui.graphics.drawscope.Stroke
import androidx.compose.ui.semantics.contentDescription
import androidx.compose.ui.semantics.semantics
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.wear.compose.material3.Button
import androidx.wear.compose.material3.Card
import androidx.wear.compose.material3.MaterialTheme
import androidx.wear.compose.material3.Text

@Composable
fun ZaraWearHealthSurface(
    controller: WearHealthController,
    goals: List<HealthGoalTarget>,
    onClose: () -> Unit,
) {
    val tokens = themeTokens(ZaraTheme.Outrun, systemDark = true, reducedGlow = false)
    var snapshot by remember { mutableStateOf(WearHealthSnapshot(WearHealthAvailability.CONNECTING)) }

    DisposableEffect(controller) {
        var disposed = false
        controller.refresh().thenAccept { if (!disposed) snapshot = it }
        onDispose { disposed = true }
    }

    MaterialTheme {
        Box(Modifier.fillMaxSize().background(tokens.background)) {
            Column(
                modifier = Modifier.fillMaxSize().verticalScroll(rememberScrollState())
                    .padding(horizontal = 18.dp, vertical = 16.dp),
                horizontalAlignment = Alignment.CenterHorizontally,
                verticalArrangement = Arrangement.spacedBy(8.dp),
            ) {
                Text(
                    "ZARA HEALTH",
                    color = tokens.accentCyan,
                    fontFamily = FontFamily.Monospace,
                    fontWeight = FontWeight.Bold,
                    fontSize = 15.sp,
                    letterSpacing = 1.5.sp,
                )
                Text(
                    availabilityText(snapshot.availability),
                    color = if (snapshot.availability == WearHealthAvailability.READY) tokens.success else tokens.textMuted,
                    fontSize = 10.sp,
                    textAlign = TextAlign.Center,
                )
                if (goals.isNotEmpty()) {
                    Text(
                        "GOALS",
                        modifier = Modifier.fillMaxWidth().padding(top = 4.dp),
                        color = tokens.accentMagenta,
                        fontFamily = FontFamily.Monospace,
                        fontSize = 9.sp,
                        letterSpacing = 1.sp,
                    )
                    Row(
                        modifier = Modifier.fillMaxWidth(),
                        horizontalArrangement = Arrangement.SpaceEvenly,
                    ) {
                        goals.filter { it.metric == HealthGoalMetric.STEPS || it.metric == HealthGoalMetric.SLEEP }
                            .forEach { goal -> GoalGauge(goal) }
                    }
                }
                if (snapshot.availability == WearHealthAvailability.READY) {
                    HealthCaptureMode.entries.forEach { mode ->
                        val trackers = snapshot.capabilities.available.filter { it.captureMode == mode }
                        if (trackers.isNotEmpty()) {
                            Text(
                                mode.label(),
                                modifier = Modifier.fillMaxWidth().padding(top = 4.dp),
                                color = tokens.accentMagenta,
                                fontFamily = FontFamily.Monospace,
                                fontSize = 9.sp,
                                letterSpacing = 1.sp,
                            )
                            trackers.forEach { tracker -> TrackerCard(tracker) }
                        }
                    }
                    if (snapshot.capabilities.updateRequired.isNotEmpty()) {
                        Text(
                            "Update watch software for ${snapshot.capabilities.updateRequired.joinToString { it.label }}.",
                            color = tokens.warning,
                            fontSize = 9.sp,
                            textAlign = TextAlign.Center,
                        )
                    }
                    Text(
                        "${snapshot.capabilities.unsupported.size} tracker types are not reported by this watch.",
                        color = tokens.textMuted,
                        fontSize = 9.sp,
                        textAlign = TextAlign.Center,
                    )
                }
                Button(onClick = onClose) { Text("BACK", fontFamily = FontFamily.Monospace, fontSize = 10.sp) }
            }
        }
    }
}

@Composable
private fun GoalGauge(goal: HealthGoalTarget) {
    val tokens = themeTokens(ZaraTheme.Outrun, systemDark = true, reducedGlow = false)
    Box(Modifier.size(74.dp), contentAlignment = Alignment.Center) {
        Canvas(Modifier.fillMaxSize().semantics {
            contentDescription = "${goal.metric.label} goal ${goal.target} ${goal.metric.unit}"
        }) {
            val stroke = Stroke(width = 6.dp.toPx(), cap = StrokeCap.Round)
            drawArc(
                color = tokens.border,
                startAngle = 135f,
                sweepAngle = 270f,
                useCenter = false,
                style = stroke,
            )
            drawArc(
                color = if (goal.metric == HealthGoalMetric.STEPS) tokens.accentCyan else tokens.accentMagenta,
                startAngle = 135f,
                sweepAngle = 54f,
                useCenter = false,
                style = stroke,
            )
        }
        Column(horizontalAlignment = Alignment.CenterHorizontally) {
            Text(
                if (goal.metric == HealthGoalMetric.SLEEP) "${goal.target / 60}H" else compact(goal.target),
                color = tokens.text,
                fontFamily = FontFamily.Monospace,
                fontWeight = FontWeight.Bold,
                fontSize = 11.sp,
            )
            Text(goal.metric.label.uppercase(), color = tokens.textMuted, fontSize = 7.sp)
            Text("TARGET", color = tokens.textMuted, fontSize = 6.sp)
        }
    }
}

private fun compact(value: Int): String = if (value >= 1_000) "${value / 1_000}K" else value.toString()

@Composable
private fun TrackerCard(tracker: SamsungHealthSensorTracker) {
    val tokens = themeTokens(ZaraTheme.Outrun, systemDark = true, reducedGlow = false)
    Card(
        onClick = {},
        enabled = false,
        border = BorderStroke(1.dp, tokens.border),
        modifier = Modifier.fillMaxWidth().semantics {
            contentDescription = "${tracker.label}. Available. Capture requires explicit permission and start."
        },
    ) {
        Row(
            modifier = Modifier.fillMaxWidth().padding(horizontal = 10.dp, vertical = 8.dp),
            horizontalArrangement = Arrangement.SpaceBetween,
            verticalAlignment = Alignment.CenterVertically,
        ) {
            Text(tracker.label, color = tokens.text, fontSize = 10.sp, modifier = Modifier.weight(1f))
            Text("AVAILABLE", color = tokens.success, fontFamily = FontFamily.Monospace, fontSize = 8.sp)
        }
    }
}

private fun availabilityText(value: WearHealthAvailability): String = when (value) {
    WearHealthAvailability.SDK_MISSING -> "Install a signed Zara Health watch build with Samsung Health Sensor SDK."
    WearHealthAvailability.CONNECTING -> "Checking this watch's sensors…"
    WearHealthAvailability.READY -> "Private on-watch capability map"
    WearHealthAvailability.AUTHORIZATION_REQUIRED -> "This package and signer are not authorized for sensor access."
    WearHealthAvailability.SERVICE_UNAVAILABLE -> "Samsung Health Sensor Service is unavailable."
    WearHealthAvailability.ERROR -> "Health sensors are temporarily unavailable."
}

private fun HealthCaptureMode.label(): String = when (this) {
    HealthCaptureMode.CONTINUOUS -> "CONTINUOUS"
    HealthCaptureMode.ON_DEMAND -> "ON DEMAND"
    HealthCaptureMode.EXERCISE -> "EXERCISE"
}
