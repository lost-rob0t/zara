package ai.zara.app.ui

import ai.zara.app.localai.LocalAiState
import ai.zara.app.localai.LocalModelSpec
import ai.zara.app.runtime.RuntimeMode
import ai.zara.app.runtime.RuntimeState
import ai.zara.app.update.UpdateState
import ai.zara.ui.theme.ZaraTheme
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp

@Composable
internal fun SettingsOverviewContent(
    runtimeMode: RuntimeMode,
    localAiState: LocalAiState,
    localModels: List<LocalModelSpec>,
    runtimeState: RuntimeState,
    microphonePermissionGranted: Boolean,
    selectedTheme: ZaraTheme,
    updateState: UpdateState,
    onNavigate: (AppRoute) -> Unit,
) {
    val modelSummary = localAiState.model?.let { "${it.id}@${it.version}" }
        ?: if (localModels.isEmpty()) "no model installed" else "${localModels.size} installed"
    SectionCard("INTELLIGENCE") {
        SettingsOverviewRow(
            title = "Runtime & local AI",
            summary = "${runtimeMode.name} · $modelSummary",
            onClick = { onNavigate(AppRoute.Runtime) },
        )
    }
    SectionCard("CONNECTIVITY") {
        SettingsOverviewRow(
            title = "Connection",
            summary = connectionLabel(runtimeState.server),
            onClick = { onNavigate(AppRoute.Connection) },
        )
    }
    SectionCard("DEVICE") {
        SettingsOverviewRow(
            title = "Permissions",
            summary = if (microphonePermissionGranted) "Microphone granted" else "Microphone required",
            onClick = { onNavigate(AppRoute.Permissions) },
        )
        SettingsOverviewRow(
            title = "Appearance",
            summary = selectedTheme.name,
            onClick = { onNavigate(AppRoute.Appearance) },
        )
    }
    SectionCard("EXTENSIONS") {
        SettingsOverviewRow(
            title = "Plugins",
            summary = "Capabilities and integrations",
            onClick = { onNavigate(AppRoute.Plugins) },
        )
    }
    SectionCard("SYSTEM") {
        SettingsOverviewRow(
            title = "Updates",
            summary = updateState.phase.name.lowercase(),
            onClick = { onNavigate(AppRoute.Updates) },
        )
        SettingsOverviewRow(
            title = "Diagnostics",
            summary = "Runtime state and safe export",
            onClick = { onNavigate(AppRoute.Diagnostics) },
        )
        SettingsOverviewRow(
            title = "About",
            summary = "Build and version information",
            onClick = { onNavigate(AppRoute.About) },
        )
    }
}

@Composable
private fun SettingsOverviewRow(
    title: String,
    summary: String,
    onClick: () -> Unit,
) {
    val tokens = LocalZaraTokens.current
    Surface(
        modifier = Modifier.fillMaxWidth().clickable(onClick = onClick),
        color = tokens.surfaceInput,
        shape = MaterialTheme.shapes.medium,
    ) {
        Row(
            modifier = Modifier.fillMaxWidth().padding(horizontal = 14.dp, vertical = 12.dp),
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.spacedBy(12.dp),
        ) {
            Column(modifier = Modifier.weight(1f)) {
                Text(
                    title,
                    color = tokens.text,
                    style = MaterialTheme.typography.bodyLarge,
                )
                Text(
                    summary,
                    color = tokens.textMuted,
                    style = MaterialTheme.typography.bodySmall,
                )
            }
            Text(
                "›",
                color = tokens.accentCyan,
                style = MaterialTheme.typography.titleLarge,
            )
        }
    }
}
