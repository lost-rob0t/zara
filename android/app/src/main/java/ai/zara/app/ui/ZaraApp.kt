package ai.zara.app.ui

import ai.zara.app.runtime.EnrollmentReadiness
import ai.zara.app.runtime.RuntimeState
import ai.zara.app.runtime.ServerConnection
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.padding
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.NavigationBar
import androidx.compose.material3.NavigationBarItem
import androidx.compose.material3.Scaffold
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp

enum class AppSurface(val label: String) {
    Chat("Chat"),
    Connection("Connection"),
    Settings("Settings"),
    Diagnostics("Diagnostics"),
}

@Composable
fun ZaraApp(
    runtimeState: RuntimeState,
    sourceSha: String,
) {
    var selected by rememberSaveable { mutableStateOf(AppSurface.Chat) }
    MaterialTheme {
        Scaffold(
            bottomBar = {
                NavigationBar {
                    AppSurface.entries.forEach { surface ->
                        NavigationBarItem(
                            selected = selected == surface,
                            onClick = { selected = surface },
                            icon = {},
                            label = { Text(surface.label) },
                        )
                    }
                }
            },
        ) { padding ->
            when (selected) {
                AppSurface.Chat -> ChatSurface(runtimeState, padding)
                AppSurface.Connection -> ConnectionSurface(runtimeState, padding)
                AppSurface.Settings -> SettingsSurface(runtimeState, padding)
                AppSurface.Diagnostics -> DiagnosticsSurface(runtimeState, sourceSha, padding)
            }
        }
    }
}

@Composable
private fun ChatSurface(state: RuntimeState, padding: PaddingValues) {
    SurfaceColumn(padding) {
        Text("Zara", style = MaterialTheme.typography.headlineMedium)
        Text(connectionLabel(state.server))
        if (state.server is ServerConnection.Connected && state.enrollment == EnrollmentReadiness.Ready) {
            Text("Authenticated text session ready. Chat composer wiring is next.")
        } else {
            Text("Chat unavailable until an authenticated Zara session is connected.")
        }
    }
}

@Composable
private fun ConnectionSurface(state: RuntimeState, padding: PaddingValues) {
    SurfaceColumn(padding) {
        Text("Connection", style = MaterialTheme.typography.headlineMedium)
        Text("server: ${state.configuredProfile?.endpoint ?: "not configured"}")
        Text("state: ${connectionLabel(state.server)}")
        Text("enrollment: ${enrollmentLabel(state.enrollment)}")
        Text("session: ${state.sessionId ?: "none"}")
    }
}

@Composable
private fun SettingsSurface(state: RuntimeState, padding: PaddingValues) {
    SurfaceColumn(padding) {
        Text("Settings", style = MaterialTheme.typography.headlineMedium)
        Text("server endpoint: ${state.configuredProfile?.endpoint ?: "not configured"}")
        Text("Endpoint editing and enrollment controls are not wired yet.")
    }
}

@Composable
private fun DiagnosticsSurface(
    state: RuntimeState,
    sourceSha: String,
    padding: PaddingValues,
) {
    SurfaceColumn(padding) {
        Text("Diagnostics", style = MaterialTheme.typography.headlineMedium)
        Text("source: $sourceSha")
        Text("connection: ${connectionLabel(state.server)}")
        Text("generation: ${state.generation}")
        Text("session: ${state.sessionId ?: "none"}")
        Text("conversation: ${state.selectedConversationId ?: "none"}")
        Text("enrollment: ${enrollmentLabel(state.enrollment)}")
    }
}

@Composable
private fun SurfaceColumn(
    padding: PaddingValues,
    content: @Composable () -> Unit,
) {
    Column(
        modifier = Modifier
            .fillMaxSize()
            .padding(padding)
            .padding(20.dp),
        verticalArrangement = Arrangement.spacedBy(12.dp),
    ) {
        content()
    }
}

internal fun connectionLabel(connection: ServerConnection): String = when (connection) {
    ServerConnection.Disconnected -> "disconnected"
    is ServerConnection.Connecting -> "connecting"
    is ServerConnection.Connected -> "connected"
    is ServerConnection.Reconnecting -> "reconnecting (attempt ${connection.attempt})"
    is ServerConnection.OfflineDegraded -> "offline (${connection.reason})"
}

internal fun enrollmentLabel(readiness: EnrollmentReadiness): String = when (readiness) {
    EnrollmentReadiness.Unenrolled -> "unenrolled"
    EnrollmentReadiness.AwaitingServerPin -> "awaiting server pin"
    EnrollmentReadiness.Ready -> "ready"
    EnrollmentReadiness.Corrupt -> "corrupt"
}
