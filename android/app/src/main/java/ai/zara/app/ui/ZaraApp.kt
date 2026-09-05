package ai.zara.app.ui

import ai.zara.app.runtime.AssistantRole
import ai.zara.app.runtime.EnrollmentReadiness
import ai.zara.app.runtime.RuntimeState
import ai.zara.app.runtime.ServerConnection
import ai.zara.app.voice.ManualVoiceState
import ai.zara.app.voice.VoiceStreamState
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.material3.Button
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.NavigationBar
import androidx.compose.material3.NavigationBarItem
import androidx.compose.material3.OutlinedTextField
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
    Voice("Voice"),
    Connection("Connection"),
    Settings("Settings"),
    Diagnostics("Diagnostics"),
}

data class RenderedTextTurn(
    val userText: String,
    val assistantText: String,
    val success: Boolean,
)

@Composable
fun ZaraApp(
    runtimeState: RuntimeState,
    sourceSha: String,
    enrollmentPublicKey: String?,
    lastTurn: RenderedTextTurn?,
    operationError: String?,
    operationBusy: Boolean,
    microphonePermissionGranted: Boolean,
    voiceState: ManualVoiceState,
    voiceStreamState: VoiceStreamState?,
    voiceStreamFailure: String?,
    onCreateIdentity: () -> Unit,
    onPinServer: (String) -> Unit,
    onConnect: (String) -> Unit,
    onSendText: (String) -> Unit,
    onRequestMicrophonePermission: () -> Unit,
    onRequestAssistantRole: () -> Unit,
    onStartVoice: () -> Unit,
    onStopVoice: () -> Unit,
    onCancelVoice: () -> Unit,
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
                AppSurface.Chat -> ChatSurface(
                    state = runtimeState,
                    lastTurn = lastTurn,
                    operationError = operationError,
                    operationBusy = operationBusy,
                    onSendText = onSendText,
                    padding = padding,
                )
                AppSurface.Voice -> VoiceSurface(
                    state = runtimeState,
                    microphonePermissionGranted = microphonePermissionGranted,
                    voiceState = voiceState,
                    voiceStreamState = voiceStreamState,
                    voiceStreamFailure = voiceStreamFailure,
                    operationError = operationError,
                    operationBusy = operationBusy,
                    onRequestMicrophonePermission = onRequestMicrophonePermission,
                    onStartVoice = onStartVoice,
                    onStopVoice = onStopVoice,
                    onCancelVoice = onCancelVoice,
                    padding = padding,
                )
                AppSurface.Connection -> ConnectionSurface(
                    state = runtimeState,
                    operationError = operationError,
                    operationBusy = operationBusy,
                    onConnect = onConnect,
                    padding = padding,
                )
                AppSurface.Settings -> SettingsSurface(
                    state = runtimeState,
                    enrollmentPublicKey = enrollmentPublicKey,
                    operationError = operationError,
                    operationBusy = operationBusy,
                    onCreateIdentity = onCreateIdentity,
                    onPinServer = onPinServer,
                    onRequestAssistantRole = onRequestAssistantRole,
                    padding = padding,
                )
                AppSurface.Diagnostics -> DiagnosticsSurface(
                    state = runtimeState,
                    sourceSha = sourceSha,
                    voiceStreamState = voiceStreamState,
                    voiceStreamFailure = voiceStreamFailure,
                    operationError = operationError,
                    padding = padding,
                )
            }
        }
    }
}

@Composable
private fun ChatSurface(
    state: RuntimeState,
    lastTurn: RenderedTextTurn?,
    operationError: String?,
    operationBusy: Boolean,
    onSendText: (String) -> Unit,
    padding: PaddingValues,
) {
    var input by rememberSaveable { mutableStateOf("") }
    val ready = state.server is ServerConnection.Connected &&
        state.enrollment == EnrollmentReadiness.Ready
    SurfaceColumn(padding) {
        Text("Zara", style = MaterialTheme.typography.headlineMedium)
        Text(connectionLabel(state.server))
        lastTurn?.let { turn ->
            Text("You: ${turn.userText}")
            Text(if (turn.success) "Zara: ${turn.assistantText}" else "Zara error: ${turn.assistantText}")
        }
        operationError?.let { Text("Error: $it") }
        if (ready) {
            OutlinedTextField(
                value = input,
                onValueChange = { input = it },
                modifier = Modifier.fillMaxWidth(),
                label = { Text("Message") },
                enabled = !operationBusy,
            )
            Button(
                onClick = {
                    val message = input.trim()
                    if (message.isNotEmpty()) {
                        input = ""
                        onSendText(message)
                    }
                },
                enabled = input.isNotBlank() && !operationBusy,
            ) {
                Text(if (operationBusy) "Working…" else "Send")
            }
        } else {
            Text("Chat unavailable until an authenticated Zara session is connected.")
        }
    }
}

@Composable
private fun VoiceSurface(
    state: RuntimeState,
    microphonePermissionGranted: Boolean,
    voiceState: ManualVoiceState,
    voiceStreamState: VoiceStreamState?,
    voiceStreamFailure: String?,
    operationError: String?,
    operationBusy: Boolean,
    onRequestMicrophonePermission: () -> Unit,
    onStartVoice: () -> Unit,
    onStopVoice: () -> Unit,
    onCancelVoice: () -> Unit,
    padding: PaddingValues,
) {
    val capturing = voiceState is ManualVoiceState.Capturing
    SurfaceColumn(padding) {
        Text("Voice", style = MaterialTheme.typography.headlineMedium)
        Text("connection: ${connectionLabel(state.server)}")
        Text(if (capturing) "microphone: streaming to Zara" else "microphone: idle")
        Text("Manual voice uses the same authenticated Zara session as Chat. The server owns STT, assistant routing, tools, and TTS.")
        voiceStreamState?.let { stream ->
            if (stream.transcriptStreamId != null) {
                val label = if (stream.transcriptFinal) "transcript" else "transcript (live)"
                Text("$label: ${stream.transcriptText}")
            }
            val audio = stream.audio
            Text(
                if (audio == null) {
                    "speaker: idle"
                } else {
                    "speaker: Zara audio ${audio.sampleRate} Hz mono"
                }
            )
        }
        voiceStreamFailure?.let { Text("Voice stream error: $it") }
        if (!microphonePermissionGranted) {
            Text("Microphone permission is required before any audio stream can open.")
            Button(
                onClick = onRequestMicrophonePermission,
                enabled = !operationBusy && !capturing,
            ) {
                Text("Grant microphone permission")
            }
        } else if (!canStartManualVoice(state, microphonePermissionGranted) && !capturing) {
            Text("Voice unavailable until an authenticated Zara session is connected.")
        } else if (capturing) {
            Button(onClick = onStopVoice, enabled = !operationBusy) {
                Text("Stop & send")
            }
            Button(onClick = onCancelVoice, enabled = !operationBusy) {
                Text("Cancel")
            }
        } else {
            Button(onClick = onStartVoice, enabled = !operationBusy) {
                Text("Start talking")
            }
        }
        operationError?.let { Text("Error: $it") }
    }
}

@Composable
private fun ConnectionSurface(
    state: RuntimeState,
    operationError: String?,
    operationBusy: Boolean,
    onConnect: (String) -> Unit,
    padding: PaddingValues,
) {
    var endpoint by rememberSaveable {
        mutableStateOf(state.configuredProfile?.endpoint.orEmpty())
    }
    SurfaceColumn(padding) {
        Text("Connection", style = MaterialTheme.typography.headlineMedium)
        Text("state: ${connectionLabel(state.server)}")
        Text("enrollment: ${enrollmentLabel(state.enrollment)}")
        Text("session: ${state.sessionId ?: "none"}")
        OutlinedTextField(
            value = endpoint,
            onValueChange = { endpoint = it },
            modifier = Modifier.fillMaxWidth(),
            label = { Text("Zara server endpoint") },
            supportingText = { Text("tcp://host:port") },
            enabled = !operationBusy && canRequestConnect(state.server),
            singleLine = true,
        )
        Button(
            onClick = { onConnect(endpoint) },
            enabled = endpoint.isNotBlank() &&
                state.enrollment == EnrollmentReadiness.Ready &&
                canRequestConnect(state.server) &&
                !operationBusy,
        ) {
            Text("Connect")
        }
        operationError?.let { Text("Error: $it") }
    }
}

@Composable
private fun SettingsSurface(
    state: RuntimeState,
    enrollmentPublicKey: String?,
    operationError: String?,
    operationBusy: Boolean,
    onCreateIdentity: () -> Unit,
    onPinServer: (String) -> Unit,
    onRequestAssistantRole: () -> Unit,
    padding: PaddingValues,
) {
    var serverPin by rememberSaveable { mutableStateOf("") }
    SurfaceColumn(padding) {
        Text("Settings", style = MaterialTheme.typography.headlineMedium)
        Text("server endpoint: ${state.configuredProfile?.endpoint ?: "not configured"}")
        Text("enrollment: ${enrollmentLabel(state.enrollment)}")
        Text("assistant role: ${assistantRoleLabel(state.assistantRole)}")
        when (state.assistantRole) {
            AssistantRole.NotYetAssessed -> Text("Checking whether this Android device supports the Assistant role.")
            AssistantRole.Held -> Text("Zara is the current Android Assistant.")
            AssistantRole.NotHeld -> {
                Text("Zara can only become the Assistant after you approve Android's system role prompt.")
                Text(samsungAssistantSetupGuidance())
                Button(
                    onClick = onRequestAssistantRole,
                    enabled = !operationBusy && canRequestAssistantRole(state.assistantRole),
                ) {
                    Text("Make Zara assistant")
                }
            }
            AssistantRole.PlatformUnavailable -> Text("This Android configuration does not expose the public Assistant role to Zara.")
        }
        enrollmentPublicKey?.let {
            Text("client public key: $it")
            Text("Enroll this public key on the Zara server before connecting. The private key stays in Android Keystore-backed storage.")
        }
        when (state.enrollment) {
            EnrollmentReadiness.Unenrolled -> Button(
                onClick = onCreateIdentity,
                enabled = !operationBusy,
            ) {
                Text("Create client identity")
            }
            EnrollmentReadiness.AwaitingServerPin -> {
                Text("After the server owner enrolls the client key, pin that server's CURVE public key here. Pinning does not enroll the client on the server.")
                OutlinedTextField(
                    value = serverPin,
                    onValueChange = { serverPin = it },
                    modifier = Modifier.fillMaxWidth(),
                    label = { Text("Server CURVE public key") },
                    enabled = !operationBusy,
                    singleLine = true,
                )
                Button(
                    onClick = { onPinServer(serverPin) },
                    enabled = serverPin.isNotBlank() && !operationBusy,
                ) {
                    Text("Pin server key")
                }
            }
            EnrollmentReadiness.Ready -> Text("Client identity and server pin are ready. Server-side enrollment is still required for authentication.")
            EnrollmentReadiness.Corrupt -> Text("Enrollment storage is corrupt; connection is disabled.")
        }
        operationError?.let { Text("Error: $it") }
    }
}

@Composable
private fun DiagnosticsSurface(
    state: RuntimeState,
    sourceSha: String,
    voiceStreamState: VoiceStreamState?,
    voiceStreamFailure: String?,
    operationError: String?,
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
        Text("assistant role: ${assistantRoleLabel(state.assistantRole)}")
        voiceStreamState?.let { stream ->
            Text("voice session: ${stream.sessionId}")
            Text("voice transcript stream: ${stream.transcriptStreamId ?: "none"}")
            Text("voice output stream: ${stream.audio?.streamId ?: "none"}")
        }
        voiceStreamFailure?.let { Text("voice stream error: $it") }
        operationError?.let { Text("last error: $it") }
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

internal fun canRequestConnect(connection: ServerConnection): Boolean =
    connection is ServerConnection.Disconnected || connection is ServerConnection.OfflineDegraded

internal fun canRequestAssistantRole(role: AssistantRole): Boolean = role is AssistantRole.NotHeld

internal fun canStartManualVoice(
    state: RuntimeState,
    microphonePermissionGranted: Boolean,
): Boolean =
    microphonePermissionGranted &&
        state.enrollment == EnrollmentReadiness.Ready &&
        state.server is ServerConnection.Connected &&
        state.sessionId != null

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

internal fun assistantRoleLabel(role: AssistantRole): String = when (role) {
    AssistantRole.NotYetAssessed -> "not assessed"
    AssistantRole.Held -> "held"
    AssistantRole.NotHeld -> "not held"
    AssistantRole.PlatformUnavailable -> "platform unavailable"
}
