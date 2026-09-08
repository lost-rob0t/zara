package ai.zara.app.ui

import ai.zara.app.runtime.AssistantRole
import ai.zara.app.runtime.EnrollmentReadiness
import ai.zara.app.runtime.RuntimeState
import ai.zara.app.runtime.ServerConnection
import ai.zara.app.voice.ManualVoiceState
import ai.zara.app.voice.VoiceStreamState
import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.Canvas
import androidx.compose.foundation.background
import androidx.compose.foundation.isSystemInDarkTheme
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.WindowInsets
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.heightIn
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.statusBars
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.layout.windowInsetsPadding
import androidx.compose.foundation.layout.widthIn
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.selection.selectable
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.foundation.verticalScroll
import androidx.compose.material3.Button
import androidx.compose.material3.ButtonDefaults
import androidx.compose.material3.DrawerValue
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.ModalDrawerSheet
import androidx.compose.material3.ModalNavigationDrawer
import androidx.compose.material3.NavigationDrawerItem
import androidx.compose.material3.NavigationDrawerItemDefaults
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.OutlinedTextFieldDefaults
import androidx.compose.material3.Scaffold
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.material3.darkColorScheme
import androidx.compose.material3.rememberDrawerState
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.rememberCoroutineScope
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.staticCompositionLocalOf
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import kotlinx.coroutines.launch

enum class AppSurface(val label: String, val gatedIssue: String? = null) {
    Chat("Chat"),
    Logic("Logic", "#652"),
    Voice("Voice"),
    Projects("Projects", "#653"),
    Remote("Remote"),
    Scheduled("Scheduled", "#654"),
    Plugins("Plugins", "#655"),
    Themes("Themes"),
    Diagnostics("Diagnostics"),
    Settings("Settings"),
    About("About"),
}

data class ZaraSemanticTokens(
    val background: Color,
    val surface: Color,
    val surfaceElevated: Color,
    val surfaceInput: Color,
    val border: Color,
    val borderActive: Color,
    val primary: Color,
    val secondary: Color,
    val accentMagenta: Color,
    val accentCyan: Color,
    val text: Color,
    val textMuted: Color,
    val success: Color,
    val warning: Color,
    val error: Color,
    val focus: Color,
    val ambientGlow: Color,
)

private val OutrunTokens = ZaraSemanticTokens(
    background = Color(0xFF02040B),
    surface = Color(0xFF07101B),
    surfaceElevated = Color(0xFF0A1324),
    surfaceInput = Color(0xFF080F1E),
    border = Color(0xFF1A2A49),
    borderActive = Color(0xFF775CFF),
    primary = Color(0xFFE21CF2),
    secondary = Color(0xFF16D9FF),
    accentMagenta = Color(0xFFF000FF),
    accentCyan = Color(0xFF00D7FF),
    text = Color(0xFFEAF2FF),
    textMuted = Color(0xFF8D9DBA),
    success = Color(0xFF6CE7A6),
    warning = Color(0xFFFFD166),
    error = Color(0xFFFF6B8B),
    focus = Color(0xFFB56DFF),
    ambientGlow = Color(0xFF3A0D5E),
)

enum class ZaraTheme { Outrun, StarIntel, Midnight, Terminal, Light, System }

fun themeTokens(theme: ZaraTheme, systemDark: Boolean, reducedGlow: Boolean): ZaraSemanticTokens {
    val resolved = if (theme == ZaraTheme.System) {
        if (systemDark) ZaraTheme.Outrun else ZaraTheme.Light
    } else theme
    val tokens = when (resolved) {
        ZaraTheme.StarIntel -> OutrunTokens.copy(
            background = Color(0xFF080807), surface = Color(0xFF14130F),
            surfaceElevated = Color(0xFF201D15), surfaceInput = Color(0xFF10100D),
            border = Color(0xFF4C4329), borderActive = Color(0xFFE8C56A),
            primary = Color(0xFFE8C56A), secondary = Color(0xFFF1DA9A),
            accentMagenta = Color(0xFFD4AF37), accentCyan = Color(0xFFF1DA9A),
            text = Color(0xFFF8F3E6), textMuted = Color(0xFFBEB5A1),
            focus = Color(0xFFFFD971), ambientGlow = Color(0xFF342A10),
        )
        ZaraTheme.Midnight -> OutrunTokens.copy(
            background = Color(0xFF080919), surface = Color(0xFF11132A),
            surfaceElevated = Color(0xFF1B1D3C), surfaceInput = Color(0xFF0D1024),
            border = Color(0xFF343B68), borderActive = Color(0xFF9C92FF),
            primary = Color(0xFFB3A4FF), secondary = Color(0xFF8ABFFF),
            accentMagenta = Color(0xFFB3A4FF), accentCyan = Color(0xFF8ABFFF),
            focus = Color(0xFFCEC4FF), ambientGlow = Color(0xFF24204E),
        )
        ZaraTheme.Terminal -> OutrunTokens.copy(
            background = Color(0xFF030805), surface = Color(0xFF08120C),
            surfaceElevated = Color(0xFF102117), surfaceInput = Color(0xFF050D08),
            border = Color(0xFF294E36), borderActive = Color(0xFF8EF0A8),
            primary = Color(0xFF8EF0A8), secondary = Color(0xFFADEBC0),
            accentMagenta = Color(0xFF8EF0A8), accentCyan = Color(0xFFADEBC0),
            text = Color(0xFFE3F8E9), textMuted = Color(0xFF9CBBA6),
            focus = Color(0xFFBFFFCC), ambientGlow = Color(0xFF12321D),
        )
        ZaraTheme.Light -> OutrunTokens.copy(
            background = Color(0xFFF7F7FA), surface = Color(0xFFFFFFFF),
            surfaceElevated = Color(0xFFECECF3), surfaceInput = Color(0xFFF2F2F7),
            border = Color(0xFFB8BAC8), borderActive = Color(0xFF6450A8),
            primary = Color(0xFF7A247D), secondary = Color(0xFF006478),
            accentMagenta = Color(0xFF88258C), accentCyan = Color(0xFF006478),
            text = Color(0xFF1C2030), textMuted = Color(0xFF555C70),
            success = Color(0xFF17623B), warning = Color(0xFF765100),
            error = Color(0xFFAC2044), focus = Color(0xFF6034A0),
            ambientGlow = Color(0xFFEAE1F3),
        )
        else -> OutrunTokens
    }
    return if (reducedGlow) tokens.copy(ambientGlow = Color.Transparent) else tokens
}

val LocalZaraTokens = staticCompositionLocalOf { OutrunTokens }

private fun tokensColorScheme(tokens: ZaraSemanticTokens) = darkColorScheme(
    primary = tokens.primary,
    secondary = tokens.secondary,
    background = tokens.background,
    surface = tokens.surface,
    surfaceVariant = tokens.surfaceElevated,
    onPrimary = Color(0xFF160018),
    onSecondary = Color(0xFF00161A),
    onBackground = tokens.text,
    onSurface = tokens.text,
    onSurfaceVariant = tokens.textMuted,
    error = tokens.error,
)

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
    selectedTheme: ZaraTheme,
    onSelectTheme: (ZaraTheme) -> Unit,
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
    val drawerState = rememberDrawerState(initialValue = DrawerValue.Closed)
    val scope = rememberCoroutineScope()
    val systemDark = isSystemInDarkTheme()
    val tokens = themeTokens(selectedTheme, systemDark, reducedGlow = false)

    CompositionLocalProvider(LocalZaraTokens provides tokens) {
        MaterialTheme(colorScheme = tokensColorScheme(tokens)) {
            ModalNavigationDrawer(
                drawerState = drawerState,
                drawerContent = {
                    ZaraDrawer(
                        selected = selected,
                        state = runtimeState,
                        onSelect = { destination ->
                            selected = destination
                            scope.launch { drawerState.close() }
                        },
                    )
                },
            ) {
                Scaffold(
                    containerColor = tokens.background,
                    topBar = {
                        ZaraTopBar(
                            selected = selected,
                            state = runtimeState,
                            onMenu = { scope.launch { drawerState.open() } },
                        )
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
                        AppSurface.Logic -> GatedSurface(selected, padding)
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
                        AppSurface.Projects -> GatedSurface(selected, padding)
                        AppSurface.Remote -> ConnectionSurface(
                            state = runtimeState,
                            operationError = operationError,
                            operationBusy = operationBusy,
                            onConnect = onConnect,
                            padding = padding,
                        )
                        AppSurface.Scheduled -> GatedSurface(selected, padding)
                        AppSurface.Plugins -> GatedSurface(selected, padding)
                        AppSurface.Themes -> ThemesSurface(
                            selected = selectedTheme,
                            onSelectTheme = onSelectTheme,
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
                        AppSurface.About -> AboutSurface(sourceSha, padding)
                    }
                }
            }
        }
    }
}

@Composable
private fun ZaraTopBar(
    selected: AppSurface,
    state: RuntimeState,
    onMenu: () -> Unit,
) {
    val tokens = LocalZaraTokens.current
    Surface(
        color = tokens.background,
        border = BorderStroke(1.dp, tokens.border),
    ) {
        Row(
            modifier = Modifier
                .fillMaxWidth()
                .windowInsetsPadding(WindowInsets.statusBars)
                .heightIn(min = 58.dp)
                .padding(horizontal = 12.dp, vertical = 6.dp),
            verticalAlignment = Alignment.CenterVertically,
        ) {
            TextButton(onClick = onMenu) {
                Text("☰", color = tokens.text, fontSize = 23.sp)
            }
            Text(
                selected.label,
                modifier = Modifier.weight(1f),
                color = tokens.textMuted,
                style = MaterialTheme.typography.labelLarge,
            )
            StatusDot(connectionAccent(tokens, state.server))
            Spacer(Modifier.size(8.dp))
            ZaraSigil(size = 34.dp)
        }
    }
}

@Composable
private fun ZaraDrawer(
    selected: AppSurface,
    state: RuntimeState,
    onSelect: (AppSurface) -> Unit,
) {
    val tokens = LocalZaraTokens.current
    ModalDrawerSheet(
        modifier = Modifier.fillMaxWidth(0.88f).widthIn(max = 360.dp),
        drawerContainerColor = tokens.surfaceElevated,
        drawerContentColor = tokens.text,
    ) {
        Column(
            modifier = Modifier
                .fillMaxSize()
                .verticalScroll(rememberScrollState())
                .padding(horizontal = 12.dp, vertical = 18.dp),
        ) {
            Row(
                modifier = Modifier.fillMaxWidth().padding(horizontal = 10.dp, vertical = 8.dp),
                verticalAlignment = Alignment.CenterVertically,
            ) {
                ZaraSigil(size = 44.dp)
                Column(Modifier.padding(start = 12.dp).weight(1f)) {
                    Text("SYMBOLIC INTELLIGENCE", color = tokens.text, fontWeight = FontWeight.SemiBold)
                    Text("ON YOUR TERMS", color = tokens.textMuted, style = MaterialTheme.typography.labelSmall)
                }
                StatusDot(connectionAccent(tokens, state.server))
            }

            Spacer(Modifier.size(8.dp))
            AppSurface.entries.forEach { surface ->
                NavigationDrawerItem(
                    label = {
                        Row(verticalAlignment = Alignment.CenterVertically) {
                            Text(surface.label, modifier = Modifier.weight(1f))
                            surface.gatedIssue?.let {
                                Text(it, color = tokens.textMuted, style = MaterialTheme.typography.labelSmall)
                            }
                        }
                    },
                    selected = selected == surface,
                    onClick = { onSelect(surface) },
                    colors = NavigationDrawerItemDefaults.colors(
                        selectedContainerColor = tokens.ambientGlow,
                        unselectedContainerColor = Color.Transparent,
                        selectedTextColor = tokens.text,
                        unselectedTextColor = tokens.textMuted,
                    ),
                )
            }

            Spacer(Modifier.size(12.dp))
            DrawerDividerLabel("PINNED")
            DrawerHistoryRow("No pinned conversations", "history hook · #650")
            Spacer(Modifier.size(10.dp))
            DrawerDividerLabel("RECENTS")
            DrawerHistoryRow("Session history", "runtime projection · #650")
            Spacer(Modifier.weight(1f))

            Surface(
                color = tokens.surface,
                border = BorderStroke(1.dp, tokens.border),
                shape = MaterialTheme.shapes.medium,
                modifier = Modifier.fillMaxWidth(),
            ) {
                Row(
                    modifier = Modifier.padding(12.dp),
                    verticalAlignment = Alignment.CenterVertically,
                ) {
                    StatusDot(connectionAccent(tokens, state.server))
                    Text(
                        connectionLabel(state.server),
                        modifier = Modifier.padding(start = 10.dp),
                        color = tokens.textMuted,
                        style = MaterialTheme.typography.labelMedium,
                    )
                }
            }
        }
    }
}

@Composable
private fun DrawerDividerLabel(label: String) {
    val tokens = LocalZaraTokens.current
    Text(
        label,
        modifier = Modifier.padding(horizontal = 10.dp, vertical = 4.dp),
        color = tokens.accentCyan,
        fontFamily = FontFamily.Monospace,
        fontSize = 10.sp,
        letterSpacing = 1.8.sp,
    )
}

@Composable
private fun DrawerHistoryRow(title: String, detail: String) {
    val tokens = LocalZaraTokens.current
    Column(Modifier.fillMaxWidth().padding(horizontal = 10.dp, vertical = 5.dp)) {
        Text(title, color = tokens.textMuted, style = MaterialTheme.typography.bodySmall)
        Text(detail, color = tokens.borderActive, style = MaterialTheme.typography.labelSmall)
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
    val tokens = LocalZaraTokens.current

    Column(
        modifier = Modifier
            .fillMaxSize()
            .padding(padding)
            .padding(horizontal = 16.dp),
    ) {
        Column(
            modifier = Modifier
                .fillMaxWidth()
                .weight(1f)
                .verticalScroll(rememberScrollState()),
        ) {
            if (lastTurn == null) {
                Box(
                    modifier = Modifier.fillMaxWidth().heightIn(min = 360.dp),
                    contentAlignment = Alignment.Center,
                ) {
                    Column(horizontalAlignment = Alignment.CenterHorizontally) {
                        ZaraSigil(size = 104.dp)
                        Text(
                            "SYMBOLIC INTELLIGENCE",
                            modifier = Modifier.padding(top = 20.dp),
                            color = tokens.text,
                            fontFamily = FontFamily.Monospace,
                            fontWeight = FontWeight.SemiBold,
                            letterSpacing = 2.sp,
                        )
                        Text(
                            "ON YOUR TERMS",
                            modifier = Modifier.padding(top = 5.dp),
                            color = tokens.textMuted,
                            fontFamily = FontFamily.Monospace,
                            fontSize = 11.sp,
                            letterSpacing = 3.sp,
                        )
                        Row(
                            modifier = Modifier.padding(top = 18.dp),
                            horizontalArrangement = Arrangement.spacedBy(8.dp),
                        ) {
                            StatusPill(enrollmentLabel(state.enrollment))
                            StatusPill(connectionLabel(state.server))
                        }
                    }
                }
            } else {
                Spacer(Modifier.size(18.dp))
                UserMessage(lastTurn.userText)
                Spacer(Modifier.size(12.dp))
                AssistantMessage(lastTurn.assistantText, lastTurn.success)
            }
            operationError?.let { ErrorBanner(it) }
        }

        CompactComposer(
            value = input,
            onValueChange = { input = it },
            ready = ready,
            operationBusy = operationBusy,
            onSend = {
                val message = input.trim()
                if (message.isNotEmpty()) {
                    input = ""
                    onSendText(message)
                }
            },
        )
        Text(
            if (ready) "LOCAL  •  SYMBOLIC  •  PRIVATE" else "REMOTE AUTH REQUIRED FOR CHAT",
            modifier = Modifier.fillMaxWidth().padding(top = 7.dp, bottom = 10.dp),
            color = tokens.textMuted,
            textAlign = TextAlign.Center,
            fontFamily = FontFamily.Monospace,
            fontSize = 9.sp,
            letterSpacing = 1.6.sp,
        )
    }
}

@Composable
private fun CompactComposer(
    value: String,
    onValueChange: (String) -> Unit,
    ready: Boolean,
    operationBusy: Boolean,
    onSend: () -> Unit,
) {
    val tokens = LocalZaraTokens.current
    OutlinedTextField(
        value = value,
        onValueChange = onValueChange,
        modifier = Modifier.fillMaxWidth(),
        enabled = ready && !operationBusy,
        singleLine = true,
        placeholder = {
            Text(
                if (ready) "Ask anything…" else "Connect in Remote to chat",
                color = tokens.textMuted,
            )
        },
        trailingIcon = {
            TextButton(
                onClick = onSend,
                enabled = ready && value.isNotBlank() && !operationBusy,
            ) {
                Text(if (operationBusy) "…" else "↑", fontSize = 20.sp)
            }
        },
        shape = MaterialTheme.shapes.extraLarge,
        colors = fieldColors(),
    )
}

@Composable
private fun UserMessage(text: String) {
    val tokens = LocalZaraTokens.current
    Row(modifier = Modifier.fillMaxWidth(), horizontalArrangement = Arrangement.End) {
        Surface(
            color = tokens.surfaceElevated,
            border = BorderStroke(1.dp, tokens.borderActive),
            shape = MaterialTheme.shapes.large,
            modifier = Modifier.widthIn(max = 320.dp),
        ) {
            Text(text, modifier = Modifier.padding(14.dp), color = tokens.text)
        }
    }
}

@Composable
private fun AssistantMessage(text: String, success: Boolean) {
    val tokens = LocalZaraTokens.current
    Surface(
        color = tokens.surface,
        border = BorderStroke(1.dp, if (success) tokens.border else tokens.error),
        shape = MaterialTheme.shapes.large,
        modifier = Modifier.fillMaxWidth(),
    ) {
        Column(Modifier.padding(16.dp)) {
            Row(verticalAlignment = Alignment.CenterVertically) {
                ZaraSigil(size = 26.dp)
                Text(
                    if (success) "ASSISTANT" else "ASSISTANT ERROR",
                    modifier = Modifier.padding(start = 9.dp),
                    color = if (success) tokens.accentCyan else tokens.error,
                    fontFamily = FontFamily.Monospace,
                    fontSize = 10.sp,
                    letterSpacing = 1.4.sp,
                )
            }
            Text(text, modifier = Modifier.padding(top = 12.dp), color = tokens.text)
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
    val tokens = LocalZaraTokens.current
    ScreenBody(padding) {
        ScreenTitle("Voice", "Authenticated capture and playback")
        SectionCard("RUNTIME") {
            KeyValueRow("connection", connectionLabel(state.server))
            KeyValueRow("microphone", if (capturing) "capturing" else "idle")
            KeyValueRow("permission", if (microphonePermissionGranted) "granted" else "required")
        }
        voiceStreamState?.let { stream ->
            SectionCard("STREAM") {
                if (stream.transcriptStreamId != null) {
                    Text(
                        stream.transcriptText,
                        color = tokens.text,
                        style = MaterialTheme.typography.bodyLarge,
                    )
                }
                KeyValueRow("speaker", stream.audio?.let { "${it.sampleRate} Hz mono" } ?: "idle")
            }
        }
        voiceStreamFailure?.let { ErrorBanner(it) }
        when {
            !microphonePermissionGranted -> PrimaryAction(
                "Grant microphone permission",
                !operationBusy && !capturing,
                onRequestMicrophonePermission,
            )
            !canStartManualVoice(state, microphonePermissionGranted) && !capturing ->
                MutedNotice("Voice becomes available after an authenticated Remote session connects.")
            capturing -> {
                PrimaryAction("Stop & send", !operationBusy, onStopVoice)
                SecondaryAction("Cancel", !operationBusy, onCancelVoice)
            }
            else -> PrimaryAction("Start talking", !operationBusy, onStartVoice)
        }
        operationError?.let { ErrorBanner(it) }
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
    ScreenBody(padding) {
        ScreenTitle("Remote", "Authenticated Zara server")
        SectionCard("STATUS") {
            KeyValueRow("connection", connectionLabel(state.server))
            KeyValueRow("enrollment", enrollmentLabel(state.enrollment))
            KeyValueRow("session", state.sessionId ?: "none")
        }
        SectionCard("SERVER") {
            OutlinedTextField(
                value = endpoint,
                onValueChange = { endpoint = it },
                modifier = Modifier.fillMaxWidth(),
                label = { Text("tcp://host:port") },
                enabled = !operationBusy && canRequestConnect(state.server),
                singleLine = true,
                colors = fieldColors(),
            )
            if (state.enrollment != EnrollmentReadiness.Ready) {
                MutedNotice("Finish client enrollment and server pinning in Settings first.")
            }
            PrimaryAction(
                label = if (operationBusy) "Connecting…" else "Connect",
                enabled = endpoint.isNotBlank() &&
                    state.enrollment == EnrollmentReadiness.Ready &&
                    canRequestConnect(state.server) &&
                    !operationBusy,
                onClick = { onConnect(endpoint) },
            )
        }
        operationError?.let { ErrorBanner(it) }
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
    var showAssistantHelp by rememberSaveable { mutableStateOf(false) }
    val tokens = LocalZaraTokens.current

    ScreenBody(padding) {
        ScreenTitle("Settings", "Identity, assistant role and enrollment")

        SectionCard("ASSISTANT") {
            KeyValueRow("role", assistantRoleLabel(state.assistantRole))
            when (state.assistantRole) {
                AssistantRole.NotYetAssessed -> MutedNotice("Checking Android Assistant role availability.")
                AssistantRole.Held -> MutedNotice("Zara is the current Android Assistant.")
                AssistantRole.NotHeld -> {
                    PrimaryAction("Make Zara assistant", !operationBusy, onRequestAssistantRole)
                    TextButton(onClick = { showAssistantHelp = !showAssistantHelp }) {
                        Text(if (showAssistantHelp) "Hide Samsung setup help" else "Samsung setup help")
                    }
                    if (showAssistantHelp) {
                        Text(
                            samsungAssistantSetupGuidance(),
                            color = tokens.textMuted,
                            style = MaterialTheme.typography.bodySmall,
                        )
                    }
                }
                AssistantRole.PlatformUnavailable ->
                    MutedNotice("This Android configuration does not expose the public Assistant role.")
            }
        }

        SectionCard("IDENTITY") {
            KeyValueRow("enrollment", enrollmentLabel(state.enrollment))
            enrollmentPublicKey?.let {
                Text("CLIENT PUBLIC KEY", color = tokens.accentCyan, style = MaterialTheme.typography.labelSmall)
                SelectionContainer {
                    Text(
                        it,
                        modifier = Modifier.fillMaxWidth().padding(top = 6.dp),
                        color = tokens.text,
                        fontFamily = FontFamily.Monospace,
                        style = MaterialTheme.typography.bodySmall,
                    )
                }
                MutedNotice("Enroll only this public key on the server. The private key remains in Android Keystore-backed storage.")
            }
            when (state.enrollment) {
                EnrollmentReadiness.Unenrolled -> PrimaryAction(
                    "Create client identity",
                    !operationBusy,
                    onCreateIdentity,
                )
                EnrollmentReadiness.AwaitingServerPin -> {
                    OutlinedTextField(
                        value = serverPin,
                        onValueChange = { serverPin = it },
                        modifier = Modifier.fillMaxWidth(),
                        label = { Text("Server CURVE public key") },
                        enabled = !operationBusy,
                        singleLine = true,
                        colors = fieldColors(),
                    )
                    PrimaryAction(
                        "Pin server key",
                        serverPin.isNotBlank() && !operationBusy,
                    ) { onPinServer(serverPin) }
                }
                EnrollmentReadiness.Ready ->
                    MutedNotice("Client identity and server pin are ready. Server-side enrollment is still required.")
                EnrollmentReadiness.Corrupt ->
                    ErrorBanner("Enrollment storage is corrupt; connection is disabled.")
            }
        }

        state.configuredProfile?.endpoint?.let {
            SectionCard("REMOTE PROFILE") {
                KeyValueRow("endpoint", it)
            }
        }
        operationError?.let { ErrorBanner(it) }
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
    ScreenBody(padding) {
        ScreenTitle("Diagnostics", "Bounded runtime state")
        SectionCard("BUILD") {
            KeyValueRow("source", sourceSha.take(12))
        }
        SectionCard("RUNTIME") {
            KeyValueRow("connection", connectionLabel(state.server))
            KeyValueRow("generation", state.generation.toString())
            KeyValueRow("session", state.sessionId ?: "none")
            KeyValueRow("conversation", state.selectedConversationId ?: "none")
            KeyValueRow("enrollment", enrollmentLabel(state.enrollment))
            KeyValueRow("assistant role", assistantRoleLabel(state.assistantRole))
        }
        voiceStreamState?.let { stream ->
            SectionCard("VOICE") {
                KeyValueRow("session", stream.sessionId)
                KeyValueRow("transcript stream", stream.transcriptStreamId ?: "none")
                KeyValueRow("audio stream", stream.audio?.streamId ?: "none")
            }
        }
        voiceStreamFailure?.let { ErrorBanner(it) }
        operationError?.let { ErrorBanner(it) }
    }
}

@Composable
private fun ThemesSurface(
    selected: ZaraTheme,
    onSelectTheme: (ZaraTheme) -> Unit,
    padding: PaddingValues,
) {
    val systemDark = isSystemInDarkTheme()
    ScreenBody(padding) {
        ScreenTitle("Themes", "Appearance")
        SectionCard("Appearance") {
            ZaraTheme.entries.forEach { theme ->
                ThemePreviewCard(
                    theme = theme,
                    selected = theme == selected,
                    systemDark = systemDark,
                    onClick = { onSelectTheme(theme) },
                )
            }
        }
    }
}

@Composable
private fun ThemePreviewCard(
    theme: ZaraTheme,
    selected: Boolean,
    systemDark: Boolean,
    onClick: () -> Unit,
) {
    val tokens = LocalZaraTokens.current
    val palette = themeTokens(theme, systemDark, reducedGlow = false)
    Surface(
        modifier = Modifier.fillMaxWidth(),
        onClick = onClick,
        color = palette.surface,
        border = BorderStroke(
            width = if (selected) 2.dp else 1.dp,
            color = if (selected) palette.borderActive else tokens.border,
        ),
        shape = MaterialTheme.shapes.medium,
    ) {
        Row(
            modifier = Modifier.padding(horizontal = 12.dp, vertical = 10.dp),
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.spacedBy(10.dp),
        ) {
            Row(horizontalArrangement = Arrangement.spacedBy(4.dp)) {
                listOf(
                    palette.background,
                    palette.primary,
                    palette.accentMagenta,
                    palette.accentCyan,
                    palette.text,
                ).forEach { swatch ->
                    Box(
                        modifier = Modifier
                            .size(16.dp)
                            .background(swatch, MaterialTheme.shapes.extraSmall)
                    )
                }
            }
            Text(
                theme.name,
                color = palette.text,
                fontFamily = FontFamily.Monospace,
                style = MaterialTheme.typography.bodyMedium,
            )
            Spacer(modifier = Modifier.weight(1f))
            if (selected) {
                Text(
                    "ACTIVE",
                    color = palette.borderActive,
                    fontFamily = FontFamily.Monospace,
                    fontSize = 10.sp,
                    letterSpacing = 1.5.sp,
                )
            }
        }
    }
}

@Composable
private fun GatedSurface(surface: AppSurface, padding: PaddingValues) {
    val tokens = LocalZaraTokens.current
    ScreenBody(padding) {
        ScreenTitle(surface.label, "Product surface")
        SectionCard("NOT YET WIRED") {
            Text(
                "This route is intentionally visible but disabled until ${surface.gatedIssue ?: "its implementation issue"}. No fake backend is running behind it.",
                color = tokens.textMuted,
            )
        }
    }
}

@Composable
private fun AboutSurface(sourceSha: String, padding: PaddingValues) {
    ScreenBody(padding) {
        ScreenTitle("About", "Zara Android")
        SectionCard("BUILD") {
            KeyValueRow("source", sourceSha.take(12))
            KeyValueRow("design", "frozen 2026-09-07")
            MutedNotice("Chat-first symbolic assistant. Native Compose, shared semantic runtime contracts.")
        }
    }
}

@Composable
private fun ScreenBody(
    padding: PaddingValues,
    content: @Composable () -> Unit,
) {
    Column(
        modifier = Modifier
            .fillMaxSize()
            .padding(padding)
            .verticalScroll(rememberScrollState())
            .padding(horizontal = 18.dp, vertical = 16.dp),
        verticalArrangement = Arrangement.spacedBy(14.dp),
    ) {
        content()
    }
}

@Composable
private fun ScreenTitle(title: String, subtitle: String) {
    val tokens = LocalZaraTokens.current
    Text(title, color = tokens.text, style = MaterialTheme.typography.headlineSmall)
    Text(
        subtitle,
        modifier = Modifier.padding(top = 2.dp),
        color = tokens.textMuted,
        style = MaterialTheme.typography.bodySmall,
    )
}

@Composable
private fun SectionCard(title: String, content: @Composable () -> Unit) {
    val tokens = LocalZaraTokens.current
    Surface(
        modifier = Modifier.fillMaxWidth(),
        color = tokens.surface,
        border = BorderStroke(1.dp, tokens.border),
        shape = MaterialTheme.shapes.large,
    ) {
        Column(
            modifier = Modifier.padding(16.dp),
            verticalArrangement = Arrangement.spacedBy(10.dp),
        ) {
            Text(
                title,
                color = tokens.accentCyan,
                fontFamily = FontFamily.Monospace,
                fontSize = 10.sp,
                letterSpacing = 1.5.sp,
            )
            content()
        }
    }
}

@Composable
private fun KeyValueRow(label: String, value: String) {
    val tokens = LocalZaraTokens.current
    Row(modifier = Modifier.fillMaxWidth(), verticalAlignment = Alignment.Top) {
        Text(
            label,
            modifier = Modifier.weight(0.38f),
            color = tokens.textMuted,
            style = MaterialTheme.typography.bodySmall,
        )
        SelectionContainer {
            Text(
                value,
                modifier = Modifier.weight(0.62f),
                color = tokens.text,
                fontFamily = FontFamily.Monospace,
                style = MaterialTheme.typography.bodySmall,
                textAlign = TextAlign.End,
            )
        }
    }
}

@Composable
private fun StatusPill(label: String) {
    val tokens = LocalZaraTokens.current
    Surface(
        color = tokens.surface,
        border = BorderStroke(1.dp, tokens.border),
        shape = MaterialTheme.shapes.extraLarge,
    ) {
        Text(
            label.uppercase(),
            modifier = Modifier.padding(horizontal = 10.dp, vertical = 6.dp),
            color = tokens.textMuted,
            fontFamily = FontFamily.Monospace,
            fontSize = 9.sp,
            letterSpacing = 1.sp,
        )
    }
}

@Composable
private fun PrimaryAction(label: String, enabled: Boolean, onClick: () -> Unit) {
    val tokens = LocalZaraTokens.current
    Button(
        onClick = onClick,
        enabled = enabled,
        colors = ButtonDefaults.buttonColors(
            containerColor = tokens.primary,
            contentColor = Color(0xFF160018),
        ),
    ) {
        Text(label)
    }
}

@Composable
private fun SecondaryAction(label: String, enabled: Boolean, onClick: () -> Unit) {
    val tokens = LocalZaraTokens.current
    TextButton(onClick = onClick, enabled = enabled) {
        Text(label, color = tokens.secondary)
    }
}

@Composable
private fun MutedNotice(text: String) {
    val tokens = LocalZaraTokens.current
    Text(text, color = tokens.textMuted, style = MaterialTheme.typography.bodySmall)
}

@Composable
private fun ErrorBanner(text: String) {
    val tokens = LocalZaraTokens.current
    Surface(
        modifier = Modifier.fillMaxWidth().padding(top = 10.dp),
        color = tokens.surface,
        border = BorderStroke(1.dp, tokens.error),
        shape = MaterialTheme.shapes.medium,
    ) {
        Text(text, modifier = Modifier.padding(12.dp), color = tokens.error)
    }
}

@Composable
private fun StatusDot(color: Color) {
    Canvas(Modifier.size(8.dp)) {
        drawCircle(color = color, radius = size.minDimension / 2f)
    }
}

@Composable
private fun ZaraSigil(size: Dp) {
    val tokens = LocalZaraTokens.current
    Canvas(Modifier.size(size)) {
        val center = Offset(this.size.width / 2f, this.size.height / 2f)
        val stroke = this.size.minDimension * 0.028f
        val radius = this.size.minDimension * 0.34f
        drawCircle(
            color = tokens.borderActive,
            radius = radius,
            center = center,
            style = androidx.compose.ui.graphics.drawscope.Stroke(width = stroke),
        )
        drawLine(
            color = tokens.accentMagenta,
            start = Offset(center.x, center.y - radius * 1.35f),
            end = Offset(center.x, center.y + radius * 1.35f),
            strokeWidth = stroke,
        )
        drawCircle(
            color = tokens.accentCyan,
            radius = stroke * 1.5f,
            center = Offset(center.x, center.y + radius * 1.35f),
        )
        drawCircle(
            color = tokens.accentMagenta,
            radius = stroke * 1.5f,
            center = Offset(center.x, center.y - radius * 1.35f),
        )
        drawLine(
            color = tokens.accentCyan,
            start = Offset(center.x - radius, center.y),
            end = Offset(center.x + radius, center.y),
            strokeWidth = stroke * 0.65f,
        )
    }
}

@Composable
private fun fieldColors() = run {
    val tokens = LocalZaraTokens.current
    OutlinedTextFieldDefaults.colors(
        focusedTextColor = tokens.text,
        unfocusedTextColor = tokens.text,
        disabledTextColor = tokens.textMuted,
        focusedBorderColor = tokens.borderActive,
        unfocusedBorderColor = tokens.border,
        disabledBorderColor = tokens.border,
        focusedLabelColor = tokens.secondary,
        unfocusedLabelColor = tokens.textMuted,
        cursorColor = tokens.secondary,
        focusedContainerColor = tokens.surfaceInput,
        unfocusedContainerColor = tokens.surfaceInput,
        disabledContainerColor = tokens.surfaceInput,
    )
}

private fun connectionAccent(tokens: ZaraSemanticTokens, connection: ServerConnection): Color = when (connection) {
    is ServerConnection.Connected -> tokens.success
    is ServerConnection.Connecting, is ServerConnection.Reconnecting -> tokens.warning
    is ServerConnection.OfflineDegraded -> tokens.warning
    ServerConnection.Disconnected -> tokens.textMuted
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
