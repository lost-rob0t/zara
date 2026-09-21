package ai.zara.app.ui

import ai.zara.app.BuildConfig
import ai.zara.app.conversations.ConversationRecord
import ai.zara.app.conversations.ConversationState
import ai.zara.app.conversations.ConversationStatus
import ai.zara.app.localai.LocalAiState
import ai.zara.app.localai.LocalModelBackend
import ai.zara.app.localai.LocalModelQuantization
import ai.zara.app.localai.LocalModelSpec
import ai.zara.app.projects.ProjectContext
import ai.zara.app.projects.ProjectContextState
import ai.zara.app.runtime.AssistantRole
import ai.zara.app.runtime.EnrollmentReadiness
import ai.zara.app.runtime.RuntimeState
import ai.zara.app.runtime.RuntimeMode
import ai.zara.app.runtime.LocalQueryResult
import ai.zara.app.runtime.LocalServerPhase
import ai.zara.app.runtime.LocalServerState
import ai.zara.app.runtime.ServerConnection
import ai.zara.app.prolog.PrologSource
import ai.zara.app.prolog.LocalEmbeddingConfiguration
import ai.zara.app.update.UpdatePhase
import ai.zara.app.update.UpdateState
import ai.zara.app.voice.ManualVoiceState
import ai.zara.app.voice.VoiceStreamState
import ai.zara.ui.theme.ZaraSemanticTokens
import ai.zara.ui.theme.ZaraTheme
import ai.zara.ui.theme.themeTokens
import androidx.activity.compose.BackHandler
import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.Canvas
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.isSystemInDarkTheme
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.BoxWithConstraints
import androidx.compose.foundation.layout.consumeWindowInsets
import androidx.compose.foundation.layout.imePadding
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
import androidx.compose.material3.AlertDialog
import androidx.compose.material3.Button
import androidx.compose.material3.ButtonDefaults
import androidx.compose.material3.DrawerValue
import androidx.compose.material3.DropdownMenu
import androidx.compose.material3.DropdownMenuItem
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.ModalDrawerSheet
import androidx.compose.material3.ModalNavigationDrawer
import androidx.compose.material3.NavigationDrawerItem
import androidx.compose.material3.NavigationDrawerItemDefaults
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.OutlinedTextFieldDefaults
import androidx.compose.material3.RadioButton
import androidx.compose.material3.Scaffold
import androidx.compose.material3.Surface
import androidx.compose.material3.Switch
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.material3.darkColorScheme
import androidx.compose.material3.rememberDrawerState
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.runtime.getValue
import androidx.compose.runtime.key
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.rememberCoroutineScope
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.saveable.rememberSaveableStateHolder
import androidx.compose.runtime.staticCompositionLocalOf
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.semantics.clearAndSetSemantics
import androidx.compose.ui.semantics.contentDescription
import androidx.compose.ui.semantics.semantics
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import kotlinx.coroutines.launch

enum class AppSurface(val label: String, val glyph: String, val gatedIssue: String? = null) {
    Chat("Chat", "⌂"),
    Logic("Logic", "λ"),
    Voice("Voice", "◉"),
    Projects("Projects", "◇", "#653"),
    Remote("Remote", "⇄"),
    Scheduled("Scheduled", "◷", "#654"),
    Plugins("Plugins", "⬡", "#655"),
    Themes("Themes", "◐"),
    Diagnostics("Diagnostics", "⌁"),
    Settings("Settings", "⚙"),
    About("About", "ⓘ"),
}

val LocalZaraTokens = staticCompositionLocalOf {
    themeTokens(ZaraTheme.Outrun, systemDark = true, reducedGlow = false)
}

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

@Composable
fun ZaraApp(
    runtimeState: RuntimeState,
    sourceSha: String,
    enrollmentPublicKey: String?,
    pinnedServerPublicKey: String?,
    conversationState: ConversationState,
    operationError: String?,
    operationBusy: Boolean,
    microphonePermissionGranted: Boolean,
    voiceState: ManualVoiceState,
    voiceStreamState: VoiceStreamState?,
    voiceStreamFailure: String?,
    selectedTheme: ZaraTheme,
    localServerState: LocalServerState,
    prologSources: List<PrologSource>,
    prologQueryResult: LocalQueryResult?,
    updateState: UpdateState,
    changelogVersion: String,
    changelogText: String?,
    showChangelog: Boolean,
    runtimeMode: RuntimeMode,
    localEmbedding: LocalEmbeddingConfiguration,
    localAiState: LocalAiState,
    localModels: List<LocalModelSpec>,
    localModelBusy: Boolean,
    projectState: ProjectContextState,
    onSelectTheme: (ZaraTheme) -> Unit,
    onSelectRuntimeMode: (RuntimeMode) -> Unit,
    onSetLocalEmbeddingEnabled: (Boolean) -> Unit,
    onImportLocalModel: (String, String, LocalModelQuantization, Int, LocalModelBackend) -> Unit,
    onSelectLocalModel: (String, String) -> Unit,
    onUnloadLocalModel: () -> Unit,
    onCreateIdentity: () -> Unit,
    onPinServer: (String) -> Unit,
    onReplaceServerPin: (String) -> Unit,
    onConnect: (String) -> Unit,
    onNewConversation: () -> Unit,
    onSelectConversation: (String) -> Unit,
    onToggleConversationPinned: (String, Boolean) -> Unit,
    onRenameConversation: (String, String) -> Unit,
    onMoveConversationToProject: (String, String?) -> Unit,
    onSendText: (String, ConversationRecord, ProjectContext?) -> Unit,
    onCreateProject: (String) -> Unit,
    onSelectProject: (String?) -> Unit,
    onRequestMicrophonePermission: () -> Unit,
    onRequestAssistantRole: () -> Unit,
    onStartVoice: () -> Unit,
    onStopVoice: () -> Unit,
    onCancelVoice: () -> Unit,
    onSavePrologSource: (String, String) -> Unit,
    onReloadLocalServer: () -> Unit,
    onRunPrologQuery: (String) -> Unit,
    onRenamePrologSource: (String, String) -> Unit,
    onDeletePrologSource: (String) -> Unit,
    onImportPrologWorkspace: (String) -> Unit,
    onExportPrologWorkspace: () -> String,
    onCheckForUpdate: () -> Unit,
    onSelectUpdate: (String) -> Unit,
    onDownloadUpdate: () -> Unit,
    onInstallUpdate: () -> Unit,
    onCopyDiagnostics: () -> Unit,
    onShareDiagnostics: () -> Unit,
    onClearDiagnostics: () -> Unit,
    onDismissChangelog: () -> Unit,
) {
    var navigation by rememberSaveable(stateSaver = AppNavigationSaver) {
        mutableStateOf(AppNavigation())
    }
    val selected = navigation.route.surface()
    val savedContent = rememberSaveableStateHolder()
    val drawerState = rememberDrawerState(initialValue = DrawerValue.Closed)
    val scope = rememberCoroutineScope()
    val systemDark = isSystemInDarkTheme()
    val tokens = themeTokens(selectedTheme, systemDark, reducedGlow = false)

    BackHandler(enabled = !drawerState.isOpen && navigation.back() != null) {
        navigation.back()?.let { navigation = it }
    }

    CompositionLocalProvider(LocalZaraTokens provides tokens) {
        MaterialTheme(colorScheme = tokensColorScheme(tokens)) {
            if (showChangelog && !changelogText.isNullOrBlank()) {
                AlertDialog(
                    onDismissRequest = onDismissChangelog,
                    confirmButton = {
                        TextButton(onClick = onDismissChangelog) { Text("Continue") }
                    },
                    title = { Text("What's new in Zara $changelogVersion") },
                    text = {
                        Column(
                            Modifier.heightIn(max = 480.dp).verticalScroll(rememberScrollState())
                        ) {
                            Text(changelogText)
                        }
                    },
                )
            }
            BoxWithConstraints(Modifier.fillMaxSize()) {
                val showRail = usesNavigationRail(maxWidth.value)
                ModalNavigationDrawer(
                    drawerState = drawerState,
                    drawerContent = {
                        ZaraDrawer(
                            selected = navigation.menu,
                            state = runtimeState,
                            localState = localServerState,
                            conversationState = conversationState,
                            projects = projectState.projects,
                            onSelect = { destination ->
                                navigation = navigation.selectMenu(destination)
                                scope.launch { drawerState.close() }
                            },
                            onNewConversation = {
                                onNewConversation()
                                navigation = navigation.selectMenu(AppMenu.Chat)
                                scope.launch { drawerState.close() }
                            },
                            onSelectConversation = { conversationId ->
                                onSelectConversation(conversationId)
                                navigation = navigation.selectMenu(AppMenu.Chat)
                                scope.launch { drawerState.close() }
                            },
                            onTogglePinned = onToggleConversationPinned,
                            onRenameConversation = onRenameConversation,
                            onMoveConversationToProject = onMoveConversationToProject,
                        )
                    },
                ) {
                    Row(Modifier.fillMaxSize()) {
                        if (showRail) {
                            ZaraNavigationRail(
                                selected = navigation.menu,
                                onSelect = { destination -> navigation = navigation.selectMenu(destination) },
                            )
                        }
                        Scaffold(
                            modifier = Modifier.weight(1f),
                            containerColor = tokens.background,
                            topBar = {
                                ZaraTopBar(
                                    selected = navigation.menu,
                                    state = runtimeState,
                                    localState = localServerState,
                                    onMenu = { scope.launch { drawerState.open() } },
                                )
                            },
                        ) { padding ->
                            Column(
                                Modifier.fillMaxSize().padding(padding)
                                    .consumeWindowInsets(padding).imePadding(),
                            ) {
                                key(navigation.menu) {
                                    ZaraRouteTabs(
                                        navigation = navigation,
                                        onSelect = { destination -> navigation = navigation.selectRoute(destination) },
                                    )
                                }
                                Box(Modifier.weight(1f).fillMaxWidth()) {
                                    savedContent.SaveableStateProvider(navigation.route.name) {
                                        val padding = PaddingValues(0.dp)
                                        when (selected) {
                                            AppSurface.Chat -> {
                                                val conversation = conversationState.selectedConversation
                                                val project = conversation?.projectId?.let(projectState::project)
                                                ChatSurface(
                                                    state = runtimeState,
                                                    localServerState = localServerState,
                                                    conversation = conversation,
                                                    project = project,
                                                    operationError = operationError,
                                                    operationBusy = operationBusy,
                                                    onSendText = onSendText,
                                                    padding = padding,
                                                )
                                            }
                                            AppSurface.Logic -> PrologStudioSurface(
                                                localState = localServerState,
                                                sources = prologSources,
                                                queryResult = prologQueryResult,
                                                operationError = operationError,
                                                operationBusy = operationBusy,
                                                onSaveSource = onSavePrologSource,
                                                onReload = onReloadLocalServer,
                                                onRunQuery = onRunPrologQuery,
                                                onRenameSource = onRenamePrologSource,
                                                onDeleteSource = onDeletePrologSource,
                                                onImportWorkspace = onImportPrologWorkspace,
                                                onExportWorkspace = onExportPrologWorkspace,
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
                                            AppSurface.Projects -> ProjectsSurface(
                                                state = projectState,
                                                operationError = operationError,
                                                operationBusy = operationBusy,
                                                onCreateProject = onCreateProject,
                                                onSelectProject = onSelectProject,
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
                                                localServerState = localServerState,
                                                voiceStreamState = voiceStreamState,
                                                voiceStreamFailure = voiceStreamFailure,
                                                operationError = operationError,
                                                onCopyDiagnostics = onCopyDiagnostics,
                                                onShareDiagnostics = onShareDiagnostics,
                                                onClearDiagnostics = onClearDiagnostics,
                                                padding = padding,
                                            )
                                            AppSurface.Remote, AppSurface.Settings -> SettingsSurface(
                                                section = navigation.route,
                                                microphonePermissionGranted = microphonePermissionGranted,
                                                onRequestMicrophonePermission = onRequestMicrophonePermission,
                                                onConnect = onConnect,
                                                state = runtimeState,
                                                localServerState = localServerState,
                                                updateState = updateState,
                                                runtimeMode = runtimeMode,
                                                localEmbedding = localEmbedding,
                                                localAiState = localAiState,
                                                localModels = localModels,
                                                localModelBusy = localModelBusy,
                                                selectedTheme = selectedTheme,
                                                enrollmentPublicKey = enrollmentPublicKey,
                                                pinnedServerPublicKey = pinnedServerPublicKey,
                                                operationError = operationError,
                                                operationBusy = operationBusy,
                                                onCreateIdentity = onCreateIdentity,
                                                onPinServer = onPinServer,
                                                onReplaceServerPin = onReplaceServerPin,
                                                onRequestAssistantRole = onRequestAssistantRole,
                                                onCheckForUpdate = onCheckForUpdate,
                                                onSelectUpdate = onSelectUpdate,
                                                onDownloadUpdate = onDownloadUpdate,
                                                onInstallUpdate = onInstallUpdate,
                                                onSelectRuntimeMode = onSelectRuntimeMode,
                                                onSetLocalEmbeddingEnabled = onSetLocalEmbeddingEnabled,
                                                onImportLocalModel = onImportLocalModel,
                                                onSelectLocalModel = onSelectLocalModel,
                                                onUnloadLocalModel = onUnloadLocalModel,
                                                onNavigateSettings = { route -> navigation = navigation.selectRoute(route) },
                                                padding = padding,
                                            )
                                            AppSurface.About -> AboutSurface(sourceSha, padding)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

internal fun AppRoute.surface(): AppSurface = when (this) {
    AppRoute.Chat -> AppSurface.Chat
    AppRoute.Voice -> AppSurface.Voice
    AppRoute.Logic -> AppSurface.Logic
    AppRoute.Projects -> AppSurface.Projects
    AppRoute.Scheduled -> AppSurface.Scheduled
    AppRoute.Connection -> AppSurface.Remote
    AppRoute.Appearance -> AppSurface.Themes
    AppRoute.Plugins -> AppSurface.Plugins
    AppRoute.Diagnostics -> AppSurface.Diagnostics
    AppRoute.About -> AppSurface.About
    AppRoute.Settings, AppRoute.Runtime, AppRoute.Permissions, AppRoute.Updates -> AppSurface.Settings
}

@Composable
private fun ZaraTopBar(
    selected: AppMenu,
    state: RuntimeState,
    localState: LocalServerState,
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
            TextButton(
                onClick = onMenu,
                modifier = Modifier.semantics { contentDescription = "Open navigation menu" },
            ) {
                Text("☰", color = tokens.text, fontSize = 23.sp)
            }
            Text(
                selected.label,
                modifier = Modifier.weight(1f),
                color = tokens.textMuted,
                style = MaterialTheme.typography.labelLarge,
            )
            StatusDot(if (localState.phase == LocalServerPhase.READY) tokens.success else tokens.warning)
            Spacer(Modifier.size(6.dp))
            StatusDot(connectionAccent(tokens, state.server))
            Spacer(Modifier.size(8.dp))
            ZaraSigil(size = 34.dp)
        }
    }
}

@Composable
private fun ZaraDrawer(
    selected: AppMenu,
    state: RuntimeState,
    localState: LocalServerState,
    conversationState: ConversationState,
    projects: List<ProjectContext>,
    onSelect: (AppMenu) -> Unit,
    onNewConversation: () -> Unit,
    onSelectConversation: (String) -> Unit,
    onTogglePinned: (String, Boolean) -> Unit,
    onRenameConversation: (String, String) -> Unit,
    onMoveConversationToProject: (String, String?) -> Unit,
) {
    val tokens = LocalZaraTokens.current
    var showAllPinned by rememberSaveable { mutableStateOf(false) }
    var showAllRecents by rememberSaveable { mutableStateOf(false) }
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
            AppMenu.entries.forEach { menu ->
                NavigationDrawerItem(
                    label = { Text(menu.label) },
                    icon = {
                        Text(
                            menu.glyph,
                            modifier = Modifier.clearAndSetSemantics { },
                            fontFamily = FontFamily.Monospace,
                        )
                    },
                    selected = selected == menu,
                    onClick = { onSelect(menu) },
                    colors = NavigationDrawerItemDefaults.colors(
                        selectedContainerColor = tokens.ambientGlow,
                        unselectedContainerColor = Color.Transparent,
                        selectedTextColor = tokens.text,
                        unselectedTextColor = tokens.textMuted,
                        selectedIconColor = tokens.accentCyan,
                        unselectedIconColor = tokens.textMuted,
                    ),
                )
            }

            TextButton(
                onClick = onNewConversation,
                enabled = conversationState.loadFailure == null,
                modifier = Modifier.fillMaxWidth().padding(horizontal = 4.dp, vertical = 4.dp),
            ) {
                Text(
                    "＋  New chat",
                    modifier = Modifier.fillMaxWidth(),
                    color = tokens.accentCyan,
                    textAlign = TextAlign.Start,
                )
            }

            conversationState.loadFailure?.let { failure ->
                DrawerHistoryEmpty("History unavailable", failure)
            }

            Spacer(Modifier.size(6.dp))
            DrawerDividerLabel("PINNED")
            if (conversationState.pinnedConversations.isEmpty()) {
                DrawerHistoryEmpty("No pinned conversations", "Pin a chat from its ⋮ menu")
            } else {
                val pinned = if (showAllPinned) {
                    conversationState.pinnedConversations
                } else {
                    conversationState.pinnedConversations.take(6)
                }
                pinned.forEach { conversation ->
                    key(conversation.id) {
                        ConversationDrawerRow(
                            conversation = conversation,
                            selected = conversation.id == conversationState.selectedConversationId,
                            projects = projects,
                            onSelect = onSelectConversation,
                            onTogglePinned = onTogglePinned,
                            onRenameConversation = onRenameConversation,
                            onMoveConversationToProject = onMoveConversationToProject,
                        )
                    }
                }
                if (conversationState.pinnedConversations.size > 6) {
                    HistoryExpansionAction(showAllPinned) { showAllPinned = !showAllPinned }
                }
            }

            Spacer(Modifier.size(10.dp))
            DrawerDividerLabel("RECENTS")
            if (conversationState.recentConversations.isEmpty()) {
                DrawerHistoryEmpty("No recent conversations", "Start a new chat")
            } else {
                val recents = if (showAllRecents) {
                    conversationState.recentConversations
                } else {
                    conversationState.recentConversations.take(10)
                }
                recents.forEach { conversation ->
                    key(conversation.id) {
                        ConversationDrawerRow(
                            conversation = conversation,
                            selected = conversation.id == conversationState.selectedConversationId,
                            projects = projects,
                            onSelect = onSelectConversation,
                            onTogglePinned = onTogglePinned,
                            onRenameConversation = onRenameConversation,
                            onMoveConversationToProject = onMoveConversationToProject,
                        )
                    }
                }
                if (conversationState.recentConversations.size > 10) {
                    HistoryExpansionAction(showAllRecents) { showAllRecents = !showAllRecents }
                }
            }

            Spacer(Modifier.weight(1f))
            Surface(
                color = tokens.surface,
                border = BorderStroke(1.dp, tokens.border),
                shape = MaterialTheme.shapes.medium,
                modifier = Modifier.fillMaxWidth(),
            ) {
                Column(modifier = Modifier.padding(12.dp)) {
                    Row(verticalAlignment = Alignment.CenterVertically) {
                        StatusDot(if (localState.phase == LocalServerPhase.READY) tokens.success else tokens.warning)
                        Text(
                            "local · ${localState.phase.name.lowercase()}",
                            modifier = Modifier.padding(start = 10.dp),
                            color = tokens.text,
                            style = MaterialTheme.typography.labelMedium,
                        )
                    }
                    Row(
                        modifier = Modifier.padding(top = 8.dp),
                        verticalAlignment = Alignment.CenterVertically,
                    ) {
                        StatusDot(connectionAccent(tokens, state.server))
                        Text(
                            "remote · ${connectionLabel(state.server)}",
                            modifier = Modifier.padding(start = 10.dp),
                            color = tokens.textMuted,
                            style = MaterialTheme.typography.labelMedium,
                        )
                    }
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
private fun DrawerHistoryEmpty(title: String, detail: String) {
    val tokens = LocalZaraTokens.current
    Column(Modifier.fillMaxWidth().padding(horizontal = 10.dp, vertical = 5.dp)) {
        Text(title, color = tokens.textMuted, style = MaterialTheme.typography.bodySmall)
        Text(detail, color = tokens.borderActive, style = MaterialTheme.typography.labelSmall)
    }
}

@Composable
private fun HistoryExpansionAction(
    expanded: Boolean,
    onClick: () -> Unit,
) {
    val tokens = LocalZaraTokens.current
    TextButton(
        onClick = onClick,
        modifier = Modifier.fillMaxWidth().padding(horizontal = 4.dp),
    ) {
        Text(
            if (expanded) "Show less" else "See all…",
            modifier = Modifier.fillMaxWidth(),
            color = tokens.textMuted,
            textAlign = TextAlign.Start,
        )
    }
}

@Composable
private fun ConversationDrawerRow(
    conversation: ConversationRecord,
    selected: Boolean,
    projects: List<ProjectContext>,
    onSelect: (String) -> Unit,
    onTogglePinned: (String, Boolean) -> Unit,
    onRenameConversation: (String, String) -> Unit,
    onMoveConversationToProject: (String, String?) -> Unit,
) {
    val tokens = LocalZaraTokens.current
    var menuExpanded by rememberSaveable(conversation.id) { mutableStateOf(false) }
    var renameOpen by rememberSaveable(conversation.id) { mutableStateOf(false) }
    var renameDraft by rememberSaveable(conversation.id) { mutableStateOf(conversation.title) }
    var moveOpen by rememberSaveable(conversation.id) { mutableStateOf(false) }
    val projectName = conversation.projectId?.let { projectId ->
        projects.firstOrNull { it.id == projectId }?.name ?: "project"
    }
    val statusLabel = conversation.status.name.lowercase()
    val detail = listOfNotNull(projectName, statusLabel).joinToString(" · ")

    Surface(
        modifier = Modifier
            .fillMaxWidth()
            .padding(vertical = 2.dp)
            .clickable { onSelect(conversation.id) },
        color = if (selected) tokens.surface else Color.Transparent,
        border = if (selected) BorderStroke(1.dp, tokens.borderActive) else null,
        shape = MaterialTheme.shapes.medium,
    ) {
        Row(
            modifier = Modifier.fillMaxWidth().padding(start = 10.dp, top = 5.dp, bottom = 5.dp),
            verticalAlignment = Alignment.CenterVertically,
        ) {
            StatusDot(conversationStatusColor(tokens, conversation.status))
            Column(Modifier.padding(start = 9.dp).weight(1f)) {
                Text(conversation.title, color = tokens.text, style = MaterialTheme.typography.bodySmall)
                Text(detail, color = tokens.textMuted, style = MaterialTheme.typography.labelSmall)
            }
            Box {
                TextButton(
                    onClick = { menuExpanded = true },
                    modifier = Modifier.semantics {
                        contentDescription = "Actions for ${conversation.title}"
                    },
                ) {
                    Text("⋮", color = tokens.textMuted)
                }
                DropdownMenu(
                    expanded = menuExpanded,
                    onDismissRequest = { menuExpanded = false },
                ) {
                    DropdownMenuItem(
                        text = { Text(if (conversation.pinned) "Unpin" else "Pin") },
                        onClick = {
                            menuExpanded = false
                            onTogglePinned(conversation.id, !conversation.pinned)
                        },
                    )
                    DropdownMenuItem(
                        text = { Text("Rename") },
                        onClick = {
                            menuExpanded = false
                            renameDraft = conversation.title
                            renameOpen = true
                        },
                    )
                    DropdownMenuItem(
                        text = { Text("Move to project") },
                        onClick = {
                            menuExpanded = false
                            moveOpen = true
                        },
                    )
                }
            }
        }
    }

    if (renameOpen) {
        AlertDialog(
            onDismissRequest = { renameOpen = false },
            title = { Text("Rename chat") },
            text = {
                OutlinedTextField(
                    value = renameDraft,
                    onValueChange = { renameDraft = it.take(120) },
                    singleLine = true,
                    label = { Text("Chat name") },
                )
            },
            confirmButton = {
                TextButton(
                    enabled = renameDraft.isNotBlank(),
                    onClick = {
                        onRenameConversation(conversation.id, renameDraft.trim())
                        renameOpen = false
                    },
                ) {
                    Text("Rename")
                }
            },
            dismissButton = {
                TextButton(onClick = { renameOpen = false }) { Text("Cancel") }
            },
        )
    }

    if (moveOpen) {
        AlertDialog(
            onDismissRequest = { moveOpen = false },
            title = { Text("Move chat to project") },
            text = {
                Column(
                    modifier = Modifier
                        .fillMaxWidth()
                        .heightIn(max = 360.dp)
                        .verticalScroll(rememberScrollState()),
                ) {
                    TextButton(
                        onClick = {
                            onMoveConversationToProject(conversation.id, null)
                            moveOpen = false
                        },
                        modifier = Modifier.fillMaxWidth(),
                    ) {
                        Text(
                            "No project",
                            modifier = Modifier.fillMaxWidth(),
                            textAlign = TextAlign.Start,
                        )
                    }
                    projects.forEach { project ->
                        TextButton(
                            onClick = {
                                onMoveConversationToProject(conversation.id, project.id)
                                moveOpen = false
                            },
                            modifier = Modifier.fillMaxWidth(),
                        ) {
                            Text(
                                project.name,
                                modifier = Modifier.fillMaxWidth(),
                                textAlign = TextAlign.Start,
                            )
                        }
                    }
                }
            },
            confirmButton = {
                TextButton(onClick = { moveOpen = false }) { Text("Cancel") }
            },
        )
    }
}

private fun conversationStatusColor(
    tokens: ZaraSemanticTokens,
    status: ConversationStatus,
): Color = when (status) {
    ConversationStatus.Empty -> tokens.border
    ConversationStatus.Running -> tokens.accentCyan
    ConversationStatus.Success -> tokens.success
    ConversationStatus.Failed -> tokens.error
    ConversationStatus.Interrupted -> tokens.warning
}

@Composable
private fun ChatSurface(
    state: RuntimeState,
    localServerState: LocalServerState,
    conversation: ConversationRecord?,
    project: ProjectContext?,
    operationError: String?,
    operationBusy: Boolean,
    onSendText: (String, ConversationRecord, ProjectContext?) -> Unit,
    padding: PaddingValues,
) {
    var input by rememberSaveable { mutableStateOf("") }
    val remoteReady = state.server is ServerConnection.Connected &&
        state.enrollment == EnrollmentReadiness.Ready
    val localReady = localServerState.phase == LocalServerPhase.READY
    val ready = conversation != null && (remoteReady || localReady)
    val tokens = LocalZaraTokens.current

    Column(
        modifier = Modifier
            .fillMaxSize()
            .padding(padding)
            .padding(horizontal = 16.dp),
    ) {
        project?.let { ProjectBreadcrumb(it) }
        Column(
            modifier = Modifier
                .fillMaxWidth()
                .weight(1f)
                .verticalScroll(rememberScrollState()),
        ) {
            if (conversation == null || conversation.turns.isEmpty()) {
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
                conversation.turns.forEachIndexed { index, turn ->
                    UserMessage(turn.userText)
                    Spacer(Modifier.size(12.dp))
                    when {
                        turn.assistantText != null ->
                            AssistantMessage(turn.assistantText, turn.success == true)
                        conversation.status == ConversationStatus.Running &&
                            index == conversation.turns.lastIndex ->
                            AssistantPendingMessage()
                    }
                    if (index != conversation.turns.lastIndex) {
                        Spacer(Modifier.size(18.dp))
                    }
                }
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
                if (message.isNotEmpty() && conversation != null) {
                    input = ""
                    onSendText(message, conversation, project)
                }
            },
        )
        Text(
            when {
                remoteReady -> "REMOTE  •  AUTHENTICATED  •  SYMBOLIC"
                localReady -> "LOCAL  •  SYMBOLIC  •  PRIVATE"
                else -> "LOCAL RUNTIME STARTING"
            },
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
private fun AssistantPendingMessage() {
    val tokens = LocalZaraTokens.current
    Surface(
        color = tokens.surface,
        border = BorderStroke(1.dp, tokens.accentCyan),
        shape = MaterialTheme.shapes.large,
        modifier = Modifier.fillMaxWidth(),
    ) {
        Row(
            modifier = Modifier.padding(16.dp),
            verticalAlignment = Alignment.CenterVertically,
        ) {
            StatusDot(tokens.accentCyan)
            Text(
                "Working…",
                modifier = Modifier.padding(start = 10.dp),
                color = tokens.textMuted,
                fontFamily = FontFamily.Monospace,
                style = MaterialTheme.typography.labelMedium,
            )
        }
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
                if (ready) "Ask anything…" else "Open Settings → Connection",
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
                MutedNotice("Voice becomes available after an authenticated session connects in Settings → Connection.")
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
private fun ConnectionControls(
    state: RuntimeState,
    operationBusy: Boolean,
    onConnect: (String) -> Unit,
) {
    var endpoint by rememberSaveable {
        mutableStateOf(state.configuredProfile?.endpoint.orEmpty())
    }
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
            MutedNotice("Complete the identity and server trust steps above first.")
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
}

@Composable
private fun SettingsSurface(
    section: AppRoute,
    microphonePermissionGranted: Boolean,
    onRequestMicrophonePermission: () -> Unit,
    onConnect: (String) -> Unit,
    state: RuntimeState,
    localServerState: LocalServerState,
    updateState: UpdateState,
    runtimeMode: RuntimeMode,
    localEmbedding: LocalEmbeddingConfiguration,
    localAiState: LocalAiState,
    localModels: List<LocalModelSpec>,
    localModelBusy: Boolean,
    selectedTheme: ZaraTheme,
    enrollmentPublicKey: String?,
    pinnedServerPublicKey: String?,
    operationError: String?,
    operationBusy: Boolean,
    onCreateIdentity: () -> Unit,
    onPinServer: (String) -> Unit,
    onReplaceServerPin: (String) -> Unit,
    onRequestAssistantRole: () -> Unit,
    onCheckForUpdate: () -> Unit,
    onSelectUpdate: (String) -> Unit,
    onDownloadUpdate: () -> Unit,
    onInstallUpdate: () -> Unit,
    onSelectRuntimeMode: (RuntimeMode) -> Unit,
    onSetLocalEmbeddingEnabled: (Boolean) -> Unit,
    onImportLocalModel: (String, String, LocalModelQuantization, Int, LocalModelBackend) -> Unit,
    onSelectLocalModel: (String, String) -> Unit,
    onUnloadLocalModel: () -> Unit,
    onNavigateSettings: (AppRoute) -> Unit,
    padding: PaddingValues,
) {
    var serverPin by rememberSaveable { mutableStateOf("") }
    var replacementServerPin by rememberSaveable { mutableStateOf("") }
    var showServerPinReplacement by rememberSaveable { mutableStateOf(false) }
    var showAssistantHelp by rememberSaveable { mutableStateOf(false) }
    val tokens = LocalZaraTokens.current

    ScreenBody(padding) {
        if (section != AppRoute.Settings) {
            TextButton(onClick = { onNavigateSettings(AppRoute.Settings) }) {
                Text("‹ Settings")
            }
        }
        ScreenTitle(
            if (section == AppRoute.Settings) "Settings" else section.label,
            if (section == AppRoute.Settings) "Private, explicit, device-first controls" else "Settings",
        )
        when (section) {
            AppRoute.Settings -> {
                SettingsOverviewContent(
                    runtimeMode = runtimeMode,
                    localAiState = localAiState,
                    localModels = localModels,
                    runtimeState = state,
                    microphonePermissionGranted = microphonePermissionGranted,
                    selectedTheme = selectedTheme,
                    updateState = updateState,
                    onNavigate = onNavigateSettings,
                )
            }
            AppRoute.Runtime -> {
                SectionCard("LOCAL RUNTIME") {
                    KeyValueRow("state", localServerState.phase.name.lowercase())
                    KeyValueRow("generation", localServerState.generation.toString())
                    KeyValueRow("knowledge sources", localServerState.loadedSources.size.toString())
                    MutedNotice("The symbolic runtime is app-private and works with no account or network.")
                    localServerState.failure?.let { ErrorBanner(it) }
                }
                SectionCard("CHAT ROUTING") {
                    RuntimeMode.entries.forEach { mode ->
                        Row(
                            modifier = Modifier
                                .fillMaxWidth()
                                .selectable(
                                    selected = mode == runtimeMode,
                                    onClick = { onSelectRuntimeMode(mode) },
                                )
                                .padding(vertical = 4.dp),
                            verticalAlignment = Alignment.CenterVertically,
                        ) {
                            RadioButton(
                                selected = mode == runtimeMode,
                                onClick = { onSelectRuntimeMode(mode) },
                            )
                            Column(modifier = Modifier.padding(start = 8.dp)) {
                                Text(
                                    mode.name,
                                    color = if (mode == runtimeMode) tokens.text else tokens.textMuted,
                                )
                                Text(
                                    when (mode) {
                                        RuntimeMode.Auto -> "Prefer authenticated remote; fall back to local"
                                        RuntimeMode.Local -> "Never send this turn to the network"
                                        RuntimeMode.Remote -> "Require an authenticated remote session"
                                    },
                                    color = tokens.textMuted,
                                    style = MaterialTheme.typography.bodySmall,
                                )
                            }
                        }
                    }
                }
                LocalModelSettingsCard(
                    state = localAiState,
                    models = localModels,
                    busy = localModelBusy,
                    onImport = onImportLocalModel,
                    onSelect = onSelectLocalModel,
                    onUnload = onUnloadLocalModel,
                )
                SectionCard("LOCAL EMBEDDINGS") {
                    Row(
                        modifier = Modifier.fillMaxWidth(),
                        verticalAlignment = Alignment.CenterVertically,
                    ) {
                        Column(modifier = Modifier.weight(1f)) {
                            Text("Semantic indexing", color = tokens.text)
                            Text(
                                "On-device only",
                                color = tokens.textMuted,
                                style = MaterialTheme.typography.bodySmall,
                            )
                        }
                        Switch(
                            checked = localEmbedding.enabled,
                            onCheckedChange = onSetLocalEmbeddingEnabled,
                        )
                    }
                    KeyValueRow("model", localEmbedding.modelVersion)
                    KeyValueRow("dimensions", localEmbedding.dimensions.toString())
                    MutedNotice("Turning this off returns no vectors and prevents local semantic indexing.")
                }
            }
            AppRoute.Permissions -> {
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
                SectionCard("MICROPHONE") {
                    KeyValueRow("permission", if (microphonePermissionGranted) "granted" else "required")
                    if (!microphonePermissionGranted) {
                        PrimaryAction("Grant microphone permission", !operationBusy, onRequestMicrophonePermission)
                    }
                    MutedNotice("Permission allows voice capture; opening this tab does not start listening.")
                }
            }
            AppRoute.Connection -> {
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
                        EnrollmentReadiness.Ready -> {
                            MutedNotice("Client identity and server pin are ready. Server-side enrollment is still required.")
                            pinnedServerPublicKey?.let { publicKey ->
                                Text("SAVED SERVER PUBLIC KEY", color = tokens.accentCyan, style = MaterialTheme.typography.labelSmall)
                                SelectionContainer {
                                    Text(
                                        publicKey,
                                        modifier = Modifier.fillMaxWidth().padding(top = 6.dp),
                                        color = tokens.text,
                                        fontFamily = FontFamily.Monospace,
                                        style = MaterialTheme.typography.bodySmall,
                                    )
                                }
                            }
                            TextButton(onClick = { showServerPinReplacement = !showServerPinReplacement }) {
                                Text(if (showServerPinReplacement) "Cancel server key change" else "Change trusted server key")
                            }
                            if (showServerPinReplacement) {
                                MutedNotice("Verify the new key with your server. Changing trust disconnects the old session; reconnect below afterward.")
                                OutlinedTextField(
                                    value = replacementServerPin,
                                    onValueChange = { replacementServerPin = it },
                                    modifier = Modifier.fillMaxWidth(),
                                    label = { Text("New server CURVE public key") },
                                    enabled = !operationBusy,
                                    singleLine = true,
                                    colors = fieldColors(),
                                )
                                PrimaryAction(
                                    "Replace trusted server key",
                                    replacementServerPin.length == 40 && !operationBusy,
                                ) { onReplaceServerPin(replacementServerPin) }
                            }
                        }
                        EnrollmentReadiness.Corrupt ->
                            ErrorBanner("Enrollment storage is corrupt; connection is disabled.")
                    }
                }

                state.configuredProfile?.endpoint?.let {
                    SectionCard("REMOTE PROFILE") {
                        KeyValueRow("endpoint", it)
                    }
                }
                ConnectionControls(state, operationBusy, onConnect)
            }
            AppRoute.Updates -> {
                SectionCard("SELF UPDATE") {
                    KeyValueRow("installed", BuildConfig.VERSION_NAME)
                    KeyValueRow("source", BuildConfig.SOURCE_SHA.take(12))
                    KeyValueRow("status", updateState.phase.name.lowercase())

                    if (updateState.choices.isNotEmpty()) {
                        Text(
                            "VERSION / CHANNEL",
                            color = tokens.accentCyan,
                            style = MaterialTheme.typography.labelSmall,
                        )
                        updateState.choices.forEach { release ->
                            val selected = updateState.selectedId == release.selectionId
                            SecondaryAction(
                                label = if (selected) "✓ ${release.displayName}" else release.displayName,
                                enabled = updateState.phase !in setOf(
                                    UpdatePhase.CHECKING,
                                    UpdatePhase.DOWNLOADING,
                                    UpdatePhase.INSTALLING,
                                ),
                            ) { onSelectUpdate(release.selectionId) }
                        }
                    }

                    updateState.release?.let { release ->
                        KeyValueRow("selected", release.displayName)
                        KeyValueRow("selected source", release.sourceSha.take(12))
                    }
                    updateState.progressPercent?.let { progress ->
                        KeyValueRow("download", "$progress%")
                    }
                    updateState.message?.let { MutedNotice(it) }
                    when (updateState.phase) {
                        UpdatePhase.AVAILABLE -> PrimaryAction("Download verified APK", true, onDownloadUpdate)
                        UpdatePhase.READY -> PrimaryAction("Install update", true, onInstallUpdate)
                        UpdatePhase.CHECKING, UpdatePhase.DOWNLOADING, UpdatePhase.INSTALLING ->
                            PrimaryAction("Working…", false) { }
                        else -> PrimaryAction("Refresh versions", true, onCheckForUpdate)
                    }
                    MutedNotice(
                        "Master (fastest green) tracks the newest fully green master APK through the mutable android-latest channel. " +
                            "Versioned entries are immutable releases. Zara verifies the exact source SHA and SHA-256 before Android installation."
                    )
                }
            }
            else -> error("Not a settings form: $section")
        }
        operationError?.let { failure ->
            ErrorBanner(failure)
            if (section == AppRoute.Connection && failure == "server_hello_timeout") {
                MutedNotice("The server did not accept the authenticated hello. Compare the saved server key above and confirm this client key is enrolled.")
            }
        }
    }
}

@Composable
private fun DiagnosticsSurface(
    state: RuntimeState,
    sourceSha: String,
    localServerState: LocalServerState,
    voiceStreamState: VoiceStreamState?,
    voiceStreamFailure: String?,
    operationError: String?,
    onCopyDiagnostics: () -> Unit,
    onShareDiagnostics: () -> Unit,
    onClearDiagnostics: () -> Unit,
    padding: PaddingValues,
) {
    ScreenBody(padding) {
        ScreenTitle("Diagnostics", "Bounded runtime state")
        SectionCard("BUILD") {
            KeyValueRow("source", sourceSha.take(12))
        }
        SectionCard("RUNTIME") {
            KeyValueRow("local server", localServerState.phase.name.lowercase())
            KeyValueRow("local generation", localServerState.generation.toString())
            KeyValueRow("local sources", localServerState.loadedSources.size.toString())
            KeyValueRow("local failure", localServerState.failure ?: "none")
            KeyValueRow("connection", connectionLabel(state.server))
            KeyValueRow("generation", state.generation.toString())
            KeyValueRow("session", state.sessionId ?: "none")
            KeyValueRow("conversation", state.selectedConversationId ?: "none")
            KeyValueRow("enrollment", enrollmentLabel(state.enrollment))
            KeyValueRow("assistant role", assistantRoleLabel(state.assistantRole))
        }
        SectionCard("LOCAL LOG") {
            MutedNotice(
                "The log is stored only in app-private storage and records runtime stages, bounded exception chains, build/source identity, and model/runtime state. Prompt text, credentials, private keys, and model bytes are not logged."
            )
            PrimaryAction("Copy diagnostics", true, onCopyDiagnostics)
            SecondaryAction("Share diagnostics", true, onShareDiagnostics)
            SecondaryAction("Clear diagnostics", true, onClearDiagnostics)
            MutedNotice("For support: tap Copy diagnostics, return to ChatGPT, and paste the block into this chat.")
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
internal fun ScreenBody(
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
internal fun ScreenTitle(title: String, subtitle: String) {
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
internal fun SectionCard(title: String, content: @Composable () -> Unit) {
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
internal fun KeyValueRow(label: String, value: String) {
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
internal fun PrimaryAction(label: String, enabled: Boolean, onClick: () -> Unit) {
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
internal fun SecondaryAction(label: String, enabled: Boolean, onClick: () -> Unit) {
    val tokens = LocalZaraTokens.current
    TextButton(onClick = onClick, enabled = enabled) {
        Text(label, color = tokens.secondary)
    }
}

@Composable
internal fun MutedNotice(text: String) {
    val tokens = LocalZaraTokens.current
    Text(text, color = tokens.textMuted, style = MaterialTheme.typography.bodySmall)
}

@Composable
internal fun ErrorBanner(text: String) {
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
