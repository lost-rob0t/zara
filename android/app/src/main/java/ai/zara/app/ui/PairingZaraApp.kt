package ai.zara.app.ui

import ai.zara.app.conversations.ConversationRecord
import ai.zara.app.conversations.ConversationState
import ai.zara.app.projects.ProjectContext
import ai.zara.app.projects.ProjectContextState
import ai.zara.app.prolog.LocalEmbeddingConfiguration
import ai.zara.app.prolog.PrologSource
import ai.zara.app.runtime.EnrollmentReadiness
import ai.zara.app.runtime.LocalQueryResult
import ai.zara.app.runtime.LocalServerState
import ai.zara.app.runtime.RuntimeMode
import ai.zara.app.runtime.RuntimeState
import ai.zara.app.update.UpdateState
import ai.zara.app.voice.ManualVoiceState
import ai.zara.app.voice.VoiceStreamState
import ai.zara.ui.theme.ZaraTheme
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.WindowInsets
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.navigationBars
import androidx.compose.foundation.layout.windowInsetsPadding
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import ai.zara.app.BuildConfig
import ai.zara.app.conversations.ConversationStatus
import ai.zara.app.localai.LocalAiState
import ai.zara.app.localai.LocalModelBackend
import ai.zara.app.localai.LocalModelQuantization
import ai.zara.app.localai.LocalModelSpec
import ai.zara.app.model.CloudModelConfig
import ai.zara.app.model.CloudModelState
import ai.zara.app.runtime.AssistantRole
import ai.zara.app.runtime.LocalServerPhase
import ai.zara.app.runtime.ServerConnection
import ai.zara.app.update.UpdatePhase
import ai.zara.ui.theme.ZaraSemanticTokens
import ai.zara.ui.theme.themeTokens
import androidx.activity.compose.BackHandler
import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.Canvas
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.isSystemInDarkTheme
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.BoxWithConstraints
import androidx.compose.foundation.layout.consumeWindowInsets
import androidx.compose.foundation.layout.imePadding
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.heightIn
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.statusBars
import androidx.compose.foundation.layout.width
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
import androidx.compose.material3.Switch
import androidx.compose.material3.darkColorScheme
import androidx.compose.material3.rememberDrawerState
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
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.semantics.clearAndSetSemantics
import androidx.compose.ui.semantics.contentDescription
import androidx.compose.ui.semantics.semantics
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.sp
import kotlinx.coroutines.launch

/**
 * Pairing-aware Android shell adapter.
 *
 * The core [ZaraApp] remains unaware of platform scanner APIs. Android supplies
 * one explicit setup action while enrollment is incomplete, and the action
 * disappears as soon as canonical runtime enrollment reaches Ready.
 */
@Composable
fun ZaraApp(
    runtimeState: RuntimeState,
    sourceSha: String,
    enrollmentPublicKey: String?,
    pinnedServerPublicKey: String?,
    conversationState: ConversationState,
    operationError: String?,
    turnFailure: TurnFailure?,
    operationBusy: Boolean,
    microphonePermissionGranted: Boolean,
    voiceState: ManualVoiceState,
    localVoiceActive: Boolean,
    localVoiceStatus: String?,
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
    cloudModelState: CloudModelState,
    cloudModelBusy: Boolean,
    projectState: ProjectContextState,
    onSelectTheme: (ZaraTheme) -> Unit,
    onSelectRuntimeMode: (RuntimeMode) -> Unit,
    onSetLocalEmbeddingEnabled: (Boolean) -> Unit,
    onImportLocalModel: (String, String, LocalModelQuantization, Int, LocalModelBackend) -> Unit,
    onSelectLocalModel: (String, String) -> Unit,
    onUnloadLocalModel: () -> Unit,
    onConfigureCloudModel: (CloudModelConfig, String?) -> Unit,
    onClearCloudModelApiKey: () -> Unit,
    onScanPairingQr: () -> Unit,
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
    onRetryTurn: (String) -> Unit,
    onReconnectRemote: () -> Unit,
    onOpenDiagnostics: () -> Unit,
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
    onExportDiagnostics: () -> String,
    onDismissChangelog: () -> Unit,
) {
    Column(Modifier.fillMaxSize()) {
        Box(Modifier.weight(1f).fillMaxWidth()) {
            ZaraApp(
                runtimeState = runtimeState,
                sourceSha = sourceSha,
                enrollmentPublicKey = enrollmentPublicKey,
                pinnedServerPublicKey = pinnedServerPublicKey,
                conversationState = conversationState,
                operationError = operationError,
                turnFailure = turnFailure,
                operationBusy = operationBusy,
                microphonePermissionGranted = microphonePermissionGranted,
                voiceState = voiceState,
                localVoiceActive = localVoiceActive,
                localVoiceStatus = localVoiceStatus,
                voiceStreamState = voiceStreamState,
                voiceStreamFailure = voiceStreamFailure,
                selectedTheme = selectedTheme,
                localServerState = localServerState,
                prologSources = prologSources,
                prologQueryResult = prologQueryResult,
                updateState = updateState,
                changelogVersion = changelogVersion,
                changelogText = changelogText,
                showChangelog = showChangelog,
                runtimeMode = runtimeMode,
                localEmbedding = localEmbedding,
                localAiState = localAiState,
                localModels = localModels,
                localModelBusy = localModelBusy,
                cloudModelState = cloudModelState,
                cloudModelBusy = cloudModelBusy,
                projectState = projectState,
                onSelectTheme = onSelectTheme,
                onSelectRuntimeMode = onSelectRuntimeMode,
                onSetLocalEmbeddingEnabled = onSetLocalEmbeddingEnabled,
                onImportLocalModel = onImportLocalModel,
                onSelectLocalModel = onSelectLocalModel,
                onUnloadLocalModel = onUnloadLocalModel,
                onConfigureCloudModel = onConfigureCloudModel,
                onClearCloudModelApiKey = onClearCloudModelApiKey,
                onCreateIdentity = onCreateIdentity,
                onPinServer = onPinServer,
                onReplaceServerPin = onReplaceServerPin,
                onConnect = onConnect,
                onNewConversation = onNewConversation,
                onSelectConversation = onSelectConversation,
                onToggleConversationPinned = onToggleConversationPinned,
                onRenameConversation = onRenameConversation,
                onMoveConversationToProject = onMoveConversationToProject,
                onSendText = onSendText,
                onRetryTurn = onRetryTurn,
                onReconnectRemote = onReconnectRemote,
                onOpenDiagnostics = onOpenDiagnostics,
                onCreateProject = onCreateProject,
                onSelectProject = onSelectProject,
                onRequestMicrophonePermission = onRequestMicrophonePermission,
                onRequestAssistantRole = onRequestAssistantRole,
                onStartVoice = onStartVoice,
                onStopVoice = onStopVoice,
                onCancelVoice = onCancelVoice,
                onSavePrologSource = onSavePrologSource,
                onReloadLocalServer = onReloadLocalServer,
                onRunPrologQuery = onRunPrologQuery,
                onRenamePrologSource = onRenamePrologSource,
                onDeletePrologSource = onDeletePrologSource,
                onImportPrologWorkspace = onImportPrologWorkspace,
                onExportPrologWorkspace = onExportPrologWorkspace,
                onCheckForUpdate = onCheckForUpdate,
                onSelectUpdate = onSelectUpdate,
                onDownloadUpdate = onDownloadUpdate,
                onInstallUpdate = onInstallUpdate,
                onCopyDiagnostics = onCopyDiagnostics,
                onShareDiagnostics = onShareDiagnostics,
                onClearDiagnostics = onClearDiagnostics,
                onExportDiagnostics = onExportDiagnostics,
                onDismissChangelog = onDismissChangelog,
            )
        }

        if (runtimeState.enrollment != EnrollmentReadiness.Ready) {
            PairingSetupAction(
                operationBusy = operationBusy,
                onScanPairingQr = onScanPairingQr,
                modifier = Modifier
                    .fillMaxWidth()
                    .windowInsetsPadding(WindowInsets.navigationBars),
            )
        }
    }
}

@Composable
internal fun PairingSetupAction(
    operationBusy: Boolean,
    onScanPairingQr: () -> Unit,
    modifier: Modifier = Modifier,
) {
    Surface(modifier = modifier, tonalElevation = 8.dp, shadowElevation = 8.dp) {
        TextButton(
            onClick = onScanPairingQr,
            enabled = !operationBusy,
            contentPadding = PaddingValues(horizontal = 14.dp, vertical = 6.dp),
        ) {
            Text("Scan pairing QR")
        }
    }
}
