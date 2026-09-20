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
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.WindowInsets
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.statusBars
import androidx.compose.foundation.layout.windowInsetsPadding
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp

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
    projectState: ProjectContextState,
    onSelectTheme: (ZaraTheme) -> Unit,
    onSelectRuntimeMode: (RuntimeMode) -> Unit,
    onSetLocalEmbeddingEnabled: (Boolean) -> Unit,
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
    Box(Modifier.fillMaxSize()) {
        ZaraApp(
            runtimeState = runtimeState,
            sourceSha = sourceSha,
            enrollmentPublicKey = enrollmentPublicKey,
            pinnedServerPublicKey = pinnedServerPublicKey,
            conversationState = conversationState,
            operationError = operationError,
            operationBusy = operationBusy,
            microphonePermissionGranted = microphonePermissionGranted,
            voiceState = voiceState,
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
            projectState = projectState,
            onSelectTheme = onSelectTheme,
            onSelectRuntimeMode = onSelectRuntimeMode,
            onSetLocalEmbeddingEnabled = onSetLocalEmbeddingEnabled,
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
            onDismissChangelog = onDismissChangelog,
        )

        if (runtimeState.enrollment != EnrollmentReadiness.Ready) {
            PairingSetupAction(
                operationBusy = operationBusy,
                onScanPairingQr = onScanPairingQr,
                modifier = Modifier
                    .align(Alignment.TopEnd)
                    .windowInsetsPadding(WindowInsets.statusBars)
                    .padding(top = 62.dp, end = 16.dp),
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
