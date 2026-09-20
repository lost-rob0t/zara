package ai.zara.app

import ai.zara.app.conversations.ConversationRecord
import ai.zara.app.conversations.ConversationState
import ai.zara.app.conversations.ConversationStore
import ai.zara.app.prolog.AndroidPureSymbolicConversationFactory
import ai.zara.app.projects.ProjectContextStore
import ai.zara.app.ui.ConversationExecutionPolicy
import ai.zara.app.ui.ConversationExecutionPolicyController
import ai.zara.app.ui.ConversationExecutionPolicyStore
import ai.zara.app.ui.LocalEmbeddingPreferenceStore
import ai.zara.app.ui.RuntimeModePreferenceStore
import ai.zara.app.ui.ThemePreferenceStore
import ai.zara.app.projects.ProjectContext
import ai.zara.app.runtime.ServerConnection
import ai.zara.app.telemetry.ZaraFailures
import ai.zara.app.telemetry.ZaraOperation
import ai.zara.app.ui.TurnFailure
import ai.zara.app.ui.TurnFailures
import ai.zara.app.ui.UiOperationFailure
import ai.zara.app.ui.ZaraApp
import ai.zara.app.update.Changelog
import ai.zara.app.update.ChangelogSeenStore
import ai.zara.app.voice.ManualVoiceState
import ai.zara.ui.theme.ZaraTheme
import android.Manifest
import android.content.ClipData
import android.content.ClipboardManager
import android.content.Intent
import android.content.pm.PackageManager
import android.graphics.Color
import android.os.Bundle
import android.widget.Toast
import androidx.activity.ComponentActivity
import androidx.activity.SystemBarStyle
import androidx.activity.compose.setContent
import androidx.activity.enableEdgeToEdge
import androidx.activity.result.contract.ActivityResultContracts
import androidx.compose.foundation.isSystemInDarkTheme
import androidx.compose.runtime.SideEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.setValue
import androidx.core.content.ContextCompat
import java.io.File

class MainActivity : ComponentActivity() {
    private lateinit var appSession: AndroidAppSession
    private lateinit var conversationStore: ConversationStore
    private var conversationState by mutableStateOf(ConversationState())
    private var microphonePermissionGranted by mutableStateOf(false)
    private var operationError by mutableStateOf<String?>(null)
    private var turnFailure by mutableStateOf<TurnFailure?>(null)
    private var voiceState by mutableStateOf<ManualVoiceState>(ManualVoiceState.Idle)

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        appSession = (application as ZaraApplication).appSession
        val updateManager = (application as ZaraApplication).updateManager
        val changelogSeenStore = ChangelogSeenStore(this)
        val currentChangelog = Changelog.load(this, BuildConfig.VERSION_NAME)
        var showCurrentChangelog by mutableStateOf(
            changelogSeenStore.shouldShow(BuildConfig.VERSION_NAME, currentChangelog)
        )
        microphonePermissionGranted = hasMicrophonePermission()
        voiceState = appSession.voiceState()

        var runtimeState by mutableStateOf(appSession.state())
        var enrollmentPublicKey by mutableStateOf(appSession.enrollmentPublicKeyZ85())
        var pinnedServerPublicKey by mutableStateOf(appSession.pinnedServerPublicKeyZ85())
        var operationBusy by mutableStateOf(false)
        var voiceStreamState by mutableStateOf(appSession.voiceStreamState())
        var voiceStreamFailure by mutableStateOf(appSession.voiceStreamFailure())
        var localServerState by mutableStateOf(appSession.localServerState())
        var prologSources by mutableStateOf(appSession.prologSources())
        var prologQueryResult by mutableStateOf<ai.zara.app.runtime.LocalQueryResult?>(null)
        var updateState by mutableStateOf(updateManager.state())
        val themePreferenceStore = ThemePreferenceStore(File(filesDir, "theme.bin"))
        var selectedTheme by mutableStateOf(themePreferenceStore.load())
        val runtimeModeStore = RuntimeModePreferenceStore(File(filesDir, "runtime-mode.bin"))
        var runtimeMode by mutableStateOf(runtimeModeStore.load())
        val embeddingPreferenceStore = LocalEmbeddingPreferenceStore(File(filesDir, "local-embedding.bin"))
        var localEmbedding by mutableStateOf(embeddingPreferenceStore.load())
        val projectStore = ProjectContextStore(File(filesDir, "projects.bin"))
        var projectState by mutableStateOf(projectStore.state())
        conversationStore = ConversationStore(File(filesDir, "conversations.bin"))
        conversationState = conversationStore.state()
        val executionPolicyController = ConversationExecutionPolicyController(
            store = ConversationExecutionPolicyStore(
                File(filesDir, "conversation-execution-policy.bin"),
            ),
            pureSymbolicSubmit = AndroidPureSymbolicConversationFactory.create(appSession)::submit,
        )
        if (conversationState.loadFailure == null && conversationState.selectedConversation == null) {
            try {
                conversationStore.create(projectState.selectedProjectId)
                conversationState = conversationStore.state()
            } catch (error: Exception) {
                operationError = UiOperationFailure.summarize(error)
            }
        }
        appSession.setRuntimeMode(runtimeMode)

        val microphonePermission = registerForActivityResult(
            ActivityResultContracts.RequestPermission()
        ) { granted ->
            reconcileMicrophonePermission(granted)
            if (!granted) operationError = "Microphone permission denied"
        }
        val assistantRoleRequest = registerForActivityResult(
            ActivityResultContracts.StartActivityForResult()
        ) {
            operationBusy = false
            try {
                appSession.completeAssistantRoleRequest()
            } catch (error: Exception) {
                operationError = UiOperationFailure.summarize(error)
            }
        }

        appSession.setStateObserver { state ->
            runOnUiThread { runtimeState = state }
        }
        appSession.setVoiceStreamObserver { streamState, failure ->
            runOnUiThread {
                voiceStreamState = streamState
                voiceStreamFailure = failure
            }
        }
        appSession.setLocalServerObserver { state ->
            runOnUiThread { localServerState = state }
        }
        updateManager.setObserver { state ->
            runOnUiThread { updateState = state }
        }
        appSession.assessAssistantRole()

        setContent {
            val systemDark = isSystemInDarkTheme()
            val resolvedSystemBarDark = when (selectedTheme) {
                ZaraTheme.System -> systemDark
                ZaraTheme.Light -> false
                else -> true
            }
            SideEffect {
                val style = if (resolvedSystemBarDark) {
                    SystemBarStyle.dark(Color.TRANSPARENT)
                } else {
                    SystemBarStyle.light(Color.TRANSPARENT, Color.TRANSPARENT)
                }
                enableEdgeToEdge(statusBarStyle = style, navigationBarStyle = style)
            }
            val submitChatText: (String, ConversationRecord, ProjectContext?) -> Unit = { text, conversation, project ->

                    operationError = null
                    operationBusy = true
                    val conversationId = conversation.id
                    try {
                        conversationState = conversationStore.beginTurn(conversationId, text)
                        val requestedPolicy = when (text.trim().lowercase()) {
                            "/symbolic on" -> ConversationExecutionPolicy.PURE_SYMBOLIC
                            "/symbolic off" -> ConversationExecutionPolicy.STANDARD
                            else -> null
                        }
                        if (requestedPolicy != null) {
                            executionPolicyController.select(requestedPolicy)
                            val enabled = requestedPolicy == ConversationExecutionPolicy.PURE_SYMBOLIC
                            conversationState = conversationStore.completeTurn(
                                conversationId = conversationId,
                                assistantText = if (enabled) {
                                    "Pure symbolic mode enabled. max_model_calls=0 and max_provider_calls=0."
                                } else {
                                    "Pure symbolic mode disabled. Standard execution policy restored."
                                },
                                success = true,
                                remoteConversationId = null,
                            )
                            operationBusy = false
                        } else {
                            val future = executionPolicyController.submit(
                                text = text,
                                conversationId = conversation.localConversationId,
                                standardTurn = {
                                    if (project == null) {
                                        appSession.submitText(
                                            text = text,
                                            localConversationId = conversation.localConversationId,
                                            remoteConversationId = conversation.remoteConversationId,
                                        )
                                    } else {
                                        appSession.submitProjectText(
                                            text = text,
                                            projectId = project.id,
                                            conversationId = conversation.remoteConversationId,
                                            localConversationId = conversation.localConversationId,
                                        )
                                    }
                                },
                            )
                            future.whenComplete { result, error ->
                                runOnUiThread {
                                    operationBusy = false
                                    if (error != null) {
                                        recordTurnFailure(conversationId, error)
                                    } else if (result != null) {
                                        turnFailure = null
                                        val remoteConversationId = result.conversationId
                                            ?.takeUnless { it.startsWith("local-") }
                                        try {
                                            conversationState = conversationStore.completeTurn(
                                                conversationId = conversationId,
                                                assistantText = result.text,
                                                success = result.success,
                                                remoteConversationId = remoteConversationId,
                                            )
                                            if (project != null && remoteConversationId != null &&
                                                projectState.loadFailure == null
                                            ) {
                                                projectState = projectStore.bindConversation(
                                                    project.id,
                                                    remoteConversationId,
                                                )
                                            }
                                        } catch (storeError: Exception) {
                                            operationError = UiOperationFailure.summarize(storeError)
                                        }
                                    }
                                }
                            }
                        }
                    } catch (error: Exception) {
                        operationBusy = false
                        recordTurnFailure(conversationId, error)
                    }
            }

            ZaraApp(
                runtimeState = runtimeState,
                sourceSha = BuildConfig.SOURCE_SHA,
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
                changelogVersion = BuildConfig.VERSION_NAME,
                changelogText = currentChangelog,
                showChangelog = showCurrentChangelog,
                runtimeMode = runtimeMode,
                localEmbedding = localEmbedding,
                projectState = projectState,
                onSelectTheme = { theme ->
                    selectedTheme = theme
                    themePreferenceStore.save(theme)
                },
                onSelectRuntimeMode = { mode ->
                    runtimeMode = mode
                    runtimeModeStore.save(mode)
                    appSession.setRuntimeMode(mode)
                },
                onSetLocalEmbeddingEnabled = { enabled ->
                    localEmbedding = localEmbedding.copy(enabled = enabled)
                    embeddingPreferenceStore.save(localEmbedding)
                },
                onCreateIdentity = {
                    operationError = null
                    try {
                        enrollmentPublicKey = appSession.createIdentity()
                    } catch (error: Exception) {
                        operationError = UiOperationFailure.summarize(error)
                    }
                },
                onPinServer = { publicKey ->
                    operationError = null
                    try {
                        appSession.pinServer(publicKey)
                        enrollmentPublicKey = appSession.enrollmentPublicKeyZ85()
                        pinnedServerPublicKey = appSession.pinnedServerPublicKeyZ85()
                    } catch (error: Exception) {
                        operationError = UiOperationFailure.summarize(error)
                    }
                },
                onReplaceServerPin = { publicKey ->
                    operationError = null
                    try {
                        appSession.replaceServerPin(publicKey)
                        pinnedServerPublicKey = appSession.pinnedServerPublicKeyZ85()
                    } catch (error: Exception) {
                        operationError = UiOperationFailure.summarize(error)
                    }
                },
                onConnect = { endpoint ->
                    operationError = null
                    operationBusy = true
                    try {
                        appSession.connect(endpoint).whenComplete { _, error ->
                            runOnUiThread {
                                operationBusy = false
                                operationError = error?.let(UiOperationFailure::summarize)
                            }
                        }
                    } catch (error: Exception) {
                        operationBusy = false
                        operationError = UiOperationFailure.summarize(error)
                    }
                },
                onNewConversation = {
                    operationError = null
                    try {
                        conversationStore.create()
                        conversationState = conversationStore.state()
                        if (projectState.loadFailure == null) {
                            projectState = projectStore.select(null)
                        }
                    } catch (error: Exception) {
                        operationError = UiOperationFailure.summarize(error)
                    }
                },
                onSelectConversation = { conversationId ->
                    operationError = null
                    try {
                        conversationState = conversationStore.select(conversationId)
                        if (projectState.loadFailure == null) {
                            projectState = projectStore.select(
                                conversationState.selectedConversation?.projectId,
                            )
                        }
                    } catch (error: Exception) {
                        operationError = UiOperationFailure.summarize(error)
                    }
                },
                onToggleConversationPinned = { conversationId, pinned ->
                    operationError = null
                    try {
                        conversationState = conversationStore.setPinned(conversationId, pinned)
                    } catch (error: Exception) {
                        operationError = UiOperationFailure.summarize(error)
                    }
                },
                onRenameConversation = { conversationId, title ->
                    operationError = null
                    try {
                        conversationState = conversationStore.rename(conversationId, title)
                    } catch (error: Exception) {
                        operationError = UiOperationFailure.summarize(error)
                    }
                },
                onMoveConversationToProject = { conversationId, projectId ->
                    operationError = null
                    try {
                        if (projectId != null) {
                            require(projectState.project(projectId) != null) { "Unknown project: $projectId" }
                        }
                        conversationState = conversationStore.moveToProject(conversationId, projectId)
                        if (conversationState.selectedConversationId == conversationId &&
                            projectState.loadFailure == null
                        ) {
                            projectState = projectStore.select(projectId)
                        }
                    } catch (error: Exception) {
                        operationError = UiOperationFailure.summarize(error)
                    }
                },
                onSendText = { text, conversation, project -> submitChatText(text, conversation, project) },
                onCreateProject = { name ->
                    operationError = null
                    try {
                        val created = projectStore.create(name)
                        projectState = projectStore.select(created.id)
                        conversationState.selectedConversation?.let { conversation ->
                            conversationState = conversationStore.moveToProject(
                                conversation.id,
                                created.id,
                            )
                        }
                    } catch (error: Exception) {
                        operationError = UiOperationFailure.summarize(error)
                    }
                },
                onSelectProject = { projectId ->
                    operationError = null
                    try {
                        projectState = projectStore.select(projectId)
                        conversationState.selectedConversation?.let { conversation ->
                            conversationState = conversationStore.moveToProject(
                                conversation.id,
                                projectId,
                            )
                        }
                    } catch (error: Exception) {
                        operationError = UiOperationFailure.summarize(error)
                    }
                },
                onRequestMicrophonePermission = {
                    operationError = null
                    microphonePermission.launch(Manifest.permission.RECORD_AUDIO)
                },
                onRequestAssistantRole = {
                    operationError = null
                    val intent = appSession.assistantRoleRequestIntent()
                    if (intent == null) {
                        appSession.assessAssistantRole()
                    } else {
                        operationBusy = true
                        assistantRoleRequest.launch(intent)
                    }
                },
                onStartVoice = {
                    operationError = null
                    operationBusy = true
                    appSession.pressToTalk(microphonePermissionGranted).whenComplete { _, error ->
                        runOnUiThread {
                            operationBusy = false
                            operationError = error?.let(UiOperationFailure::summarize)
                            voiceState = appSession.voiceState()
                        }
                    }
                },
                onStopVoice = {
                    operationError = null
                    operationBusy = true
                    appSession.releasePushToTalk().whenComplete { _, error ->
                        runOnUiThread {
                            operationBusy = false
                            operationError = error?.let(UiOperationFailure::summarize)
                            voiceState = appSession.voiceState()
                        }
                    }
                },
                onCancelVoice = {
                    operationError = null
                    operationBusy = true
                    appSession.cancelPushToTalk().whenComplete { _, error ->
                        runOnUiThread {
                            operationBusy = false
                            operationError = error?.let(UiOperationFailure::summarize)
                            voiceState = appSession.voiceState()
                        }
                    }
                },
                onSavePrologSource = { name, source ->
                    operationError = null
                    operationBusy = true
                    appSession.savePrologSource(name, source).whenComplete { _, error ->
                        runOnUiThread {
                            operationBusy = false
                            operationError = error?.let(UiOperationFailure::summarize)
                            if (error == null) prologSources = appSession.prologSources()
                        }
                    }
                },
                onReloadLocalServer = {
                    operationError = null
                    operationBusy = true
                    appSession.reloadLocalServer().whenComplete { _, error ->
                        runOnUiThread {
                            operationBusy = false
                            operationError = error?.let(UiOperationFailure::summarize)
                        }
                    }
                },
                onRunPrologQuery = { query ->
                    operationError = null
                    operationBusy = true
                    appSession.queryLocalProlog(query).whenComplete { result, error ->
                        runOnUiThread {
                            operationBusy = false
                            operationError = error?.let(UiOperationFailure::summarize)
                            if (result != null) prologQueryResult = result
                        }
                    }
                },
                onRenamePrologSource = { from, to ->
                    operationError = null
                    operationBusy = true
                    appSession.renamePrologSource(from, to).whenComplete { result, error ->
                        runOnUiThread {
                            operationBusy = false
                            operationError = error?.let(UiOperationFailure::summarize)
                            if (result != null) prologSources = result
                        }
                    }
                },
                onDeletePrologSource = { name ->
                    operationError = null
                    operationBusy = true
                    appSession.deletePrologSource(name).whenComplete { result, error ->
                        runOnUiThread {
                            operationBusy = false
                            operationError = error?.let(UiOperationFailure::summarize)
                            if (result != null) prologSources = result
                        }
                    }
                },
                onImportPrologWorkspace = { bundle ->
                    operationError = null
                    operationBusy = true
                    appSession.importPrologWorkspace(bundle).whenComplete { result, error ->
                        runOnUiThread {
                            operationBusy = false
                            operationError = error?.let(UiOperationFailure::summarize)
                            if (result != null) prologSources = result
                        }
                    }
                },
                onExportPrologWorkspace = appSession::exportPrologWorkspace,
                onCheckForUpdate = {
                    operationError = null
                    updateManager.check().whenComplete { _, error ->
                        runOnUiThread {
                            operationError = error?.let(UiOperationFailure::summarize)
                        }
                    }
                },
                onSelectUpdate = { selectionId ->
                    operationError = null
                    updateManager.select(selectionId).whenComplete { _, error ->
                        runOnUiThread {
                            operationError = error?.let(UiOperationFailure::summarize)
                        }
                    }
                },
                onDownloadUpdate = {
                    operationError = null
                    updateManager.download().whenComplete { _, error ->
                        runOnUiThread {
                            operationError = error?.let(UiOperationFailure::summarize)
                        }
                    }
                },
                onInstallUpdate = {
                    operationError = updateManager.requestInstall().exceptionOrNull()
                        ?.let(UiOperationFailure::summarize)
                },
                turnFailure = turnFailure,
                onRetryTurn = { text ->
                    val selected = conversationState.selectedConversation
                    val project = selected?.projectId?.let(projectState::project)
                    if (selected != null) submitChatText(text, selected, project)
                },
                onReconnectRemote = {
                    appSession.state().configuredProfile?.let { profile ->
                        appSession.connect(profile.endpoint)
                    }
                },
                onOpenDiagnostics = ::copyDiagnostics,
                onCopyDiagnostics = ::copyDiagnostics,
                onExportDiagnostics = { appSession.exportDiagnostics() },
                onShareDiagnostics = ::shareDiagnostics,
                onClearDiagnostics = ::clearDiagnostics,
                onDismissChangelog = {
                    changelogSeenStore.markShown(BuildConfig.VERSION_NAME)
                    showCurrentChangelog = false
                },
            )
        }
    }

    override fun onResume() {
        super.onResume()
        if (!::appSession.isInitialized) return
        appSession.assessAssistantRole()
        reconcileMicrophonePermission(hasMicrophonePermission())
    }

    override fun onStop() {
        if (::appSession.isInitialized) {
            appSession.onHostStopped().whenComplete { _, error ->
                runOnUiThread {
                    if (error != null) operationError = UiOperationFailure.summarize(error)
                    voiceState = appSession.voiceState()
                }
            }
        }
        super.onStop()
    }

    override fun onDestroy() {
        if (::appSession.isInitialized) {
            appSession.setStateObserver(null)
            appSession.setVoiceStreamObserver(null)
            appSession.setLocalServerObserver(null)
            (application as ZaraApplication).updateManager.setObserver(null)
        }
        super.onDestroy()
    }

    private fun reconcileMicrophonePermission(granted: Boolean) {
        microphonePermissionGranted = granted
        appSession.onMicrophonePermissionChanged(granted).whenComplete { _, error ->
            runOnUiThread {
                if (error != null) operationError = UiOperationFailure.summarize(error)
                voiceState = appSession.voiceState()
            }
        }
    }

    private fun recordTurnFailure(conversationId: String, error: Throwable) {
        val classified = ZaraFailures.classify(error, ZaraOperation.SUBMIT)
        val connected = appSession.state().server is ServerConnection.Connected
        val candidate = TurnFailures.from(
            failure = classified,
            transportConnected = connected,
            incidentId = appSession.diagnosticsIncidentId(),
        )
        turnFailure = TurnFailures.mostSpecific(turnFailure, candidate)
        val summary = TurnFailures.renderSummary(turnFailure ?: candidate)
        try {
            val selected = conversationStore.state().conversation(conversationId)
            if (selected?.status == ai.zara.app.conversations.ConversationStatus.Running) {
                conversationState = conversationStore.failTurn(conversationId, summary)
            }
        } catch (_: Exception) {
        }
    }

    private fun copyDiagnostics() {
        val text = appSession.exportDiagnostics()
        val clipboard = getSystemService(ClipboardManager::class.java)
        clipboard.setPrimaryClip(ClipData.newPlainText("Zara local diagnostics", text))
        Toast.makeText(this, "Diagnostics copied — paste them into ChatGPT", Toast.LENGTH_LONG).show()
    }

    private fun shareDiagnostics() {
        val text = appSession.exportDiagnostics()
        val intent = Intent(Intent.ACTION_SEND).apply {
            type = "text/plain"
            putExtra(Intent.EXTRA_SUBJECT, "Zara local diagnostics")
            putExtra(Intent.EXTRA_TEXT, text)
        }
        startActivity(Intent.createChooser(intent, "Share Zara diagnostics"))
    }

    private fun clearDiagnostics() {
        appSession.clearDiagnostics()
        Toast.makeText(this, "Diagnostics cleared", Toast.LENGTH_SHORT).show()
    }

    private fun hasMicrophonePermission(): Boolean =
        ContextCompat.checkSelfPermission(this, Manifest.permission.RECORD_AUDIO) ==
            PackageManager.PERMISSION_GRANTED
}
