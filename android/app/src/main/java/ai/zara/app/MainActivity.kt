package ai.zara.app

import ai.zara.app.assistant.AssistantLifecycleFence
import ai.zara.app.assistant.LocalAssistantVoiceController
import ai.zara.app.auth.PairingProgress
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
import androidx.compose.material3.AlertDialog
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.runtime.SideEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.setValue
import androidx.core.content.ContextCompat
import com.google.mlkit.vision.codescanner.GmsBarcodeScannerOptions
import com.google.mlkit.vision.codescanner.GmsBarcodeScanning
import com.google.mlkit.vision.barcode.common.Barcode
import java.io.File
import ai.zara.app.localai.LocalAiState
import ai.zara.app.localai.LocalModelBackend
import ai.zara.app.localai.LocalModelMetadata
import ai.zara.app.localai.LocalModelQuantization
import ai.zara.app.localai.LocalModelSpec
import java.io.InputStream
import java.security.MessageDigest
import java.util.concurrent.CompletableFuture

private data class PairingDialogState(
    val title: String,
    val message: String,
    val terminal: Boolean = false,
)

class MainActivity : ComponentActivity() {
    private lateinit var appSession: AndroidAppSession
    private lateinit var pairingCoordinator: AndroidPairingCoordinator
    private lateinit var conversationStore: ConversationStore
    private lateinit var localVoiceController: LocalAssistantVoiceController
    private val localVoiceFence = AssistantLifecycleFence()
    private var conversationState by mutableStateOf(ConversationState())
    private var microphonePermissionGranted by mutableStateOf(false)
    private var operationError by mutableStateOf<String?>(null)
    private var turnFailure by mutableStateOf<TurnFailure?>(null)
    private var voiceState by mutableStateOf<ManualVoiceState>(ManualVoiceState.Idle)
    private var localVoiceActive by mutableStateOf(false)
    private var localVoiceStatus by mutableStateOf<String?>(null)
    private var enrollmentPublicKey by mutableStateOf<String?>(null)
    private var pinnedServerPublicKey by mutableStateOf<String?>(null)
    private var pairingDialog by mutableStateOf<PairingDialogState?>(null)
    private var pairingUiGeneration = 0L

    private var localAiState by mutableStateOf(LocalAiState())
    private var localModels by mutableStateOf<List<LocalModelSpec>>(emptyList())
    private var localModelBusy by mutableStateOf(false)
    private var pendingLocalModelImport: LocalModelImportRequest? = null

    private data class LocalModelImportRequest(
        val id: String,
        val version: String,
        val quantization: LocalModelQuantization,
        val maxContextTokens: Int,
        val backend: LocalModelBackend,
    )

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        appSession = (application as ZaraApplication).appSession
        localVoiceController = LocalAssistantVoiceController(
            applicationContext,
            appSession,
            localVoiceFence,
        ) { status ->
            runOnUiThread { localVoiceStatus = status }
        }
        pairingCoordinator = AndroidPairingCoordinator(applicationContext, appSession)
        val updateManager = (application as ZaraApplication).updateManager
        val changelogSeenStore = ChangelogSeenStore(this)
        val currentChangelog = Changelog.load(this, BuildConfig.VERSION_NAME)
        var showCurrentChangelog by mutableStateOf(
            changelogSeenStore.shouldShow(BuildConfig.VERSION_NAME, currentChangelog)
        )
        microphonePermissionGranted = hasMicrophonePermission()
        voiceState = appSession.voiceState()
        enrollmentPublicKey = appSession.enrollmentPublicKeyZ85()
        pinnedServerPublicKey = appSession.pinnedServerPublicKeyZ85()

        var runtimeState by mutableStateOf(appSession.state())
        var operationBusy by mutableStateOf(false)
        var voiceStreamState by mutableStateOf(appSession.voiceStreamState())
        var voiceStreamFailure by mutableStateOf(appSession.voiceStreamFailure())
        var localServerState by mutableStateOf(appSession.localServerState())
        var cloudModelState by mutableStateOf(appSession.cloudModelState())
        var cloudModelBusy by mutableStateOf(false)
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

        val localModelPicker = registerForActivityResult(
            ActivityResultContracts.OpenDocument()
        ) { uri ->
            val request = pendingLocalModelImport
            pendingLocalModelImport = null
            if (uri == null || request == null) return@registerForActivityResult

            operationError = null
            localModelBusy = true
            runCatching {
                contentResolver.takePersistableUriPermission(
                    uri,
                    Intent.FLAG_GRANT_READ_URI_PERMISSION,
                )
            }
            CompletableFuture.supplyAsync {
                val source = contentResolver.openInputStream(uri)
                    ?: error("Selected local model could not be opened")
                sha256(source)
            }.thenCompose { digest ->
                val source = contentResolver.openInputStream(uri)
                    ?: error("Selected local model could not be reopened")
                val metadata = LocalModelMetadata(
                    id = request.id,
                    version = request.version,
                    quantization = request.quantization,
                    sha256 = digest,
                    maxContextTokens = request.maxContextTokens,
                    backend = request.backend,
                )
                try {
                    appSession.installLocalModel(source, metadata)
                } catch (error: Throwable) {
                    source.close()
                    throw error
                }
            }.whenComplete { _, error ->
                runOnUiThread {
                    localModelBusy = false
                    operationError = error?.let(UiOperationFailure::summarize)
                    refreshLocalModels()
                }
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
        refreshLocalModels()

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
                        appSession.recordChatBreadcrumb("chat.turn.begin", conversationId)
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
                localVoiceActive = localVoiceActive,
                localVoiceStatus = localVoiceStatus,
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
                localAiState = localAiState,
                localModels = localModels,
                localModelBusy = localModelBusy,
                cloudModelState = cloudModelState,
                cloudModelBusy = cloudModelBusy,
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
                onImportLocalModel = { id, version, quantization, maxContextTokens, backend ->
                    pendingLocalModelImport = LocalModelImportRequest(
                        id = id,
                        version = version,
                        quantization = quantization,
                        maxContextTokens = maxContextTokens,
                        backend = backend,
                    )
                    localModelPicker.launch(arrayOf("*/*"))
                },
                onSelectLocalModel = { id, version ->
                    operationError = null
                    localModelBusy = true
                    appSession.selectLocalModel(id, version).whenComplete { _, error ->
                        runOnUiThread {
                            localModelBusy = false
                            operationError = error?.let(UiOperationFailure::summarize)
                            refreshLocalModels()
                        }
                    }
                },
                onUnloadLocalModel = {
                    operationError = null
                    localModelBusy = true
                    appSession.unloadLocalModel().whenComplete { _, error ->
                        runOnUiThread {
                            localModelBusy = false
                            operationError = error?.let(UiOperationFailure::summarize)
                            refreshLocalModels()
                        }
                    }
                },
                onConfigureCloudModel = { config, apiKey ->
                    operationError = null
                    cloudModelBusy = true
                    CompletableFuture.supplyAsync {
                        appSession.configureCloudModel(config, apiKey)
                    }.whenComplete { state, error ->
                        runOnUiThread {
                            cloudModelBusy = false
                            operationError = error?.let(UiOperationFailure::summarize)
                            if (state != null) cloudModelState = state
                        }
                    }
                },
                onClearCloudModelApiKey = {
                    operationError = null
                    cloudModelBusy = true
                    CompletableFuture.supplyAsync {
                        appSession.clearCloudModelApiKey()
                    }.whenComplete { state, error ->
                        runOnUiThread {
                            cloudModelBusy = false
                            operationError = error?.let(UiOperationFailure::summarize)
                            if (state != null) cloudModelState = state
                        }
                    }
                },
                onScanPairingQr = ::scanPairingQr,
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
                    if (shouldUseRemoteVoiceTransport(runtimeMode, runtimeState)) {
                        operationBusy = true
                        appSession.pressToTalk(microphonePermissionGranted).whenComplete { _, error ->
                            runOnUiThread {
                                operationBusy = false
                                operationError = error?.let(UiOperationFailure::summarize)
                                voiceState = appSession.voiceState()
                            }
                        }
                    } else {
                        try {
                            localVoiceController.start(microphonePermissionGranted)
                            localVoiceActive = true
                        } catch (error: Throwable) {
                            localVoiceActive = false
                            operationError = UiOperationFailure.summarize(error)
                        }
                    }
                },
                onStopVoice = {
                    operationError = null
                    if (localVoiceActive) {
                        localVoiceController.stop()
                        localVoiceActive = false
                    } else {
                        operationBusy = true
                        appSession.releasePushToTalk().whenComplete { _, error ->
                            runOnUiThread {
                                operationBusy = false
                                operationError = error?.let(UiOperationFailure::summarize)
                                voiceState = appSession.voiceState()
                            }
                        }
                    }
                },
                onCancelVoice = {
                    operationError = null
                    if (localVoiceActive) {
                        localVoiceController.cancel()
                        localVoiceActive = false
                    } else {
                        operationBusy = true
                        appSession.cancelPushToTalk().whenComplete { _, error ->
                            runOnUiThread {
                                operationBusy = false
                                operationError = error?.let(UiOperationFailure::summarize)
                                voiceState = appSession.voiceState()
                            }
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

            pairingDialog?.let { dialog ->
                AlertDialog(
                    onDismissRequest = {
                        if (dialog.terminal) pairingDialog = null
                    },
                    title = { Text(dialog.title) },
                    text = { Text(dialog.message) },
                    confirmButton = {
                        if (dialog.terminal) {
                            TextButton(onClick = { pairingDialog = null }) {
                                Text("OK")
                            }
                        } else {
                            TextButton(onClick = {}, enabled = false) {
                                Text("WAITING")
                            }
                        }
                    },
                )
            }
        }

        handlePairingIntent(intent)
    }

    override fun onNewIntent(intent: Intent) {
        super.onNewIntent(intent)
        setIntent(intent)
        handlePairingIntent(intent)
    }

    override fun onResume() {
        super.onResume()
        if (!::appSession.isInitialized) return
        appSession.assessAssistantRole()
        reconcileMicrophonePermission(hasMicrophonePermission())
        refreshLocalModels()
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
        pairingUiGeneration += 1
        if (::appSession.isInitialized) {
            appSession.setStateObserver(null)
            appSession.setVoiceStreamObserver(null)
            appSession.setLocalServerObserver(null)
            (application as ZaraApplication).updateManager.setObserver(null)
        }
        if (::localVoiceController.isInitialized) {
            localVoiceFence.invalidate()
            localVoiceController.close()
        }
        if (::pairingCoordinator.isInitialized) pairingCoordinator.close()
        super.onDestroy()
    }

    private fun shouldUseRemoteVoiceTransport(
        mode: ai.zara.app.runtime.RuntimeMode,
        state: ai.zara.app.runtime.RuntimeState,
    ): Boolean {
        val remoteReady =
            state.enrollment == ai.zara.app.runtime.EnrollmentReadiness.Ready &&
                state.server is ServerConnection.Connected &&
                state.sessionId != null
        return remoteReady &&
            (mode == ai.zara.app.runtime.RuntimeMode.Remote ||
                mode == ai.zara.app.runtime.RuntimeMode.Auto)
    }

    private fun scanPairingQr() {
        operationError = null
        val options = GmsBarcodeScannerOptions.Builder()
            .setBarcodeFormats(Barcode.FORMAT_QR_CODE)
            .enableAutoZoom()
            .build()
        GmsBarcodeScanning.getClient(this, options)
            .startScan()
            .addOnSuccessListener { barcode ->
                val raw = barcode.rawValue
                if (raw.isNullOrBlank()) {
                    operationError = "QR code did not contain Zara pairing data"
                } else {
                    handlePairingUri(raw)
                }
            }
            .addOnCanceledListener {
                // User cancellation is not an error.
            }
            .addOnFailureListener { error ->
                operationError = UiOperationFailure.summarize(error)
            }
    }

    private fun handlePairingIntent(intent: Intent?) {
        val data = intent?.data ?: return
        if (!data.scheme.equals("zara", ignoreCase = true) ||
            !data.host.equals("pair", ignoreCase = true) ||
            data.path != "/v1"
        ) {
            return
        }
        handlePairingUri(data.toString())
    }

    private fun handlePairingUri(rawPayload: String) {
        pairingUiGeneration += 1
        val pairingGeneration = pairingUiGeneration
        operationError = null
        pairingDialog = PairingDialogState(
            title = "PAIR WITH ZARA",
            message = "Contacting the trusted pairing broker…",
        )
        pairingCoordinator.pair(rawPayload) { progress ->
            if (progress is PairingProgress.AwaitingApproval) {
                runOnUiThread {
                    if (!isPairingUiCurrent(pairingGeneration)) return@runOnUiThread
                    pairingDialog = PairingDialogState(
                        title = "VERIFY PAIRING",
                        message =
                            "${progress.verificationCode}\n\n" +
                                "Confirm this same code in the zara pair terminal. " +
                                "Device: ${progress.deviceId}",
                    )
                }
            }
        }.whenComplete { _, error ->
            runOnUiThread {
                if (!isPairingUiCurrent(pairingGeneration)) return@runOnUiThread
                enrollmentPublicKey = appSession.enrollmentPublicKeyZ85()
                pinnedServerPublicKey = appSession.pinnedServerPublicKeyZ85()
                if (error != null) {
                    operationError = UiOperationFailure.summarize(error)
                    pairingDialog = PairingDialogState(
                        title = "PAIRING FAILED",
                        message = operationError ?: "Zara pairing failed",
                        terminal = true,
                    )
                } else {
                    pairingDialog = PairingDialogState(
                        title = "PAIRED",
                        message = "This device is enrolled and connected to Zara.",
                        terminal = true,
                    )
                }
            }
        }
    }

    private fun isPairingUiCurrent(pairingGeneration: Long): Boolean =
        !isDestroyed && pairingUiGeneration == pairingGeneration

    private fun refreshLocalModels() {
        if (!::appSession.isInitialized) return
        appSession.localAiState()
            .thenCombine(appSession.localAiModels()) { state, models -> state to models }
            .whenComplete { snapshot, error ->
                runOnUiThread {
                    if (error != null) {
                        operationError = UiOperationFailure.summarize(error)
                    } else if (snapshot != null) {
                        localAiState = snapshot.first
                        localModels = snapshot.second
                    }
                }
            }
    }

    private fun sha256(source: InputStream): String {
        val digest = MessageDigest.getInstance("SHA-256")
        source.use { input ->
            val buffer = ByteArray(DEFAULT_BUFFER_SIZE)
            while (true) {
                val read = input.read(buffer)
                if (read < 0) break
                if (read > 0) digest.update(buffer, 0, read)
            }
        }
        return digest.digest().joinToString("") { "%02x".format(it) }
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
        appSession.recordChatBreadcrumb("chat.turn.failed code=${classified.code}", conversationId)
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
