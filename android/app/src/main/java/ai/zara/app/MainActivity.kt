package ai.zara.app

import ai.zara.app.ui.RenderedTextTurn
import ai.zara.app.ui.AppSurface
import ai.zara.app.ui.LocalEmbeddingPreferenceStore
import ai.zara.app.ui.RuntimeModePreferenceStore
import ai.zara.app.ui.ThemePreferenceStore
import ai.zara.app.ui.UiOperationFailure
import ai.zara.app.ui.ZaraApp
import ai.zara.app.voice.ManualVoiceState
import ai.zara.app.widget.ACTION_WIDGET_ROUTE
import ai.zara.app.widget.EXTRA_WIDGET_ROUTE
import ai.zara.app.widget.WidgetRoute
import ai.zara.app.widget.WidgetRuntimeSnapshot
import ai.zara.app.widget.WidgetRuntimeSnapshotStore
import ai.zara.app.widget.WidgetStyleCompiler
import ai.zara.app.widget.WidgetStyleEnvironment
import ai.zara.app.widget.ZaraWidgetUpdater
import ai.zara.ui.theme.ZaraTheme
import android.Manifest
import android.content.Intent
import android.content.pm.PackageManager
import android.graphics.Color
import android.os.Bundle
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
import java.nio.charset.StandardCharsets

class MainActivity : ComponentActivity() {
    private lateinit var appSession: AndroidAppSession
    private var microphonePermissionGranted by mutableStateOf(false)
    private var operationError by mutableStateOf<String?>(null)
    private var voiceState by mutableStateOf<ManualVoiceState>(ManualVoiceState.Idle)
    private var requestedSurface by mutableStateOf<AppSurface?>(null)
    private var widgetNavigationSequence by mutableStateOf(0L)

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        appSession = (application as ZaraApplication).appSession
        requestedSurface = widgetSurface(intent)
        if (requestedSurface != null) widgetNavigationSequence += 1
        val updateManager = (application as ZaraApplication).updateManager
        microphonePermissionGranted = hasMicrophonePermission()
        voiceState = appSession.voiceState()

        var runtimeState by mutableStateOf(appSession.state())
        var enrollmentPublicKey by mutableStateOf(appSession.enrollmentPublicKeyZ85())
        var pinnedServerPublicKey by mutableStateOf(appSession.pinnedServerPublicKeyZ85())
        var lastTurn by mutableStateOf<RenderedTextTurn?>(null)
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
        var widgetStyleStatus by mutableStateOf(WidgetStyleEnvironment.status(this))
        val widgetSnapshotStore = WidgetRuntimeSnapshotStore(
            File(noBackupFilesDir, "zara/widget-runtime.bin"),
        )
        val refreshWidgetSnapshot = {
            widgetSnapshotStore.save(
                WidgetRuntimeSnapshot.from(
                    runtimeState.server,
                    localServerState.phase,
                    runtimeMode,
                    System.currentTimeMillis(),
                ),
            )
            ZaraWidgetUpdater.refreshAll(this)
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
        val widgetStyleImport = registerForActivityResult(
            ActivityResultContracts.OpenDocument(),
        ) { uri ->
            if (uri == null) return@registerForActivityResult
            operationError = null
            try {
                val bytes = contentResolver.openInputStream(uri)?.use {
                    it.readNBytes(WidgetStyleCompiler.MAX_SOURCE_BYTES + 1)
                } ?: throw IllegalArgumentException("Widget stylesheet could not be opened")
                require(bytes.size <= WidgetStyleCompiler.MAX_SOURCE_BYTES) {
                    "Widget stylesheet is too large"
                }
                widgetStyleStatus = WidgetStyleEnvironment.import(
                    this,
                    String(bytes, StandardCharsets.UTF_8),
                )
                ZaraWidgetUpdater.refreshAll(this)
            } catch (error: Exception) {
                operationError = UiOperationFailure.summarize(error)
            }
        }
        val widgetStyleExport = registerForActivityResult(
            ActivityResultContracts.CreateDocument("text/x-prolog"),
        ) { uri ->
            if (uri == null) return@registerForActivityResult
            operationError = null
            try {
                contentResolver.openOutputStream(uri, "wt")?.bufferedWriter()?.use { writer ->
                    writer.write(WidgetStyleEnvironment.export(this))
                } ?: throw IllegalArgumentException("Widget stylesheet destination could not be opened")
            } catch (error: Exception) {
                operationError = UiOperationFailure.summarize(error)
            }
        }

        appSession.setStateObserver { state ->
            runOnUiThread {
                runtimeState = state
                refreshWidgetSnapshot()
            }
        }
        appSession.setVoiceStreamObserver { streamState, failure ->
            runOnUiThread {
                voiceStreamState = streamState
                voiceStreamFailure = failure
            }
        }
        appSession.setLocalServerObserver { state ->
            runOnUiThread {
                localServerState = state
                refreshWidgetSnapshot()
            }
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
            ZaraApp(
                runtimeState = runtimeState,
                sourceSha = BuildConfig.SOURCE_SHA,
                enrollmentPublicKey = enrollmentPublicKey,
                pinnedServerPublicKey = pinnedServerPublicKey,
                lastTurn = lastTurn,
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
                runtimeMode = runtimeMode,
                localEmbedding = localEmbedding,
                requestedSurface = requestedSurface,
                widgetNavigationSequence = widgetNavigationSequence,
                widgetStyleStatus = widgetStyleStatus,
                onSelectTheme = { theme ->
                    selectedTheme = theme
                    themePreferenceStore.save(theme)
                    widgetStyleStatus = WidgetStyleEnvironment.status(this)
                    ZaraWidgetUpdater.refreshAll(this)
                },
                onSelectRuntimeMode = { mode ->
                    runtimeMode = mode
                    runtimeModeStore.save(mode)
                    appSession.setRuntimeMode(mode)
                    refreshWidgetSnapshot()
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
                onSendText = { text ->
                    operationError = null
                    operationBusy = true
                    try {
                        appSession.submitText(text).whenComplete { result, error ->
                            runOnUiThread {
                                operationBusy = false
                                if (error != null) {
                                    operationError = UiOperationFailure.summarize(error)
                                } else if (result != null) {
                                    lastTurn = RenderedTextTurn(
                                        userText = text,
                                        assistantText = result.text,
                                        success = result.success,
                                    )
                                }
                            }
                        }
                    } catch (error: Exception) {
                        operationBusy = false
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
                onImportWidgetStyle = {
                    widgetStyleImport.launch(arrayOf("text/x-prolog", "text/plain", "application/octet-stream"))
                },
                onExportWidgetStyle = {
                    widgetStyleExport.launch("${widgetStyleStatus.name}.pl")
                },
                onResetWidgetStyle = {
                    operationError = null
                    try {
                        widgetStyleStatus = WidgetStyleEnvironment.reset(this)
                        ZaraWidgetUpdater.refreshAll(this)
                    } catch (error: Exception) {
                        operationError = UiOperationFailure.summarize(error)
                    }
                },
                onCheckForUpdate = {
                    operationError = null
                    updateManager.check().whenComplete { _, error ->
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
            )
        }
    }

    override fun onNewIntent(intent: Intent) {
        super.onNewIntent(intent)
        setIntent(intent)
        requestedSurface = widgetSurface(intent)
        if (requestedSurface != null) widgetNavigationSequence += 1
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

    private fun widgetSurface(intent: Intent?): AppSurface? {
        if (intent?.action != ACTION_WIDGET_ROUTE) return null
        return when (intent.getStringExtra(EXTRA_WIDGET_ROUTE)) {
            WidgetRoute.CHAT.atom -> AppSurface.Chat
            WidgetRoute.LOGIC.atom -> AppSurface.Logic
            WidgetRoute.VOICE.atom -> AppSurface.Voice
            WidgetRoute.REMOTE.atom -> AppSurface.Remote
            WidgetRoute.DIAGNOSTICS.atom -> AppSurface.Diagnostics
            WidgetRoute.THEMES.atom -> AppSurface.Themes
            else -> null
        }
    }

    private fun hasMicrophonePermission(): Boolean =
        ContextCompat.checkSelfPermission(this, Manifest.permission.RECORD_AUDIO) ==
            PackageManager.PERMISSION_GRANTED
}
