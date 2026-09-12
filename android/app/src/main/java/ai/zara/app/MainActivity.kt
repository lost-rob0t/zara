package ai.zara.app

import ai.zara.app.auth.PairingProgress
import ai.zara.app.ui.RenderedTextTurn
import ai.zara.app.ui.ThemePreferenceStore
import ai.zara.app.ui.UiOperationFailure
import ai.zara.app.ui.ZaraApp
import ai.zara.app.ui.ZaraTheme
import ai.zara.app.voice.ManualVoiceState
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
import androidx.compose.material3.AlertDialog
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.runtime.SideEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.setValue
import androidx.core.content.ContextCompat
import java.io.File

private data class PairingDialogState(
    val title: String,
    val message: String,
    val terminal: Boolean = false,
)

class MainActivity : ComponentActivity() {
    private lateinit var appSession: AndroidAppSession
    private lateinit var pairingCoordinator: AndroidPairingCoordinator
    private var microphonePermissionGranted by mutableStateOf(false)
    private var operationError by mutableStateOf<String?>(null)
    private var voiceState by mutableStateOf<ManualVoiceState>(ManualVoiceState.Idle)
    private var enrollmentPublicKey by mutableStateOf<String?>(null)
    private var pairingDialog by mutableStateOf<PairingDialogState?>(null)

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        appSession = (application as ZaraApplication).appSession
        pairingCoordinator = AndroidPairingCoordinator(applicationContext, appSession)
        microphonePermissionGranted = hasMicrophonePermission()
        voiceState = appSession.voiceState()
        enrollmentPublicKey = appSession.enrollmentPublicKeyZ85()

        var runtimeState by mutableStateOf(appSession.state())
        var lastTurn by mutableStateOf<RenderedTextTurn?>(null)
        var operationBusy by mutableStateOf(false)
        var voiceStreamState by mutableStateOf(appSession.voiceStreamState())
        var voiceStreamFailure by mutableStateOf(appSession.voiceStreamFailure())
        val themePreferenceStore = ThemePreferenceStore(File(filesDir, "theme.bin"))
        var selectedTheme by mutableStateOf(themePreferenceStore.load())

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
                lastTurn = lastTurn,
                operationError = operationError,
                operationBusy = operationBusy,
                microphonePermissionGranted = microphonePermissionGranted,
                voiceState = voiceState,
                voiceStreamState = voiceStreamState,
                voiceStreamFailure = voiceStreamFailure,
                selectedTheme = selectedTheme,
                onSelectTheme = { theme ->
                    selectedTheme = theme
                    themePreferenceStore.save(theme)
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
        }
        if (::pairingCoordinator.isInitialized) pairingCoordinator.close()
        super.onDestroy()
    }

    private fun handlePairingIntent(intent: Intent?) {
        val data = intent?.data ?: return
        if (!data.scheme.equals("zara", ignoreCase = true) ||
            !data.host.equals("pair", ignoreCase = true) ||
            data.path != "/v1"
        ) {
            return
        }

        operationError = null
        pairingDialog = PairingDialogState(
            title = "PAIR WITH ZARA",
            message = "Contacting the trusted pairing broker…",
        )
        pairingCoordinator.pair(data.toString()) { progress ->
            if (progress is PairingProgress.AwaitingApproval) {
                runOnUiThread {
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
                enrollmentPublicKey = appSession.enrollmentPublicKeyZ85()
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

    private fun reconcileMicrophonePermission(granted: Boolean) {
        microphonePermissionGranted = granted
        appSession.onMicrophonePermissionChanged(granted).whenComplete { _, error ->
            runOnUiThread {
                if (error != null) operationError = UiOperationFailure.summarize(error)
                voiceState = appSession.voiceState()
            }
        }
    }

    private fun hasMicrophonePermission(): Boolean =
        ContextCompat.checkSelfPermission(this, Manifest.permission.RECORD_AUDIO) ==
            PackageManager.PERMISSION_GRANTED
}
