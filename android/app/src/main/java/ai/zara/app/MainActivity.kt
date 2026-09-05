package ai.zara.app

import ai.zara.app.ui.RenderedTextTurn
import ai.zara.app.ui.ZaraApp
import ai.zara.app.voice.ManualVoiceState
import android.Manifest
import android.content.pm.PackageManager
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.result.contract.ActivityResultContracts
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.setValue
import androidx.core.content.ContextCompat

class MainActivity : ComponentActivity() {
    private lateinit var appSession: AndroidAppSession
    private var microphonePermissionGranted by mutableStateOf(false)
    private var operationError by mutableStateOf<String?>(null)
    private var voiceState by mutableStateOf<ManualVoiceState>(ManualVoiceState.Idle)

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        appSession = (application as ZaraApplication).appSession
        microphonePermissionGranted = hasMicrophonePermission()
        voiceState = appSession.voiceState()

        var runtimeState by mutableStateOf(appSession.state())
        var enrollmentPublicKey by mutableStateOf(appSession.enrollmentPublicKeyZ85())
        var lastTurn by mutableStateOf<RenderedTextTurn?>(null)
        var operationBusy by mutableStateOf(false)
        var voiceStreamState by mutableStateOf(appSession.voiceStreamState())
        var voiceStreamFailure by mutableStateOf(appSession.voiceStreamFailure())

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
                operationError = rootMessage(error)
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
                onCreateIdentity = {
                    operationError = null
                    try {
                        enrollmentPublicKey = appSession.createIdentity()
                    } catch (error: Exception) {
                        operationError = rootMessage(error)
                    }
                },
                onPinServer = { publicKey ->
                    operationError = null
                    try {
                        appSession.pinServer(publicKey)
                        enrollmentPublicKey = appSession.enrollmentPublicKeyZ85()
                    } catch (error: Exception) {
                        operationError = rootMessage(error)
                    }
                },
                onConnect = { endpoint ->
                    operationError = null
                    operationBusy = true
                    try {
                        appSession.connect(endpoint).whenComplete { _, error ->
                            runOnUiThread {
                                operationBusy = false
                                operationError = error?.let(::rootMessage)
                            }
                        }
                    } catch (error: Exception) {
                        operationBusy = false
                        operationError = rootMessage(error)
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
                                    operationError = rootMessage(error)
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
                        operationError = rootMessage(error)
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
                            operationError = error?.let(::rootMessage)
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
                            operationError = error?.let(::rootMessage)
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
                            operationError = error?.let(::rootMessage)
                            voiceState = appSession.voiceState()
                        }
                    }
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
                    if (error != null) operationError = rootMessage(error)
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
        super.onDestroy()
    }

    private fun reconcileMicrophonePermission(granted: Boolean) {
        microphonePermissionGranted = granted
        appSession.onMicrophonePermissionChanged(granted).whenComplete { _, error ->
            runOnUiThread {
                if (error != null) operationError = rootMessage(error)
                voiceState = appSession.voiceState()
            }
        }
    }

    private fun hasMicrophonePermission(): Boolean =
        ContextCompat.checkSelfPermission(this, Manifest.permission.RECORD_AUDIO) ==
            PackageManager.PERMISSION_GRANTED

    private fun rootMessage(error: Throwable): String {
        var current = error
        while (current.cause != null && current.cause !== current) {
            current = current.cause!!
        }
        return current.message ?: current::class.java.simpleName
    }
}
