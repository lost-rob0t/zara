package ai.zara.app

import ai.zara.app.ui.RenderedTextTurn
import ai.zara.app.ui.ZaraApp
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.setValue

class MainActivity : ComponentActivity() {
    private lateinit var appSession: AndroidAppSession

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        appSession = AndroidAppSession(this)

        var runtimeState by mutableStateOf(appSession.state())
        var enrollmentPublicKey by mutableStateOf(appSession.enrollmentPublicKeyZ85())
        var lastTurn by mutableStateOf<RenderedTextTurn?>(null)
        var operationError by mutableStateOf<String?>(null)
        var operationBusy by mutableStateOf(false)

        setContent {
            ZaraApp(
                runtimeState = runtimeState,
                sourceSha = BuildConfig.SOURCE_SHA,
                enrollmentPublicKey = enrollmentPublicKey,
                lastTurn = lastTurn,
                operationError = operationError,
                operationBusy = operationBusy,
                onCreateIdentity = {
                    operationError = null
                    try {
                        enrollmentPublicKey = appSession.createIdentity()
                        runtimeState = appSession.state()
                    } catch (error: Exception) {
                        operationError = rootMessage(error)
                        runtimeState = appSession.state()
                    }
                },
                onPinServer = { publicKey ->
                    operationError = null
                    try {
                        appSession.pinServer(publicKey)
                        enrollmentPublicKey = appSession.enrollmentPublicKeyZ85()
                        runtimeState = appSession.state()
                    } catch (error: Exception) {
                        operationError = rootMessage(error)
                        runtimeState = appSession.state()
                    }
                },
                onConnect = { endpoint ->
                    operationError = null
                    operationBusy = true
                    try {
                        appSession.connect(endpoint).whenComplete { _, error ->
                            runOnUiThread {
                                operationBusy = false
                                runtimeState = appSession.state()
                                operationError = error?.let(::rootMessage)
                            }
                        }
                        runtimeState = appSession.state()
                    } catch (error: Exception) {
                        operationBusy = false
                        runtimeState = appSession.state()
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
                                runtimeState = appSession.state()
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
                        runtimeState = appSession.state()
                        operationError = rootMessage(error)
                    }
                },
            )
        }
    }

    override fun onDestroy() {
        if (::appSession.isInitialized) appSession.close()
        super.onDestroy()
    }

    private fun rootMessage(error: Throwable): String {
        var current = error
        while (current.cause != null && current.cause !== current) {
            current = current.cause!!
        }
        return current.message ?: current::class.java.simpleName
    }
}
