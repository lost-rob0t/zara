package ai.zara.app.assistant

import ai.zara.app.AndroidAppSession
import ai.zara.app.runtime.LocalServerPhase
import ai.zara.app.ui.UiOperationFailure
import android.annotation.SuppressLint
import android.content.Context
import android.content.Intent
import android.os.Build
import android.os.Bundle
import android.speech.RecognitionListener
import android.speech.RecognizerIntent
import android.speech.SpeechRecognizer
import java.util.Locale
import java.util.concurrent.CompletableFuture

internal class LocalAssistantVoiceController(
    context: Context,
    private val appSession: AndroidAppSession,
    private val lifecycleFence: AssistantLifecycleFence,
    private val statusObserver: (String) -> Unit,
) : AutoCloseable {
    private val appContext = context.applicationContext
    private var recognizer: SpeechRecognizer? = null
    private var generation = 0L
    private var listening = false
    private var closed = false
    private val pendingTurn = PendingLocalTurn()

    @SuppressLint("NewApi")
    fun start(permissionGranted: Boolean) {
        check(!closed) { "Local assistant voice is closed" }
        check(permissionGranted) { "microphone permission is required" }
        check(appSession.localServerState().phase == LocalServerPhase.READY) {
            "Local Zara server is not ready"
        }
        check(Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            "Strict local voice requires Android 12 or newer"
        }
        check(SpeechRecognizer.isOnDeviceRecognitionAvailable(appContext)) {
            "On-device speech recognition is unavailable"
        }

        appSession.stopLocalSpeech()
        cancelRecognizer(invalidate = true)
        pendingTurn.cancel()
        val token = generation
        val lifecycleToken = lifecycleFence.beginStart()
        val next = SpeechRecognizer.createOnDeviceSpeechRecognizer(appContext)
        recognizer = next
        listening = true
        next.setRecognitionListener(listener(token, lifecycleToken))
        next.startListening(
            Intent(RecognizerIntent.ACTION_RECOGNIZE_SPEECH).apply {
                putExtra(
                    RecognizerIntent.EXTRA_LANGUAGE_MODEL,
                    RecognizerIntent.LANGUAGE_MODEL_FREE_FORM,
                )
                putExtra(RecognizerIntent.EXTRA_LANGUAGE, Locale.getDefault().toLanguageTag())
                putExtra(RecognizerIntent.EXTRA_PARTIAL_RESULTS, true)
                putExtra(RecognizerIntent.EXTRA_PREFER_OFFLINE, true)
                putExtra(RecognizerIntent.EXTRA_MAX_RESULTS, 3)
            }
        )
        statusObserver("Listening locally…")
    }

    fun stop() {
        if (!listening) return
        recognizer?.stopListening()
        statusObserver("Transcribing locally…")
    }

    fun cancel(notify: Boolean = true) {
        if (closed) return
        cancelRecognizer(invalidate = true)
        pendingTurn.cancel()
        appSession.stopLocalSpeech()
        if (notify) statusObserver("Voice cancelled")
    }

    override fun close() {
        if (closed) return
        closed = true
        cancelRecognizer(invalidate = true)
        pendingTurn.cancel()
        appSession.stopLocalSpeech()
    }

    private fun listener(
        token: Long,
        lifecycleToken: Long,
    ) = object : RecognitionListener {
        override fun onReadyForSpeech(params: Bundle?) {
            if (isCurrent(token, lifecycleToken)) statusObserver("Listening locally…")
        }

        override fun onBeginningOfSpeech() = Unit

        override fun onRmsChanged(rmsdB: Float) = Unit

        override fun onBufferReceived(buffer: ByteArray?) = Unit

        override fun onEndOfSpeech() {
            if (isCurrent(token, lifecycleToken)) statusObserver("Transcribing locally…")
        }

        override fun onError(error: Int) {
            if (!isCurrent(token, lifecycleToken)) return
            listening = false
            destroyRecognizer()
            statusObserver("Local voice unavailable: ${speechErrorLabel(error)}")
        }

        override fun onResults(results: Bundle?) {
            if (!isCurrent(token, lifecycleToken)) return
            val transcript = results
                ?.getStringArrayList(SpeechRecognizer.RESULTS_RECOGNITION)
                ?.firstOrNull()
                ?.trim()
                .orEmpty()
            listening = false
            destroyRecognizer()
            if (transcript.isEmpty()) {
                statusObserver("Local voice did not hear a usable utterance")
                return
            }
            statusObserver(
                if (appSession.runtimeMode() == ai.zara.app.runtime.RuntimeMode.Remote) {
                    "Thinking remotely…"
                } else {
                    "Thinking locally…"
                }
            )
            val turn = appSession.submitText(transcript)
            pendingTurn.track(turn)
            turn.whenComplete { result, error ->
                appContext.mainExecutor.execute {
                    if (!pendingTurn.isCurrent(turn)) return@execute
                    pendingTurn.clear(turn)
                    if (!isCurrent(token, lifecycleToken)) return@execute
                    if (error != null) {
                        statusObserver("Assistant failed: ${UiOperationFailure.summarize(error)}")
                    } else if (result != null) {
                        statusObserver(result.text)
                        if (result.text.isNotBlank()) appSession.speakLocal(result.text).whenComplete { _, speechError ->
                            if (speechError != null) appContext.mainExecutor.execute {
                                if (isCurrent(token, lifecycleToken)) statusObserver("Speech unavailable: ${UiOperationFailure.summarize(speechError)}")
                            }
                        }
                    }
                }
            }
        }

        override fun onPartialResults(partialResults: Bundle?) {
            if (!isCurrent(token, lifecycleToken)) return
            val partial = partialResults
                ?.getStringArrayList(SpeechRecognizer.RESULTS_RECOGNITION)
                ?.firstOrNull()
                ?.trim()
                .orEmpty()
            if (partial.isNotEmpty()) statusObserver("Heard: $partial")
        }

        override fun onEvent(eventType: Int, params: Bundle?) = Unit
    }

    private fun isCurrent(
        token: Long,
        lifecycleToken: Long,
    ): Boolean =
        !closed && token == generation && lifecycleFence.isCurrent(lifecycleToken)

    private fun cancelRecognizer(invalidate: Boolean) {
        if (invalidate) generation += 1
        listening = false
        runCatching { recognizer?.cancel() }
        destroyRecognizer()
    }

    private fun destroyRecognizer() {
        runCatching { recognizer?.destroy() }
        recognizer = null
    }
}

internal class PendingLocalTurn {
    private val lock = Any()
    private var active: CompletableFuture<*>? = null

    fun track(next: CompletableFuture<*>) {
        val previous = synchronized(lock) {
            val old = active
            active = next
            old
        }
        if (previous !== next) previous?.cancel(true)
    }

    fun isCurrent(turn: CompletableFuture<*>): Boolean = synchronized(lock) {
        active === turn
    }

    fun clear(completed: CompletableFuture<*>) {
        synchronized(lock) {
            if (active === completed) active = null
        }
    }

    fun cancel() {
        val pending = synchronized(lock) {
            val current = active
            active = null
            current
        }
        pending?.cancel(true)
    }
}

private fun speechErrorLabel(error: Int): String = when (error) {
    SpeechRecognizer.ERROR_AUDIO -> "audio capture failed"
    SpeechRecognizer.ERROR_CLIENT -> "recognizer client failed"
    SpeechRecognizer.ERROR_INSUFFICIENT_PERMISSIONS -> "microphone permission is required"
    SpeechRecognizer.ERROR_NETWORK,
    SpeechRecognizer.ERROR_NETWORK_TIMEOUT -> "on-device recognizer unexpectedly requested network"
    SpeechRecognizer.ERROR_NO_MATCH -> "speech was not recognized"
    SpeechRecognizer.ERROR_RECOGNIZER_BUSY -> "speech recognizer is busy"
    SpeechRecognizer.ERROR_SERVER -> "on-device recognizer failed"
    SpeechRecognizer.ERROR_SPEECH_TIMEOUT -> "no speech detected"
    else -> "speech recognizer error $error"
}
