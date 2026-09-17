package ai.zara.app.assistant

import android.content.ComponentName
import android.content.Intent
import android.os.Build
import android.os.Bundle
import android.speech.RecognitionListener
import android.speech.RecognitionService
import android.speech.SpeechRecognizer

class ZaraRecognitionService : RecognitionService() {
    private var delegate: SpeechRecognizer? = null

    override fun onStartListening(recognizerIntent: Intent, callback: Callback) {
        if (delegate != null) {
            callback.error(SpeechRecognizer.ERROR_RECOGNIZER_BUSY)
            return
        }

        val recognizer = runCatching { createDelegateRecognizer() }.getOrNull()
        if (recognizer == null) {
            callback.error(SpeechRecognizer.ERROR_CLIENT)
            return
        }

        delegate = recognizer
        recognizer.setRecognitionListener(ForwardingRecognitionListener(callback, recognizer))
        runCatching { recognizer.startListening(recognizerIntent) }
            .onFailure {
                callback.error(SpeechRecognizer.ERROR_CLIENT)
                releaseDelegate(recognizer)
            }
    }

    override fun onStopListening(callback: Callback) {
        val recognizer = delegate ?: return
        runCatching { recognizer.stopListening() }
            .onFailure {
                callback.error(SpeechRecognizer.ERROR_CLIENT)
                releaseDelegate(recognizer)
            }
    }

    override fun onCancel(callback: Callback) {
        val recognizer = delegate ?: return
        runCatching { recognizer.cancel() }
        releaseDelegate(recognizer)
    }

    override fun onDestroy() {
        releaseDelegate(delegate)
        super.onDestroy()
    }

    private fun createDelegateRecognizer(): SpeechRecognizer? {
        if (
            Build.VERSION.SDK_INT >= Build.VERSION_CODES.S &&
            SpeechRecognizer.isOnDeviceRecognitionAvailable(this)
        ) {
            return SpeechRecognizer.createOnDeviceSpeechRecognizer(this)
        }

        val serviceInfo = packageManager.queryIntentServices(
            Intent(SERVICE_INTERFACE),
            0,
        ).asSequence()
            .mapNotNull { it.serviceInfo }
            .firstOrNull { it.packageName != packageName }
            ?: return null

        return SpeechRecognizer.createSpeechRecognizer(
            this,
            ComponentName(serviceInfo.packageName, serviceInfo.name),
        )
    }

    private fun releaseDelegate(expected: SpeechRecognizer?) {
        val current = delegate ?: return
        if (expected != null && current !== expected) return
        delegate = null
        runCatching { current.destroy() }
    }

    private inner class ForwardingRecognitionListener(
        private val callback: Callback,
        private val recognizer: SpeechRecognizer,
    ) : RecognitionListener {
        override fun onReadyForSpeech(params: Bundle) = callback.readyForSpeech(params)

        override fun onBeginningOfSpeech() = callback.beginningOfSpeech()

        override fun onRmsChanged(rmsdB: Float) = callback.rmsChanged(rmsdB)

        override fun onBufferReceived(buffer: ByteArray) = callback.bufferReceived(buffer)

        override fun onEndOfSpeech() = callback.endOfSpeech()

        override fun onError(error: Int) {
            callback.error(error)
            releaseDelegate(recognizer)
        }

        override fun onResults(results: Bundle) {
            callback.results(results)
            releaseDelegate(recognizer)
        }

        override fun onPartialResults(partialResults: Bundle) =
            callback.partialResults(partialResults)

        override fun onEvent(eventType: Int, params: Bundle) = Unit
    }
}
