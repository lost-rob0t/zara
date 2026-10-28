package ai.zara.app.localai

import android.content.Context
import android.os.Handler
import android.os.Looper
import android.speech.tts.TextToSpeech
import android.speech.tts.UtteranceProgressListener
import java.util.concurrent.CompletableFuture

class AndroidOfflineTtsBackend(context: Context) : LocalTtsProvider {
    override val capabilities = LocalTtsProviderCapabilities(
        id = PROVIDER_ID,
        displayName = "Android Offline Voice",
        offlineOnly = true,
        modelBacked = false,
    )
    private val appContext = context.applicationContext
    private val session = OfflineSpeechSession(::createEngine)

    override fun state() = session.state()
    override fun initialize() = session.initialize()
    override fun speak(text: String) = session.speak(text)
    override fun stop() = session.stop()
    override fun close() = session.close()

    private fun createEngine(): CompletableFuture<OfflineSpeechEngine> {
        val ready = CompletableFuture<OfflineSpeechEngine>()
        val main = Handler(Looper.getMainLooper())
        var engine: TextToSpeech? = null
        ready.orTimeout(15, java.util.concurrent.TimeUnit.SECONDS).whenComplete { _, error ->
            if (error != null) main.post { engine?.shutdown() }
        }
        main.post {
            try {
                engine = TextToSpeech(appContext) { status ->
                    main.post {
                        val initialized = engine
                        if (status != TextToSpeech.SUCCESS || initialized == null) {
                            initialized?.shutdown()
                            ready.completeExceptionally(IllegalStateException("Android speech engine is unavailable"))
                        } else if (!ready.complete(AndroidSpeechEngine(initialized))) initialized.shutdown()
                    }
                }
            } catch (error: Exception) { ready.completeExceptionally(error) }
        }
        return ready
    }

    private class AndroidSpeechEngine(private val engine: TextToSpeech) : OfflineSpeechEngine {
        override val voices: List<OfflineSpeechVoice>
            get() = engine.voices.orEmpty().map { voice ->
                OfflineSpeechVoice(voice.name, voice.locale.toLanguageTag(), voice.isNetworkConnectionRequired,
                    TextToSpeech.Engine.KEY_FEATURE_NOT_INSTALLED !in voice.features.orEmpty(), voice.quality, voice.latency)
            }
        override fun selectVoice(id: String): Boolean {
            val voice = engine.voices.orEmpty().firstOrNull { it.name == id } ?: return false
            return engine.setVoice(voice) == TextToSpeech.SUCCESS
        }
        override fun setListener(value: OfflineSpeechListener) {
            engine.setOnUtteranceProgressListener(object : UtteranceProgressListener() {
                override fun onStart(utteranceId: String) = Unit
                override fun onDone(utteranceId: String) = value.done(utteranceId)
                @Deprecated("Android legacy callback")
                override fun onError(utteranceId: String) = value.failed(utteranceId)
                override fun onError(utteranceId: String, errorCode: Int) = value.failed(utteranceId)
                override fun onStop(utteranceId: String, interrupted: Boolean) = value.stopped(utteranceId)
            })
        }
        override fun speak(text: String, id: String): Boolean =
            engine.speak(text, TextToSpeech.QUEUE_FLUSH, null, id) != TextToSpeech.ERROR
        override fun stop() { engine.stop() }
        override fun close() { engine.shutdown() }
    }

    companion object { const val PROVIDER_ID = "android-offline" }
}
