package ai.zara.app.localai

import android.content.Context
import android.speech.tts.TextToSpeech
import android.speech.tts.UtteranceProgressListener
import android.speech.tts.Voice
import java.util.Locale
import java.util.UUID
import java.util.concurrent.CancellationException
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ConcurrentHashMap

class AndroidOfflineTtsBackend(
    context: Context,
) : LocalTtsBackend {
    private val appContext = context.applicationContext
    private val pending = ConcurrentHashMap<String, CompletableFuture<Unit>>()

    @Volatile
    private var current = LocalTtsState(LocalTtsPhase.STOPPED)

    @Volatile
    private var tts: TextToSpeech? = null

    @Volatile
    private var initialization: CompletableFuture<LocalTtsState>? = null

    override fun state(): LocalTtsState = current

    @Synchronized
    override fun initialize(): CompletableFuture<LocalTtsState> {
        initialization?.let { return it }
        val future = CompletableFuture<LocalTtsState>()
        initialization = future
        current = LocalTtsState(LocalTtsPhase.STARTING)
        var engine: TextToSpeech? = null
        engine = TextToSpeech(appContext) { status ->
            val instance = engine
            if (status != TextToSpeech.SUCCESS || instance == null) {
                current = LocalTtsState(LocalTtsPhase.UNAVAILABLE, failure = "Offline TTS engine initialization failed")
                future.complete(current)
                return@TextToSpeech
            }
            tts = instance
            instance.setOnUtteranceProgressListener(progressListener)
            val voice = chooseOfflineVoice(instance.voices.orEmpty())
            if (voice == null || instance.setVoice(voice) == TextToSpeech.ERROR) {
                current = LocalTtsState(LocalTtsPhase.UNAVAILABLE, failure = "No installed offline TTS voice is available")
                future.complete(current)
                return@TextToSpeech
            }
            current = LocalTtsState(
                phase = LocalTtsPhase.READY,
                voiceId = voice.name,
                locale = voice.locale.toLanguageTag(),
            )
            future.complete(current)
        }
        return future
    }

    override fun speak(text: String): CompletableFuture<Unit> {
        val normalized = text.trim()
        require(normalized.isNotEmpty()) { "Speech text is required" }
        require(normalized.length <= MAX_TEXT_CHARS) { "Speech text is too large" }
        return initialize().thenCompose { ready ->
            if (ready.phase != LocalTtsPhase.READY && ready.phase != LocalTtsPhase.SPEAKING) {
                return@thenCompose failed(IllegalStateException(ready.failure ?: "Offline TTS is unavailable"))
            }
            val engine = tts
                ?: return@thenCompose failed(IllegalStateException("Offline TTS engine is unavailable"))
            val utteranceId = UUID.randomUUID().toString()
            val future = CompletableFuture<Unit>()
            pending[utteranceId] = future
            val result = engine.speak(normalized, TextToSpeech.QUEUE_FLUSH, null, utteranceId)
            if (result == TextToSpeech.ERROR) {
                pending.remove(utteranceId)
                future.completeExceptionally(IllegalStateException("Offline TTS rejected the utterance"))
            }
            future
        }
    }

    override fun stop() {
        val engine = tts ?: return
        engine.stop()
        val error = CancellationException("Local speech cancelled")
        pending.entries.toList().forEach { (id, future) ->
            if (pending.remove(id, future)) future.completeExceptionally(error)
        }
        if (current.phase != LocalTtsPhase.UNAVAILABLE && current.phase != LocalTtsPhase.FAILED) {
            current = current.copy(phase = LocalTtsPhase.READY, failure = null)
        }
    }

    @Synchronized
    override fun close() {
        stop()
        tts?.shutdown()
        tts = null
        initialization = null
        current = LocalTtsState(LocalTtsPhase.STOPPED)
    }

    private fun chooseOfflineVoice(voices: Set<Voice>): Voice? =
        voices
            .asSequence()
            .filter { !it.isNetworkConnectionRequired }
            .sortedWith(
                compareByDescending<Voice> { it.locale.language == Locale.US.language }
                    .thenByDescending { it.quality }
                    .thenBy { it.latency }
            )
            .firstOrNull()

    private val progressListener = object : UtteranceProgressListener() {
        override fun onStart(utteranceId: String) {
            if (pending.containsKey(utteranceId)) current = current.copy(phase = LocalTtsPhase.SPEAKING)
        }

        override fun onDone(utteranceId: String) {
            pending.remove(utteranceId)?.complete(Unit)
            if (pending.isEmpty()) current = current.copy(phase = LocalTtsPhase.READY, failure = null)
        }

        @Deprecated("Deprecated by Android; API 21+ dispatches onError(String, Int)")
        override fun onError(utteranceId: String) {
            onError(utteranceId, TextToSpeech.ERROR)
        }

        override fun onError(utteranceId: String, errorCode: Int) {
            pending.remove(utteranceId)?.completeExceptionally(
                IllegalStateException("Offline TTS failed with code $errorCode")
            )
            current = current.copy(
                phase = LocalTtsPhase.FAILED,
                failure = "Offline TTS synthesis failed",
            )
        }

        override fun onStop(utteranceId: String, interrupted: Boolean) {
            pending.remove(utteranceId)?.completeExceptionally(CancellationException("Local speech stopped"))
            if (pending.isEmpty()) current = current.copy(phase = LocalTtsPhase.READY, failure = null)
        }
    }

    private fun <T> failed(error: Throwable): CompletableFuture<T> =
        CompletableFuture<T>().also { it.completeExceptionally(error) }

    companion object {
        private const val MAX_TEXT_CHARS = 16_384
    }
}
