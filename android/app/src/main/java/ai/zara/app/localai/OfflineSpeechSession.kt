package ai.zara.app.localai

import java.util.Locale
import java.util.UUID
import java.util.concurrent.CancellationException
import java.util.concurrent.CompletableFuture

internal data class OfflineSpeechVoice(
    val id: String,
    val locale: String,
    val networkRequired: Boolean,
    val installed: Boolean,
    val quality: Int,
    val latency: Int,
)

internal interface OfflineSpeechListener {
    fun done(id: String)
    fun failed(id: String)
    fun stopped(id: String)
}

internal interface OfflineSpeechEngine : AutoCloseable {
    val voices: List<OfflineSpeechVoice>
    fun selectVoice(id: String): Boolean
    fun setListener(value: OfflineSpeechListener)
    fun speak(text: String, id: String): Boolean
    fun stop()
}

internal class OfflineSpeechSession(
    private val createEngine: () -> CompletableFuture<OfflineSpeechEngine>,
) : LocalTtsBackend {
    private var current = LocalTtsState(LocalTtsPhase.STOPPED)
    private var engine: OfflineSpeechEngine? = null
    private var initialization: CompletableFuture<LocalTtsState>? = null
    private var closed = false
    private var generation = 0L
    private val pending = linkedMapOf<String, CompletableFuture<Unit>>()

    @Synchronized override fun state(): LocalTtsState = current

    @Synchronized override fun initialize(): CompletableFuture<LocalTtsState> {
        if (closed) return CompletableFuture.failedFuture(IllegalStateException("Offline speech is closed"))
        initialization?.takeIf { !it.isDone }?.let { return it }
        if (current.phase in setOf(LocalTtsPhase.READY, LocalTtsPhase.SPEAKING)) {
            return CompletableFuture.completedFuture(current)
        }
        engine?.close()
        engine = null
        val result = CompletableFuture<LocalTtsState>()
        initialization = result
        current = LocalTtsState(LocalTtsPhase.STARTING)
        val creation = try { createEngine() } catch (error: Exception) {
            CompletableFuture.failedFuture(error)
        }
        creation.whenComplete { next, error ->
            synchronized(this) {
                if (closed) {
                    next?.close()
                    result.completeExceptionally(IllegalStateException("Offline speech is closed"))
                } else if (error != null || next == null) {
                    current = LocalTtsState(LocalTtsPhase.UNAVAILABLE,
                        failure = "Install or enable a text-to-speech engine in Android speech settings.")
                    result.complete(current)
                } else {
                    engine = next
                    try {
                        next.setListener(listener)
                        val language = Locale.getDefault().language
                        val voice = next.voices.filter { !it.networkRequired && it.installed }
                            .sortedWith(compareByDescending<OfflineSpeechVoice> {
                                Locale.forLanguageTag(it.locale).language == language
                            }.thenByDescending { it.quality }.thenBy { it.latency }).firstOrNull()
                        current = if (voice == null || !next.selectVoice(voice.id)) {
                            LocalTtsState(LocalTtsPhase.UNAVAILABLE,
                                failure = "No installed offline voice. Install voice data, then refresh voices.")
                        } else LocalTtsState(LocalTtsPhase.READY, voice.id, voice.locale)
                    } catch (_: Exception) {
                        current = LocalTtsState(LocalTtsPhase.UNAVAILABLE,
                            failure = "Speech engine could not list offline voices. Check Android speech settings.")
                    }
                    result.complete(current)
                }
            }
        }
        return result
    }

    @Synchronized override fun speak(text: String): CompletableFuture<Unit> {
        val normalized = text.trim()
        require(normalized.isNotEmpty()) { "Speech text is required" }
        require(normalized.length <= 16_384) { "Speech text is too large" }
        if (closed) return CompletableFuture.failedFuture(IllegalStateException("Offline speech is closed"))
        stop()
        val token = generation
        return initialize().thenCompose { ready ->
            synchronized(this) {
                if (closed || token != generation) {
                    return@thenCompose CompletableFuture.failedFuture(CancellationException("Speech cancelled"))
                }
                if (ready.phase != LocalTtsPhase.READY) {
                    return@thenCompose CompletableFuture.failedFuture(IllegalStateException(ready.failure))
                }
                val id = UUID.randomUUID().toString()
                val result = CompletableFuture<Unit>()
                pending[id] = result
                current = current.copy(phase = LocalTtsPhase.SPEAKING, failure = null)
                try {
                    if (engine?.speak(normalized, id) != true) listener.failed(id)
                } catch (_: Exception) { listener.failed(id) }
                result
            }
        }
    }

    @Synchronized override fun stop() {
        generation += 1
        runCatching { engine?.stop() }
        val previous = pending.values.toList()
        pending.clear()
        previous.forEach { it.completeExceptionally(CancellationException("Speech stopped")) }
        if (current.phase == LocalTtsPhase.SPEAKING) current = current.copy(phase = LocalTtsPhase.READY)
    }

    @Synchronized override fun close() {
        if (closed) return
        closed = true
        stop()
        initialization?.completeExceptionally(IllegalStateException("Offline speech is closed"))
        runCatching { engine?.close() }
        engine = null
        current = LocalTtsState(LocalTtsPhase.STOPPED)
    }

    private val listener = object : OfflineSpeechListener {
        override fun done(id: String) = finish(id, null)
        override fun failed(id: String) = finish(id, IllegalStateException("Offline speech synthesis failed"))
        override fun stopped(id: String) = finish(id, CancellationException("Speech stopped"))
    }

    @Synchronized private fun finish(id: String, error: Throwable?) {
        val result = pending.remove(id) ?: return
        if (!closed) current = current.copy(
            phase = if (error != null && error !is CancellationException) LocalTtsPhase.FAILED else LocalTtsPhase.READY,
            failure = error?.takeUnless { it is CancellationException }?.message,
        )
        if (error == null) result.complete(Unit) else result.completeExceptionally(error)
    }
}
