package ai.zara.app.localai

import java.util.concurrent.CancellationException
import java.util.concurrent.CompletableFuture
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.atomic.AtomicReference

/** External TTS Binder calls must never own Zara's main/IPC thread or state reads. */
internal class IsolatedSpeechBackend(
    private val backend: LocalTtsBackend,
    private val initializationTimeoutMs: Long = 15_000,
) : LocalTtsBackend {
    private val worker = Executors.newSingleThreadExecutor { task ->
        Thread(task, "zara-offline-speech").apply { isDaemon = true }
    }
    private val lock = Any()
    private val closed = AtomicBoolean()
    private val generation = AtomicLong()
    private val active = AtomicReference<CompletableFuture<Unit>?>()
    private val stopQueued = AtomicBoolean()
    @Volatile private var current = LocalTtsState(LocalTtsPhase.STOPPED)
    @Volatile private var engineInitializing = false
    private var initialization: CompletableFuture<LocalTtsState>? = null

    override fun state() = current

    override fun initialize(): CompletableFuture<LocalTtsState> = synchronized(lock) {
        if (closed.get()) return@synchronized CompletableFuture.failedFuture(CancellationException("Speech is closed"))
        initialization?.takeIf { !it.isDone }?.let { return@synchronized it }
        // One outstanding engine call is enough. Do not queue retries behind a hung Binder.
        if (engineInitializing) return@synchronized CompletableFuture.completedFuture(unavailable())
        engineInitializing = true
        current = LocalTtsState(LocalTtsPhase.STARTING)
        val reply = CompletableFuture<LocalTtsState>()
            .completeOnTimeout(unavailable(), initializationTimeoutMs, TimeUnit.MILLISECONDS)
        val result = reply.thenApply { state ->
            synchronized(lock) {
                if (closed.get()) throw CancellationException("Speech is closed")
                current = state
                state
            }
        }
        initialization = result
        worker.execute {
            try {
                backend.initialize().whenComplete { state, error ->
                    engineInitializing = false
                    reply.complete(if (error == null && state != null) state else unavailable())
                }
            } catch (_: Exception) {
                engineInitializing = false
                reply.complete(unavailable())
            }
        }
        result
    }

    override fun speak(text: String): CompletableFuture<Unit> {
        require(text.trim().isNotEmpty()) { "Speech text is required" }
        require(text.trim().length <= 16_384) { "Speech text is too large" }
        stop()
        val result = CompletableFuture<Unit>()
        val token = synchronized(lock) {
            if (closed.get()) return CompletableFuture.failedFuture(CancellationException("Speech is closed"))
            active.set(result)
            generation.get()
        }
        initialize().whenComplete { ready, error ->
            if (error != null) result.completeExceptionally(error)
            else if (ready.phase != LocalTtsPhase.READY) result.completeExceptionally(IllegalStateException(ready.failure))
            else synchronized(lock) {
                if (closed.get() || token != generation.get()) result.completeExceptionally(CancellationException("Speech stopped"))
                else worker.execute {
                    if (closed.get() || token != generation.get()) {
                        result.completeExceptionally(CancellationException("Speech stopped"))
                    } else try {
                        current = ready.copy(phase = LocalTtsPhase.SPEAKING)
                        backend.speak(text).whenComplete { _, failure ->
                            finishSpeech(result, ready, token, failure)
                        }
                    } catch (failure: Exception) { finishSpeech(result, ready, token, failure) }
                }
            }
        }
        return result
    }

    private fun finishSpeech(result: CompletableFuture<Unit>, ready: LocalTtsState, token: Long, failure: Throwable?) {
        synchronized(lock) {
            if (active.compareAndSet(result, null) && token == generation.get() && !closed.get()) {
                current = ready.copy(phase = if (failure == null) LocalTtsPhase.READY else LocalTtsPhase.FAILED,
                    failure = if (failure == null) null else "Offline speech failed. Check Android speech settings.")
            }
        }
        if (failure == null) result.complete(Unit) else result.completeExceptionally(failure)
    }

    override fun stop() {
        generation.incrementAndGet()
        active.getAndSet(null)?.completeExceptionally(CancellationException("Speech stopped"))
        if (current.phase == LocalTtsPhase.SPEAKING) current = current.copy(phase = LocalTtsPhase.READY)
        synchronized(lock) {
            if (!worker.isShutdown && stopQueued.compareAndSet(false, true)) worker.execute {
                try { backend.stop() } finally { stopQueued.set(false) }
            }
        }
    }

    override fun close() = synchronized(lock) {
        if (closed.getAndSet(true)) return@synchronized
        stop()
        initialization?.completeExceptionally(CancellationException("Speech is closed"))
        current = LocalTtsState(LocalTtsPhase.STOPPED)
        worker.execute { backend.close() }
        worker.shutdown()
    }

    private fun unavailable() = LocalTtsState(LocalTtsPhase.UNAVAILABLE,
        failure = "Speech engine did not respond. Check Android speech settings, then refresh voices.")
}
