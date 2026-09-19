package ai.zara.app.localai

import java.util.concurrent.CancellationException
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

class LocalAiRuntime(
    private val backend: LocalLlmBackend,
) : AutoCloseable {
    @Volatile
    private var actorThread: Thread? = null

    private val actor: ExecutorService = Executors.newSingleThreadExecutor { runnable ->
        Thread(runnable, "zara-local-ai").apply {
            isDaemon = true
            actorThread = this
        }
    }
    private val lifecycleLock = Any()
    private val closeDone = CompletableFuture<Unit>()

    @Volatile
    private var current = LocalAiState()

    @Volatile
    private var closed = false

    private var activeSession: LocalGenerationSession? = null
    private var activeFuture: CompletableFuture<LocalGenerationResult>? = null
    private var activeText = StringBuilder()
    private var activeChunkObserver: ((String) -> Unit)? = null
    private var stateObserver: ((LocalAiState) -> Unit)? = null

    fun state(): LocalAiState = current

    fun setStateObserver(observer: ((LocalAiState) -> Unit)?) {
        enqueueIfOpen {
            stateObserver = observer
            observer?.invoke(current)
        }
    }

    fun load(spec: LocalModelSpec): CompletableFuture<LocalAiState> = submit {
        check(current.phase != LocalAiPhase.GENERATING) { "Local model is generating" }
        if (current.model == spec && current.phase == LocalAiPhase.READY) return@submit current
        unloadBackend()
        update(current.copy(phase = LocalAiPhase.LOADING, model = spec, failure = null))
        try {
            backend.load(spec)
            LocalAiState(
                phase = LocalAiPhase.READY,
                generation = current.generation + 1,
                model = spec,
            ).also(::update)
        } catch (error: Throwable) {
            runCatching { backend.unload() }
            LocalAiState(
                phase = LocalAiPhase.FAILED,
                generation = current.generation,
                model = spec,
                failure = boundedMessage(error),
            ).also(::update)
            throw error
        }
    }

    fun generate(
        request: LocalGenerationRequest,
        onChunk: (String) -> Unit = {},
    ): CompletableFuture<LocalGenerationResult> {
        val result = CompletableFuture<LocalGenerationResult>()
        val accepted = enqueueIfOpen {
            try {
                check(current.phase == LocalAiPhase.READY) { "Local model is not ready" }
                check(activeFuture == null) { "A local generation is already active" }
                val spec = checkNotNull(current.model) { "Local model metadata is missing" }
                activeText = StringBuilder()
                activeFuture = result
                activeChunkObserver = onChunk
                update(current.copy(phase = LocalAiPhase.GENERATING, failure = null))
                activeSession = backend.generate(
                    request,
                    object : LocalGenerationListener {
                        override fun onChunk(text: String) {
                            enqueueIfOpen { acceptChunk(text) }
                        }

                        override fun onDone() {
                            enqueueIfOpen { finishGeneration(spec) }
                        }

                        override fun onError(error: Throwable) {
                            enqueueIfOpen { failGeneration(error) }
                        }
                    },
                )
            } catch (error: Throwable) {
                activeFuture = null
                activeChunkObserver = null
                result.completeExceptionally(error)
            }
        }
        if (!accepted) {
            result.completeExceptionally(IllegalStateException("Local AI runtime is closed"))
        }
        return result
    }

    fun cancel(): CompletableFuture<LocalAiState> = submit {
        val future = activeFuture ?: return@submit current
        val session = activeSession
        activeSession = null
        activeFuture = null
        activeChunkObserver = null
        activeText = StringBuilder()
        runCatching { session?.cancel() }
        runCatching { session?.close() }
        future.completeExceptionally(CancellationException("Local generation cancelled"))
        current.copy(phase = LocalAiPhase.READY, failure = null).also(::update)
    }

    fun unload(): CompletableFuture<LocalAiState> = submit {
        cancelActive("Local model unloaded")
        unloadBackend()
        LocalAiState(
            phase = LocalAiPhase.STOPPED,
            generation = current.generation + 1,
        ).also(::update)
    }

    private fun acceptChunk(text: String) {
        if (activeFuture == null || current.phase != LocalAiPhase.GENERATING) return
        activeText.append(text)
        runCatching { activeChunkObserver?.invoke(text) }
    }

    private fun finishGeneration(spec: LocalModelSpec) {
        val future = activeFuture ?: return
        val text = activeText.toString()
        clearActiveSession()
        update(current.copy(phase = LocalAiPhase.READY, failure = null))
        future.complete(
            LocalGenerationResult(
                text = text,
                modelId = spec.id,
                modelVersion = spec.version,
                quantization = spec.quantization,
                generation = current.generation,
            )
        )
    }

    private fun failGeneration(error: Throwable) {
        val future = activeFuture ?: return
        clearActiveSession()
        update(current.copy(phase = LocalAiPhase.READY, failure = boundedMessage(error)))
        future.completeExceptionally(error)
    }

    private fun clearActiveSession() {
        val session = activeSession
        activeSession = null
        activeFuture = null
        activeChunkObserver = null
        activeText = StringBuilder()
        runCatching { session?.close() }
    }

    private fun cancelActive(reason: String) {
        val future = activeFuture ?: return
        val session = activeSession
        activeSession = null
        activeFuture = null
        activeChunkObserver = null
        activeText = StringBuilder()
        runCatching { session?.cancel() }
        runCatching { session?.close() }
        future.completeExceptionally(CancellationException(reason))
    }

    private fun unloadBackend() {
        if (current.model != null || current.phase != LocalAiPhase.STOPPED) {
            runCatching { backend.unload() }
        }
    }

    private fun update(state: LocalAiState) {
        current = state
        runCatching { stateObserver?.invoke(state) }
    }

    private fun <T> submit(block: () -> T): CompletableFuture<T> {
        val future = CompletableFuture<T>()
        val accepted = enqueueIfOpen {
            try {
                future.complete(block())
            } catch (error: Throwable) {
                future.completeExceptionally(error)
            }
        }
        if (!accepted) {
            future.completeExceptionally(IllegalStateException("Local AI runtime is closed"))
        }
        return future
    }

    override fun close() {
        synchronized(lifecycleLock) {
            if (!closed) {
                closed = true
                actor.execute {
                    try {
                        cancelActive("Local AI runtime closed")
                        unloadBackend()
                        runCatching { backend.close() }
                        update(LocalAiState(LocalAiPhase.STOPPED, current.generation + 1))
                        stateObserver = null
                        actor.shutdown()
                        closeDone.complete(Unit)
                    } catch (error: Throwable) {
                        actor.shutdown()
                        closeDone.completeExceptionally(error)
                    }
                }
            }
        }
        if (Thread.currentThread() === actorThread) return
        runCatching { closeDone.get() }
    }

    private fun enqueueIfOpen(block: () -> Unit): Boolean = synchronized(lifecycleLock) {
        if (closed) {
            false
        } else {
            actor.execute(block)
            true
        }
    }

    private fun boundedMessage(error: Throwable): String =
        (error.message ?: error::class.java.simpleName).take(256)
}
