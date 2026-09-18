package ai.zara.app.localai

import java.util.concurrent.CancellationException
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

class LocalAiRuntime(
    private val backend: LocalLlmBackend,
) : AutoCloseable {
    private val actor: ExecutorService = Executors.newSingleThreadExecutor { runnable ->
        Thread(runnable, "zara-local-ai").apply { isDaemon = true }
    }

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
        actor.execute {
            stateObserver = observer
            observer?.invoke(current)
        }
    }

    fun load(spec: LocalModelSpec): CompletableFuture<LocalAiState> = submit {
        check(current.phase != LocalAiPhase.GENERATING) { "Local model is generating" }
        if (current.model == spec && current.phase == LocalAiPhase.READY) return@submit current
        unloadBackend()
        update(current.copy(phase = LocalAiPhase.LOADING, model = null, failure = null))
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
                failure = boundedMessage(error),
            ).also(::update)
            throw error
        }
    }

    fun generate(
        request: LocalGenerationRequest,
        onChunk: (String) -> Unit = {},
    ): CompletableFuture<LocalGenerationResult> {
        if (closed) return failed(IllegalStateException("Local AI runtime is closed"))
        val result = CompletableFuture<LocalGenerationResult>()
        actor.execute {
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
                            actor.execute { acceptChunk(text) }
                        }

                        override fun onDone() {
                            actor.execute { finishGeneration(spec) }
                        }

                        override fun onError(error: Throwable) {
                            actor.execute { failGeneration(error) }
                        }
                    },
                )
            } catch (error: Throwable) {
                activeFuture = null
                activeChunkObserver = null
                result.completeExceptionally(error)
            }
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
        if (closed) return failed(IllegalStateException("Local AI runtime is closed"))
        val future = CompletableFuture<T>()
        actor.execute {
            try {
                future.complete(block())
            } catch (error: Throwable) {
                future.completeExceptionally(error)
            }
        }
        return future
    }

    override fun close() {
        if (closed) return
        val done = CompletableFuture<Unit>()
        actor.execute {
            try {
                cancelActive("Local AI runtime closed")
                unloadBackend()
                runCatching { backend.close() }
                update(LocalAiState(LocalAiPhase.STOPPED, current.generation + 1))
                stateObserver = null
                done.complete(Unit)
            } catch (error: Throwable) {
                done.completeExceptionally(error)
            }
        }
        runCatching { done.get() }
        closed = true
        actor.shutdownNow()
    }

    private fun boundedMessage(error: Throwable): String =
        (error.message ?: error::class.java.simpleName).take(256)

    private fun <T> failed(error: Throwable): CompletableFuture<T> =
        CompletableFuture<T>().also { it.completeExceptionally(error) }
}
