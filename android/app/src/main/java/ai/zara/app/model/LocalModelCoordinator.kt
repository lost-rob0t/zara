package ai.zara.app.model

import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicBoolean

enum class LocalModelPhase { DISABLED, READY, GENERATING, DEGRADED }

data class LocalModelState(
    val phase: LocalModelPhase,
    val config: LocalModelConfig,
    val generation: Long,
    val lastFailure: LocalModelFailureReason? = null,
    val message: String? = null,
)

class LocalModelCoordinator(
    private val store: LocalModelConfigStore,
    private val backend: LocalModelBackend = LoopbackOpenAiLocalModelBackend(),
) : AutoCloseable {
    private val actor: ExecutorService = Executors.newSingleThreadExecutor { runnable ->
        Thread(runnable, "zara-local-model").apply { isDaemon = true }
    }
    @Volatile
    private var current = store.load().let { config ->
        LocalModelState(
            phase = if (config.enabled) LocalModelPhase.READY else LocalModelPhase.DISABLED,
            config = config,
            generation = 0,
        )
    }
    @Volatile
    private var activeCancellation: AtomicBoolean? = null
    @Volatile
    private var observer: ((LocalModelState) -> Unit)? = null
    @Volatile
    private var closed = false

    fun state(): LocalModelState = current

    fun setObserver(value: ((LocalModelState) -> Unit)?) {
        observer = value
        value?.invoke(current)
    }

    fun configure(config: LocalModelConfig): LocalModelState {
        check(!closed) { "Local model coordinator is closed" }
        val safe = store.save(config)
        cancelActive()
        return LocalModelState(
            phase = if (safe.enabled) LocalModelPhase.READY else LocalModelPhase.DISABLED,
            config = safe,
            generation = current.generation + 1,
        ).also(::update)
    }

    fun generate(
        prompt: String,
        systemPrompt: String = LocalModelRequest.DEFAULT_SYSTEM_PROMPT,
        onText: (String) -> Unit = {},
    ): CompletableFuture<LocalModelResult> {
        val snapshot = current
        if (!snapshot.config.enabled) {
            return CompletableFuture.failedFuture(
                LocalModelException(LocalModelFailureReason.DISABLED, "Local model is disabled"),
            )
        }
        if (closed) {
            return CompletableFuture.failedFuture(
                IllegalStateException("Local model coordinator is closed"),
            )
        }
        val cancellation = AtomicBoolean(false)
        val future = CompletableFuture<LocalModelResult>()
        actor.execute {
            if (current.generation != snapshot.generation || !current.config.enabled) {
                future.completeExceptionally(
                    LocalModelException(
                        LocalModelFailureReason.CANCELLED,
                        "Local model configuration changed before generation started",
                    )
                )
                return@execute
            }
            activeCancellation = cancellation
            update(
                current.copy(
                    phase = LocalModelPhase.GENERATING,
                    lastFailure = null,
                    message = null,
                )
            )
            try {
                val result = backend.generate(
                    config = snapshot.config,
                    request = LocalModelRequest(
                        prompt = prompt,
                        maxOutputTokens = snapshot.config.maxOutputTokens,
                        deadlineMs = snapshot.config.deadlineMs,
                        systemPrompt = systemPrompt,
                    ),
                    cancelled = cancellation::get,
                    onText = onText,
                )
                if (
                    cancellation.get() ||
                    current.generation != snapshot.generation ||
                    !current.config.enabled
                ) {
                    throw LocalModelException(
                        LocalModelFailureReason.CANCELLED,
                        "Local model result became stale",
                    )
                }
                update(
                    current.copy(
                        phase = LocalModelPhase.READY,
                        lastFailure = null,
                        message = null,
                    )
                )
                future.complete(result)
            } catch (error: Throwable) {
                val failure = when (error) {
                    is LocalModelException -> error
                    else -> LocalModelException(
                        LocalModelFailureReason.UNAVAILABLE,
                        error.message ?: "Local model failed",
                        error,
                    )
                }
                if (current.generation == snapshot.generation) {
                    update(
                        current.copy(
                            phase = if (current.config.enabled) {
                                LocalModelPhase.DEGRADED
                            } else {
                                LocalModelPhase.DISABLED
                            },
                            lastFailure = failure.reason,
                            message = failure.message,
                        )
                    )
                }
                future.completeExceptionally(failure)
            } finally {
                if (activeCancellation === cancellation) activeCancellation = null
            }
        }
        return future
    }

    fun cancelActive() {
        activeCancellation?.set(true)
        backend.cancel()
    }

    private fun update(state: LocalModelState) {
        current = state
        observer?.invoke(state)
    }

    override fun close() {
        if (closed) return
        closed = true
        cancelActive()
        observer = null
        backend.close()
        actor.shutdownNow()
    }
}
