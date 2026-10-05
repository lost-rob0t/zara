package ai.zara.app.model

import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicBoolean

enum class CloudModelPhase { DISABLED, READY, GENERATING, DEGRADED }

data class CloudModelState(
    val phase: CloudModelPhase,
    val config: CloudModelConfig,
    val generation: Long,
    val apiKeyConfigured: Boolean,
    val lastFailure: CloudModelFailureReason? = null,
    val message: String? = null,
)

class CloudModelCoordinator(
    private val configStore: CloudModelConfigStore,
    private val apiKeyStore: CloudApiKeyStore,
    private val backend: CloudModelBackend = OpenAiCompatibleCloudModelBackend(),
) : AutoCloseable {
    private val actor: ExecutorService = Executors.newSingleThreadExecutor { runnable ->
        Thread(runnable, "zara-cloud-model").apply { isDaemon = true }
    }
    @Volatile
    private var current = initialState(configStore.load(), generation = 0)
    @Volatile
    private var activeCancellation: AtomicBoolean? = null
    @Volatile
    private var observer: ((CloudModelState) -> Unit)? = null
    @Volatile
    private var closed = false

    fun state(): CloudModelState = current

    fun setObserver(value: ((CloudModelState) -> Unit)?) {
        observer = value
        value?.invoke(current)
    }

    fun reloadConfig(): CloudModelState {
        check(!closed) { "Cloud model coordinator is closed" }
        val loaded = configStore.load()
        if (loaded == current.config && apiKeyStore.exists() == current.apiKeyConfigured) return current
        cancelActive()
        return initialState(loaded, current.generation + 1).also(::update)
    }

    fun configure(config: CloudModelConfig): CloudModelState {
        check(!closed) { "Cloud model coordinator is closed" }
        val safe = configStore.save(config)
        cancelActive()
        return initialState(safe, current.generation + 1).also(::update)
    }

    fun setApiKey(apiKey: String): CloudModelState {
        check(!closed) { "Cloud model coordinator is closed" }
        apiKeyStore.save(apiKey)
        cancelActive()
        return initialState(configStore.load(), current.generation + 1).also(::update)
    }

    fun clearApiKey(): CloudModelState {
        check(!closed) { "Cloud model coordinator is closed" }
        apiKeyStore.clear()
        cancelActive()
        return initialState(configStore.load(), current.generation + 1).also(::update)
    }

    fun generate(
        prompt: String,
        purpose: CloudModelPurpose,
        effectiveAppName: String? = null,
        onText: (String) -> Unit = {},
    ): CompletableFuture<CloudModelResult> {
        val refreshed = try {
            reloadConfig()
        } catch (error: Throwable) {
            return CompletableFuture.failedFuture(error)
        }
        if (!refreshed.config.enabled) {
            return CompletableFuture.failedFuture(
                CloudModelException(CloudModelFailureReason.DISABLED, "Cloud model is disabled"),
            )
        }
        if (refreshed.config.provider.codingOnly && purpose != CloudModelPurpose.CODING) {
            return CompletableFuture.failedFuture(
                CloudModelException(
                    CloudModelFailureReason.UNSUPPORTED,
                    "${refreshed.config.provider.wireName} is coding-only; use an explicit coding request",
                )
            )
        }
        val appName = effectiveAppName?.trim()?.takeIf(String::isNotEmpty) ?: refreshed.config.appName
        require(appName.length <= CloudModelConfig.MAX_APP_NAME_CHARS) { "LLM app name is too long" }
        val snapshot = refreshed
        val cancellation = AtomicBoolean(false)
        val future = CompletableFuture<CloudModelResult>()
        actor.execute {
            if (current.generation != snapshot.generation || !current.config.enabled) {
                future.completeExceptionally(
                    CloudModelException(
                        CloudModelFailureReason.CANCELLED,
                        "Cloud model configuration changed before generation started",
                    )
                )
                return@execute
            }
            activeCancellation = cancellation
            update(
                current.copy(
                    phase = CloudModelPhase.GENERATING,
                    lastFailure = null,
                    message = null,
                )
            )
            try {
                val apiKey = apiKeyStore.load()
                    ?: throw CloudModelException(
                        CloudModelFailureReason.AUTHENTICATION,
                        "Cloud model API key is not configured",
                    )
                val result = backend.generate(
                    config = snapshot.config,
                    apiKey = apiKey,
                    request = CloudModelRequest(
                        prompt = prompt,
                        purpose = purpose,
                        maxOutputTokens = snapshot.config.maxOutputTokens,
                        deadlineMs = snapshot.config.deadlineMs,
                        appName = appName,
                    ),
                    cancelled = cancellation::get,
                    onText = onText,
                )
                if (
                    cancellation.get() ||
                    current.generation != snapshot.generation ||
                    !current.config.enabled
                ) {
                    throw CloudModelException(
                        CloudModelFailureReason.CANCELLED,
                        "Cloud model result became stale",
                    )
                }
                update(
                    current.copy(
                        phase = CloudModelPhase.READY,
                        lastFailure = null,
                        message = null,
                    )
                )
                future.complete(result)
            } catch (error: Throwable) {
                val failure = when (error) {
                    is CloudModelException -> error
                    else -> CloudModelException(
                        CloudModelFailureReason.UNAVAILABLE,
                        error.message ?: "Cloud model failed",
                        error,
                    )
                }
                if (current.generation == snapshot.generation) {
                    update(
                        current.copy(
                            phase = if (current.config.enabled) CloudModelPhase.DEGRADED else CloudModelPhase.DISABLED,
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

    private fun update(state: CloudModelState) {
        current = state
        observer?.invoke(state)
    }

    private fun initialState(config: CloudModelConfig, generation: Long) = CloudModelState(
        phase = if (config.enabled) CloudModelPhase.READY else CloudModelPhase.DISABLED,
        config = config,
        generation = generation,
        apiKeyConfigured = apiKeyStore.exists(),
    )

    override fun close() {
        if (closed) return
        closed = true
        cancelActive()
        observer = null
        backend.close()
        actor.shutdownNow()
    }
}
