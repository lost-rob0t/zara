package ai.zara.app.runtime

import ai.zara.app.model.LocalModelConfig
import ai.zara.app.model.LocalModelConfigStore
import ai.zara.app.model.LocalModelCoordinator
import ai.zara.app.model.LocalModelPhase
import ai.zara.app.model.LocalModelState
import ai.zara.app.prolog.PrologQueryPolicy
import ai.zara.app.prolog.PrologWorkspace
import ai.zara.app.prolog.TreallaBridge
import java.io.File
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

enum class LocalServerPhase { STOPPED, STARTING, READY, RELOADING, FAILED }

data class LocalServerState(
    val phase: LocalServerPhase,
    val generation: Long,
    val loadedSources: List<String>,
    val failure: String? = null,
)

data class LocalQueryResult(
    val query: String,
    val terms: List<String>,
    val generation: Long,
)

class LocalZaraServer(
    private val bridge: TreallaBridge,
    private val corePath: String,
    private val workspace: PrologWorkspace,
    private val localModel: LocalModelCoordinator = defaultLocalModelCoordinator(corePath),
) : AutoCloseable {
    private val actor: ExecutorService = Executors.newSingleThreadExecutor { runnable ->
        Thread(runnable, "zara-local-server").apply { isDaemon = true }
    }
    @Volatile
    private var current = LocalServerState(LocalServerPhase.STOPPED, 0, emptyList())
    @Volatile
    private var closed = false
    @Volatile
    private var stateObserver: ((LocalServerState) -> Unit)? = null

    fun state(): LocalServerState = current

    fun localModelState(): LocalModelState = localModel.state()

    fun setStateObserver(observer: ((LocalServerState) -> Unit)?) {
        stateObserver = observer
        observer?.invoke(current)
    }

    fun start(): CompletableFuture<LocalServerState> = submit {
        check(current.phase == LocalServerPhase.STOPPED) { "Local Zara server is already started" }
        boot(LocalServerPhase.STARTING)
    }

    fun reload(): CompletableFuture<LocalServerState> = submit {
        check(current.phase == LocalServerPhase.READY || current.phase == LocalServerPhase.FAILED) {
            "Local Zara server is not reloadable"
        }
        val wasReady = current.phase == LocalServerPhase.READY
        updateState(current.copy(phase = LocalServerPhase.RELOADING, failure = null))
        if (wasReady) {
            try {
                bridge.shutdown()
            } catch (error: Throwable) {
                return@submit LocalServerState(
                    phase = LocalServerPhase.FAILED,
                    generation = current.generation,
                    loadedSources = emptyList(),
                    failure = error.message ?: "Local runtime shutdown failed",
                ).also(::updateState)
            }
        }
        boot(LocalServerPhase.RELOADING)
    }

    fun query(rawQuery: String): CompletableFuture<LocalQueryResult> {
        val query = try {
            PrologQueryPolicy.requireSafe(rawQuery)
        } catch (error: Throwable) {
            return CompletableFuture.failedFuture(error)
        }
        return submit {
            check(current.phase == LocalServerPhase.READY) { "Local Zara server is not ready" }
            LocalQueryResult(query, bridge.evaluate(query), current.generation)
        }
    }

    fun resolve(utterance: String): CompletableFuture<LocalQueryResult> {
        val text = utterance.trim()
        require(text.isNotEmpty()) { "Utterance is required" }
        require(text.length <= 8_192) { "Utterance is too large" }
        modelCommand(text)?.let { return it }
        val escaped = text
            .replace("\\", "\\\\")
            .replace("\"", "\\\"")
            .replace("\n", "\\n")
        val query = "resolve_frames(\"$escaped\", passive, [], Frames), member(Result, Frames)"
        val symbolic = submit {
            check(current.phase == LocalServerPhase.READY) { "Local Zara server is not ready" }
            LocalQueryResult(query, bridge.evaluate(query), current.generation)
        }
        return symbolic.thenCompose { result ->
            if (result.terms.isNotEmpty() || !localModel.state().config.enabled) {
                CompletableFuture.completedFuture(result)
            } else {
                localModel.generate(text).handle { modelResult, error ->
                    when {
                        modelResult != null -> result.copy(terms = listOf(modelResult.text))
                        error != null -> result.copy(
                            terms = listOf(
                                "Local model unavailable: ${rootMessage(error)}. " +
                                    "The symbolic runtime is still ready.",
                            ),
                        )
                        else -> result
                    }
                }
            }
        }
    }

    private fun modelCommand(text: String): CompletableFuture<LocalQueryResult>? {
        if (text != "/model" && !text.startsWith("/model ")) return null
        return try {
            val arguments = text.split(Regex("\\s+"), limit = 6)
            val message = when (arguments.getOrNull(1)?.lowercase() ?: "status") {
                "status" -> describeModelState(localModel.state())
                "on" -> {
                    val state = localModel.configure(localModel.state().config.copy(enabled = true))
                    "Local model enabled: ${describeModelState(state)}"
                }
                "off" -> {
                    val state = localModel.configure(localModel.state().config.copy(enabled = false))
                    "Local model disabled: ${describeModelState(state)}"
                }
                "cancel" -> {
                    localModel.cancelActive()
                    "Local model generation cancelled"
                }
                "use" -> {
                    require(arguments.size >= 4) {
                        "Usage: /model use <loopback-endpoint> <model> [quantization]"
                    }
                    val previous = localModel.state().config
                    val state = localModel.configure(
                        previous.copy(
                            enabled = true,
                            endpoint = arguments[2],
                            model = arguments[3],
                            quantization = arguments.getOrNull(4),
                        )
                    )
                    "Local model configured: ${describeModelState(state)}"
                }
                else -> error(
                    "Unknown local model command. Use /model status, /model on, /model off, " +
                        "/model cancel, or /model use <loopback-endpoint> <model> [quantization]"
                )
            }
            CompletableFuture.completedFuture(
                LocalQueryResult(
                    query = text,
                    terms = listOf(message),
                    generation = current.generation,
                )
            )
        } catch (error: Throwable) {
            CompletableFuture.failedFuture(error)
        }
    }

    private fun describeModelState(state: LocalModelState): String {
        val config = state.config
        val quantization = config.quantization?.let { " · $it" }.orEmpty()
        val failure = state.message?.let { " · $it" }.orEmpty()
        return "${state.phase.name.lowercase()} · ${config.model}$quantization · ${config.endpoint}$failure"
    }

    private fun boot(phase: LocalServerPhase): LocalServerState {
        updateState(current.copy(phase = phase, failure = null))
        return try {
            bridge.initialize(corePath)
            val sources = workspace.sourceFiles()
            sources.forEach { bridge.consult(it.absolutePath) }
            LocalServerState(
                phase = LocalServerPhase.READY,
                generation = current.generation + 1,
                loadedSources = sources.map { it.name },
            ).also(::updateState)
        } catch (error: Throwable) {
            runCatching { bridge.shutdown() }
            LocalServerState(
                phase = LocalServerPhase.FAILED,
                generation = current.generation,
                loadedSources = emptyList(),
                failure = error.message ?: error::class.java.simpleName,
            ).also(::updateState)
        }
    }

    private fun updateState(state: LocalServerState) {
        current = state
        stateObserver?.invoke(state)
    }

    private fun <T> submit(block: () -> T): CompletableFuture<T> {
        if (closed) return CompletableFuture.failedFuture(IllegalStateException("Local Zara server is closed"))
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
        closed = true
        localModel.close()
        val future = CompletableFuture<Unit>()
        actor.execute {
            try {
                if (current.phase != LocalServerPhase.STOPPED) bridge.shutdown()
                updateState(current.copy(phase = LocalServerPhase.STOPPED))
                future.complete(Unit)
            } catch (error: Throwable) {
                future.completeExceptionally(error)
            }
        }
        runCatching { future.get() }
        actor.shutdownNow()
        stateObserver = null
    }

    companion object {
        private fun defaultLocalModelCoordinator(corePath: String): LocalModelCoordinator {
            val root = File(corePath).parentFile ?: error("Local runtime directory is unavailable")
            return LocalModelCoordinator(
                LocalModelConfigStore(File(root, "local-model.properties"))
            )
        }

        private fun rootMessage(error: Throwable): String {
            var current: Throwable = error
            while (current.cause != null && current.cause !== current) {
                current = current.cause!!
            }
            return current.message ?: current::class.java.simpleName
        }
    }
}
