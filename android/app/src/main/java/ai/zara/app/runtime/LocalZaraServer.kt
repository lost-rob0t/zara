package ai.zara.app.runtime

import ai.zara.app.policy.PolicyAdvice
import ai.zara.app.policy.PolicyWire
import ai.zara.app.prolog.PrologQueryPolicy
import ai.zara.app.prolog.PrologWorkspace
import ai.zara.app.prolog.TreallaBridge
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
    private val policyPath: String? = null,
    private val diagnostics: (String, Map<String, Any?>, Throwable?) -> Unit = { _, _, _ -> },
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

    fun setStateObserver(observer: ((LocalServerState) -> Unit)?) {
        stateObserver = observer
        observer?.invoke(current)
    }

    fun start(): CompletableFuture<LocalServerState> = submit {
        check(current.phase == LocalServerPhase.STOPPED) { "Local Zara server is already started" }
        diagnostics(
            "local_server.start",
            mapOf("generation" to current.generation),
            null,
        )
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
            diagnostics(
                "local_server.query.begin",
                mapOf("query_length" to query.length, "generation" to current.generation),
                null,
            )
            try {
                val terms = bridge.evaluate(query)
                diagnostics(
                    "local_server.query.complete",
                    mapOf("terms" to terms.size, "generation" to current.generation),
                    null,
                )
                LocalQueryResult(query, terms, current.generation)
            } catch (error: Throwable) {
                diagnostics(
                    "local_server.query.failed",
                    mapOf("generation" to current.generation),
                    error,
                )
                throw error
            }
        }
    }

    fun inspectPolicy(text: String): CompletableFuture<PolicyAdvice> {
        val query = try {
            PolicyWire.query(text)
        } catch (error: IllegalArgumentException) {
            return CompletableFuture.failedFuture(error)
        }
        return submit {
            check(current.phase == LocalServerPhase.READY) { "Local Zara server is not ready" }
            check(policyPath != null) { "Local output policy is not installed" }
            diagnostics(
                "local_server.policy.begin",
                mapOf("text_length" to text.length, "generation" to current.generation),
                null,
            )
            try {
                val advice = PolicyWire.decode(bridge.evaluate(query), current.generation)
                diagnostics(
                    "local_server.policy.complete",
                    mapOf("generation" to current.generation),
                    null,
                )
                advice
            } catch (error: Throwable) {
                diagnostics(
                    "local_server.policy.failed",
                    mapOf("generation" to current.generation),
                    error,
                )
                throw error
            }
        }
    }

    fun resolve(utterance: String): CompletableFuture<LocalQueryResult> {
        val text = utterance.trim()
        require(text.isNotEmpty()) { "Utterance is required" }
        require(text.length <= 8_192) { "Utterance is too large" }
        val escaped = text
            .replace("\\", "\\\\")
            .replace("\"", "\\\"")
            .replace("\n", "\\n")
        val query = "resolve_frames(\"$escaped\", passive, [], Frames), member(Result, Frames)"
        return submit {
            check(current.phase == LocalServerPhase.READY) { "Local Zara server is not ready" }
            diagnostics(
                "local_server.resolve.begin",
                mapOf("utterance_length" to text.length, "generation" to current.generation),
                null,
            )
            try {
                val terms = bridge.evaluate(query)
                diagnostics(
                    "local_server.resolve.complete",
                    mapOf("terms" to terms.size, "generation" to current.generation),
                    null,
                )
                LocalQueryResult(query, terms, current.generation)
            } catch (error: Throwable) {
                diagnostics(
                    "local_server.resolve.failed",
                    mapOf("generation" to current.generation),
                    error,
                )
                throw error
            }
        }
    }

    private fun boot(phase: LocalServerPhase): LocalServerState {
        updateState(current.copy(phase = phase, failure = null))
        diagnostics(
            "local_server.boot.begin",
            mapOf("phase" to phase.name.lowercase(), "generation" to current.generation),
            null,
        )
        return try {
            bridge.initialize(corePath)
            diagnostics("local_server.native.ready", emptyMap(), null)
            policyPath?.let { path ->
                diagnostics("local_server.policy.consult.begin", mapOf("path" to path), null)
                bridge.consult(path)
                diagnostics("local_server.policy.consult.complete", mapOf("path" to path), null)
            }
            val sources = workspace.sourceFiles()
            sources.forEachIndexed { index, source ->
                diagnostics(
                    "local_server.consult.begin",
                    mapOf("source" to source.name, "index" to index, "total" to sources.size),
                    null,
                )
                bridge.consult(source.absolutePath)
                diagnostics(
                    "local_server.consult.complete",
                    mapOf("source" to source.name, "index" to index),
                    null,
                )
            }
            LocalServerState(
                phase = LocalServerPhase.READY,
                generation = current.generation + 1,
                loadedSources = sources.map { it.name },
            ).also { state ->
                updateState(state)
                diagnostics(
                    "local_server.ready",
                    mapOf(
                        "generation" to state.generation,
                        "sources" to state.loadedSources.size,
                    ),
                    null,
                )
            }
        } catch (error: Throwable) {
            diagnostics(
                "local_server.boot.failed",
                mapOf("phase" to phase.name.lowercase(), "generation" to current.generation),
                error,
            )
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
        val task = actor.submit {
            if (future.isCancelled) return@submit
            try {
                future.complete(block())
            } catch (error: Throwable) {
                future.completeExceptionally(error)
            }
        }
        future.whenComplete { _, _ ->
            if (future.isCancelled) task.cancel(false)
        }
        if (future.isCancelled) task.cancel(false)
        return future
    }

    override fun close() {
        if (closed) return
        closed = true
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
}
