package ai.zara.app.runtime

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
    private val diagnostics: (String, Map<String, Any?>, Throwable?) -> Unit = { _, _, _ -> },
) : AutoCloseable {
    companion object {
        private const val READINESS_QUERY = "Result = zara_ready"
        private const val READINESS_RESULT = "zara_ready"
    }

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
                mapOf(
                    "query_length" to query.length,
                    "generation" to current.generation,
                    "phase" to current.phase.name.lowercase(),
                    "result_binding_requested" to query.contains("Result"),
                ),
                null,
            )
            try {
                val terms = bridge.evaluate(query)
                diagnostics(
                    "local_server.query.complete",
                    mapOf(
                        "terms" to terms.size,
                        "query_length" to query.length,
                        "generation" to current.generation,
                        "phase" to current.phase.name.lowercase(),
                    ),
                    null,
                )
                LocalQueryResult(query, terms, current.generation)
            } catch (error: Throwable) {
                diagnostics(
                    "local_server.query.failed",
                    mapOf(
                        "query_length" to query.length,
                        "generation" to current.generation,
                        "phase" to current.phase.name.lowercase(),
                    ),
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
                mapOf(
                    "utterance_length" to text.length,
                    "query_length" to query.length,
                    "generation" to current.generation,
                    "phase" to current.phase.name.lowercase(),
                    "route" to "resolve_frames",
                ),
                null,
            )
            try {
                val terms = bridge.evaluate(query)
                diagnostics(
                    "local_server.resolve.complete",
                    mapOf(
                        "terms" to terms.size,
                        "utterance_length" to text.length,
                        "query_length" to query.length,
                        "generation" to current.generation,
                        "phase" to current.phase.name.lowercase(),
                    ),
                    null,
                )
                LocalQueryResult(query, terms, current.generation)
            } catch (error: Throwable) {
                diagnostics(
                    "local_server.resolve.failed",
                    mapOf(
                        "utterance_length" to text.length,
                        "query_length" to query.length,
                        "generation" to current.generation,
                        "phase" to current.phase.name.lowercase(),
                    ),
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
            verifyReadiness(sources.size)
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
                        "self_test" to "passed",
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

    private fun verifyReadiness(sourceCount: Int) {
        diagnostics(
            "local_server.self_test.begin",
            mapOf(
                "generation" to current.generation,
                "query_length" to READINESS_QUERY.length,
                "sources" to sourceCount,
            ),
            null,
        )
        try {
            val terms = bridge.evaluate(READINESS_QUERY)
            check(terms == listOf(READINESS_RESULT)) {
                "Local Prolog readiness probe returned ${terms.size} unexpected result(s)"
            }
            diagnostics(
                "local_server.self_test.complete",
                mapOf(
                    "generation" to current.generation,
                    "query_length" to READINESS_QUERY.length,
                    "terms" to terms.size,
                    "result" to READINESS_RESULT,
                ),
                null,
            )
        } catch (error: Throwable) {
            diagnostics(
                "local_server.self_test.failed",
                mapOf(
                    "generation" to current.generation,
                    "query_length" to READINESS_QUERY.length,
                    "sources" to sourceCount,
                ),
                error,
            )
            throw IllegalStateException("Local Prolog readiness probe failed", error)
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
