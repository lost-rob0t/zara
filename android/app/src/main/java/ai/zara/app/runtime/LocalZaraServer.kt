package ai.zara.app.runtime

import ai.zara.app.prolog.PrologAuthorityPolicy
import ai.zara.app.prolog.PrologQueryPolicy
import ai.zara.app.prolog.PrologWorkspace
import ai.zara.app.prolog.TreallaBridge
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicLong

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
    val cancelled: Boolean = false,
)

class LocalZaraServer(
    private val bridge: TreallaBridge,
    private val corePath: String,
    private val workspace: PrologWorkspace,
) : AutoCloseable {
    private val actor: ExecutorService = Executors.newSingleThreadExecutor { runnable ->
        Thread(runnable, "zara-local-server").apply { isDaemon = true }
    }
    private val queryEpoch = AtomicLong(0)
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
        boot(LocalServerPhase.STARTING)
    }

    fun reload(): CompletableFuture<LocalServerState> = submit {
        check(current.phase == LocalServerPhase.READY || current.phase == LocalServerPhase.FAILED) {
            "Local Zara server is not reloadable"
        }

        // Reject statically-invalid workspace edits before destroying the last-good runtime.
        if (current.phase == LocalServerPhase.READY) {
            val validationError = runCatching {
                PrologAuthorityPolicy.requireSafeWorkspace(workspace.listSources())
            }.exceptionOrNull()
            if (validationError != null) {
                return@submit LocalServerState(
                    phase = LocalServerPhase.FAILED,
                    generation = current.generation,
                    loadedSources = current.loadedSources,
                    failure = validationError.message ?: "Prolog workspace validation failed",
                )
            }
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
        if (rawQuery == CANCEL_QUERY_COMMAND) {
            cancelQuery()
            return CompletableFuture.completedFuture(
                LocalQueryResult(CANCEL_QUERY_COMMAND, emptyList(), current.generation, cancelled = true),
            )
        }
        val query = try {
            PrologAuthorityPolicy.requireSafeQuery(PrologQueryPolicy.requireSafe(rawQuery))
        } catch (error: Throwable) {
            return CompletableFuture.failedFuture(error)
        }
        val ticket = queryEpoch.get()
        return submit {
            check(current.phase == LocalServerPhase.READY) { "Local Zara server is not ready" }
            val terms = bridge.evaluate(bounded(query))
            if (ticket != queryEpoch.get()) {
                LocalQueryResult(query, emptyList(), current.generation, cancelled = true)
            } else {
                LocalQueryResult(query, terms, current.generation)
            }
        }
    }

    /**
     * Invalidates the active/queued console query without waiting on the actor thread. Trealla's
     * call_with_time_limit/2 envelope guarantees that native execution also exits within the
     * bounded deadline even though the pinned C embedding exposes no host interrupt entry point.
     */
    fun cancelQuery() {
        queryEpoch.incrementAndGet()
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
            LocalQueryResult(query, bridge.evaluate(bounded(query)), current.generation)
        }
    }

    private fun bounded(query: String): String =
        "call_with_time_limit($QUERY_TIME_LIMIT_SECONDS, ($query))"

    private fun boot(phase: LocalServerPhase): LocalServerState {
        updateState(current.copy(phase = phase, failure = null))
        return try {
            val workspaceSources = workspace.listSources()
            PrologAuthorityPolicy.requireSafeWorkspace(workspaceSources)
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
        queryEpoch.incrementAndGet()
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
        const val CANCEL_QUERY_COMMAND = "__zara_cancel_prolog_query__"
        internal const val QUERY_TIME_LIMIT_SECONDS = 5
    }
}
