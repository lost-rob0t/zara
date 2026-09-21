package ai.zara.app.prolog

import ai.zara.app.AndroidAppSession
import ai.zara.app.history.HistoryMessageRole
import ai.zara.app.history.HistoryMessageStatus
import ai.zara.app.history.PortableConversationStore
import ai.zara.app.history.SymbolicConversationProjection
import ai.zara.app.history.loadSymbolicProjection
import ai.zara.app.history.saveSymbolicProjection
import ai.zara.app.runtime.LocalQueryResult
import java.util.concurrent.CompletableFuture

/**
 * Binds the zero-model conversation controller to Android's existing Prolog runtime owner.
 *
 * This does not own a runtime, conversation store, expert registry, or provider client. Explicit
 * queries stay on the bounded local-query path. Natural-language turns execute the canonical
 * symbolic_dialogue_turn -> symbolic_dialogue renderer chain through AndroidAppSession's already
 * started LocalZaraServer via queryLocalProlog(). A durable [PortableConversationStore] is
 * mandatory so there is no stateless factory path that can silently reset Context0 between turns.
 */
internal object AndroidPureSymbolicConversationFactory {
    /**
     * Compose natural turns with the canonical durable conversation projection.
     *
     * The supplied [projectionStore] remains the sole persistence authority. This factory adds no
     * Android-local context cache: every natural turn loads Context0 from the existing
     * SymbolicConversationProjection, installs a pending CAS fence bound to the already-persisted
     * canonical message turn id, executes the canonical dialogue turn exactly once, receives both
     * the rendered response and Context1 through the existing Trealla Result-binding ABI, and
     * persists Context1 through the same pending generation.
     *
     * Persisting the pending projection before local evaluation is what lets
     * PortableConversationStore.loadState() interrupt an in-flight turn during Activity/process
     * recreation and advance the generation. A late callback from the old turn then loses its CAS
     * and cannot publish stale Context1. Cancellation is serialized with the same terminal commit.
     */
    fun create(
        session: AndroidAppSession,
        projectionStore: PortableConversationStore,
    ): PureSymbolicConversationController =
        controller(
            session = session,
            resolve = { utterance, conversationId ->
                resolvePersistedTurn(
                    session = session,
                    projectionStore = projectionStore,
                    utterance = utterance,
                    conversationId = conversationId,
                )
            },
        )

    private fun controller(
        session: AndroidAppSession,
        resolve: (String, String) -> CompletableFuture<LocalQueryResult>,
    ): PureSymbolicConversationController =
        PureSymbolicConversationController(
            catalog = { PrologWorkspaceCatalog.from(session.prologSources()) },
            query = session::queryLocalProlog,
            resolve = resolve,
        )

    private fun resolvePersistedTurn(
        session: AndroidAppSession,
        projectionStore: PortableConversationStore,
        utterance: String,
        conversationId: String,
    ): CompletableFuture<LocalQueryResult> {
        val current = projectionStore.loadSymbolicProjection(conversationId)
        current?.assertPureSymbolic()
        val expectedGeneration = current?.projectionGeneration ?: 0L
        val context0 = SymbolicDialogueContextCodec.decode(current?.dialogueStateJson ?: "{}")
        val turnId = requireRunningTurnId(projectionStore, conversationId)
        val pendingProjection = pendingProjection(
            current = current,
            conversationId = conversationId,
            expectedGeneration = expectedGeneration,
            context0 = context0,
            turnId = turnId,
        )
        projectionStore.saveSymbolicProjection(
            projection = pendingProjection,
            expectedGeneration = expectedGeneration,
        )
        val pendingGeneration = pendingProjection.projectionGeneration

        val turnFuture = try {
            session.queryLocalProlog(dialogueTurnEnvelopeQuery(utterance, context0))
        } catch (error: Throwable) {
            return failBeforeAsyncEvaluation(
                projectionStore = projectionStore,
                pendingProjection = pendingProjection,
                pendingGeneration = pendingGeneration,
                context0 = context0,
                error = error,
            )
        }
        val output = PersistenceFencedFuture(turnFuture) {
            val cancelledProjection = terminalProjection(
                pending = pendingProjection,
                contextTerm = context0,
                outcome = "cancelled",
            )
            projectionStore.saveSymbolicProjection(
                projection = cancelledProjection,
                expectedGeneration = pendingGeneration,
            )
        }

        turnFuture.whenComplete { result, resultError ->
            if (output.isDone) return@whenComplete
            if (resultError != null || result == null) {
                val failure = resultError
                    ?: IllegalStateException("Symbolic dialogue result is missing")
                completeFailure(
                    output = output,
                    projectionStore = projectionStore,
                    pendingProjection = pendingProjection,
                    pendingGeneration = pendingGeneration,
                    context0 = context0,
                    error = failure,
                )
                return@whenComplete
            }
            if (result.terms.isEmpty()) {
                completeNoMatch(
                    output = output,
                    projectionStore = projectionStore,
                    pendingProjection = pendingProjection,
                    pendingGeneration = pendingGeneration,
                    context0 = context0,
                    result = result,
                )
                return@whenComplete
            }

            try {
                val (renderedResponse, context1) = splitDialogueEnvelope(result)
                val completedProjection = terminalProjection(
                    pending = pendingProjection,
                    contextTerm = context1,
                    outcome = "success",
                )
                output.commitOrCancel {
                    projectionStore.saveSymbolicProjection(
                        projection = completedProjection,
                        expectedGeneration = pendingGeneration,
                    )
                    output.complete(result.copy(terms = listOf(renderedResponse)))
                }
            } catch (error: Throwable) {
                completeFailure(
                    output = output,
                    projectionStore = projectionStore,
                    pendingProjection = pendingProjection,
                    pendingGeneration = pendingGeneration,
                    context0 = context0,
                    error = error,
                )
            }
        }
        return output
    }

    private fun requireRunningTurnId(
        projectionStore: PortableConversationStore,
        conversationId: String,
    ): String {
        val assistant = projectionStore.loadMessages(conversationId).lastOrNull { message ->
            message.role == HistoryMessageRole.Assistant &&
                (message.status == HistoryMessageStatus.Pending ||
                    message.status == HistoryMessageStatus.Streaming)
        } ?: error("Pure-symbolic conversation has no running canonical turn")
        return requireNotNull(assistant.turnId) {
            "Pure-symbolic running turn is missing canonical turn identity"
        }
    }

    private fun failBeforeAsyncEvaluation(
        projectionStore: PortableConversationStore,
        pendingProjection: SymbolicConversationProjection,
        pendingGeneration: Long,
        context0: String,
        error: Throwable,
    ): CompletableFuture<LocalQueryResult> {
        val terminal = terminalProjection(
            pending = pendingProjection,
            contextTerm = context0,
            outcome = "error",
        )
        return try {
            projectionStore.saveSymbolicProjection(
                projection = terminal,
                expectedGeneration = pendingGeneration,
            )
            CompletableFuture.failedFuture(error)
        } catch (fenceError: Throwable) {
            CompletableFuture.failedFuture(fenceError)
        }
    }

    private fun completeFailure(
        output: PersistenceFencedFuture,
        projectionStore: PortableConversationStore,
        pendingProjection: SymbolicConversationProjection,
        pendingGeneration: Long,
        context0: String,
        error: Throwable,
    ) {
        try {
            output.commitOrCancel {
                projectionStore.saveSymbolicProjection(
                    projection = terminalProjection(
                        pending = pendingProjection,
                        contextTerm = context0,
                        outcome = "error",
                    ),
                    expectedGeneration = pendingGeneration,
                )
                output.completeExceptionally(error)
            }
        } catch (fenceError: Throwable) {
            output.completeExceptionally(fenceError)
        }
    }

    private fun completeNoMatch(
        output: PersistenceFencedFuture,
        projectionStore: PortableConversationStore,
        pendingProjection: SymbolicConversationProjection,
        pendingGeneration: Long,
        context0: String,
        result: LocalQueryResult,
    ) {
        try {
            output.commitOrCancel {
                projectionStore.saveSymbolicProjection(
                    projection = terminalProjection(
                        pending = pendingProjection,
                        contextTerm = context0,
                        outcome = "error",
                    ),
                    expectedGeneration = pendingGeneration,
                )
                output.complete(result)
            }
        } catch (fenceError: Throwable) {
            output.completeExceptionally(fenceError)
        }
    }

    private fun pendingProjection(
        current: SymbolicConversationProjection?,
        conversationId: String,
        expectedGeneration: Long,
        context0: String,
        turnId: String,
    ): SymbolicConversationProjection {
        val runtimeGeneration = current?.runtimeGeneration?.let(Math::incrementExact) ?: 1L
        val base = current ?: SymbolicConversationProjection(
            conversationId = conversationId,
            projectionGeneration = expectedGeneration + 1L,
            runtimeGeneration = runtimeGeneration,
            dialogueStateJson = SymbolicDialogueContextCodec.encode(context0),
        )
        return base.copy(
            projectionGeneration = expectedGeneration + 1L,
            runtimeGeneration = runtimeGeneration,
            turnId = turnId,
            outcome = "pending",
            dialogueAct = "conversation",
            dialogueStateJson = SymbolicDialogueContextCodec.encode(context0),
            rendererProvenance = "",
            providersEnabled = false,
            maxModelCalls = 0L,
            providerCalls = 0L,
            modelCalls = 0L,
        ).also(SymbolicConversationProjection::assertPureSymbolic)
    }

    private fun terminalProjection(
        pending: SymbolicConversationProjection,
        contextTerm: String,
        outcome: String,
    ): SymbolicConversationProjection = pending.copy(
        projectionGeneration = Math.incrementExact(pending.projectionGeneration),
        outcome = outcome,
        dialogueStateJson = SymbolicDialogueContextCodec.encode(contextTerm),
        rendererProvenance = if (outcome == "success") "symbolic-dcg/v1" else "",
        providersEnabled = false,
        maxModelCalls = 0L,
        providerCalls = 0L,
        modelCalls = 0L,
    ).also(SymbolicConversationProjection::assertPureSymbolic)

    private fun splitDialogueEnvelope(result: LocalQueryResult): Pair<String, String> {
        check(result.terms.size == 2) {
            "Symbolic dialogue must return one rendered response and one canonical Context1 term"
        }
        val renderedResponse = result.terms[0]
        val wrappedContext = result.terms[1].trim()
        check(
            wrappedContext.startsWith(DIALOGUE_CONTEXT_PREFIX) && wrappedContext.endsWith(')')
        ) {
            "Symbolic dialogue Context1 result has an invalid envelope"
        }
        val contextTerm = wrappedContext
            .removePrefix(DIALOGUE_CONTEXT_PREFIX)
            .dropLast(1)
        return renderedResponse to SymbolicDialogueContextCodec.requireContextTerm(contextTerm)
    }

    /**
     * Serialize cancellation against the canonical projection terminal commit.
     *
     * Cancellation and completion hold the same monitor. If cancellation wins, it records a
     * terminal cancelled projection (when this process still owns the pending generation), marks
     * the returned future cancelled, and cancels the upstream local query. If restart recovery has
     * already advanced the generation to interrupted, the stale cancellation write is ignored and
     * the old turn still cannot commit Context1.
     */
    private class PersistenceFencedFuture(
        private val upstream: CompletableFuture<*>,
        private val onCancel: () -> Unit,
    ) : CompletableFuture<LocalQueryResult>() {
        private val fence = Any()

        fun commitOrCancel(commit: () -> Unit) = synchronized(fence) {
            if (!isDone) commit()
        }

        override fun cancel(mayInterruptIfRunning: Boolean): Boolean = synchronized(fence) {
            if (isDone) return@synchronized false
            val cancelled = super.cancel(mayInterruptIfRunning)
            if (cancelled) {
                runCatching(onCancel)
                upstream.cancel(mayInterruptIfRunning)
            }
            cancelled
        }
    }

    /**
     * Render one canonical dialogue turn from an explicitly supplied continuation term.
     *
     * The term is data, not executable query text: it is quoted as a Prolog string, decoded by
     * term_string/3, and must pass the shared valid_dialogue_context/1 shape fence before the
     * canonical router sees it. Context1 is also validated before a response can escape.
     */
    internal fun dialogueTurnQuery(
        utterance: String,
        contextTerm: String = SymbolicDialogueContextCodec.emptyContextTerm,
    ): String = dialogueTurnPrelude(utterance, contextTerm) +
        ", symbolic_dialogue:render_response(Act, Result)"

    /**
     * Execute one canonical dialogue turn and return both outputs as ordered Result solutions.
     *
     * The first solution is exactly the renderer result used by the existing UI contract. The
     * second wraps Context1 so Kotlin can distinguish continuation state without replaying the
     * dialogue turn. `once/1` commits the router+renderer to one deterministic solution before the
     * two Result alternatives are enumerated, preventing a backtracking runtime from re-entering
     * expert/effectful dialogue work while the JNI bridge drains Result solutions.
     */
    internal fun dialogueTurnEnvelopeQuery(
        utterance: String,
        contextTerm: String,
    ): String = "once((" + dialogueTurnPrelude(utterance, contextTerm) +
        ", symbolic_dialogue:render_response(Act, Response))), " +
        "(Result = Response ; Result = dialogue_context(Context1))"

    private fun dialogueTurnPrelude(utterance: String, contextTerm: String): String {
        val text = utterance.trim()
        require(text.isNotEmpty()) { "Utterance is required" }
        require(text.length <= MAX_UTTERANCE_CHARS) { "Utterance is too large" }
        val escapedText = prologString(text)
        val canonicalContext = SymbolicDialogueContextCodec.requireContextTerm(contextTerm)
        val escapedContext = SymbolicDialogueContextCodec.prologString(canonicalContext)
        return "term_string(Context0, \"$escapedContext\", [quoted(true)]), " +
            "symbolic_dialogue_turn:valid_dialogue_context(Context0), " +
            "symbolic_dialogue_turn:dialogue_turn(\"$escapedText\", conversation, Context0, " +
            "turn(_Frames, Act, Context1)), " +
            "symbolic_dialogue_turn:valid_dialogue_context(Context1)"
    }

    private fun prologString(raw: String): String = buildString(raw.length + 8) {
        raw.forEach { character ->
            when (character) {
                '\\' -> append("\\\\")
                '"' -> append("\\\"")
                '\n' -> append("\\n")
                '\r' -> append("\\r")
                '\t' -> append("\\t")
                else -> append(character)
            }
        }
    }

    private fun Math.incrementExact(value: Long): Long = addExact(value, 1L)

    private const val DIALOGUE_CONTEXT_PREFIX = "dialogue_context("
    private const val MAX_UTTERANCE_CHARS = 8_192
}
