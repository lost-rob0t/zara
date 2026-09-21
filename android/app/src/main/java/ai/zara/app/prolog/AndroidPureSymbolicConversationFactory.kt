package ai.zara.app.prolog

import ai.zara.app.AndroidAppSession
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
     * SymbolicConversationProjection, executes the canonical dialogue turn exactly once, receives
     * both the rendered response and Context1 through the existing Trealla Result-binding ABI,
     * and persists Context1 with the projection generation CAS.
     *
     * Cancellation is fenced with the persistence commit. If cancellation wins the fence, no
     * Context1 write occurs. If persistence wins, the resolver completes successfully before a
     * later cancellation can claim the turn was cancelled.
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
        val turnFuture = session.queryLocalProlog(dialogueTurnEnvelopeQuery(utterance, context0))
        val output = PersistenceFencedFuture(turnFuture)

        turnFuture.whenComplete { result, resultError ->
            if (output.isDone) return@whenComplete
            if (resultError != null || result == null) {
                output.completeExceptionally(
                    resultError ?: IllegalStateException("Symbolic dialogue result is missing"),
                )
                return@whenComplete
            }
            if (result.terms.isEmpty()) {
                output.complete(result)
                return@whenComplete
            }

            try {
                val (renderedResponse, context1) = splitDialogueEnvelope(result)
                val next = nextProjection(
                    current = current,
                    conversationId = conversationId,
                    expectedGeneration = expectedGeneration,
                    runtimeGeneration = result.generation,
                    context1 = context1,
                )
                output.commitOrCancel {
                    projectionStore.saveSymbolicProjection(
                        projection = next,
                        expectedGeneration = expectedGeneration,
                    )
                    output.complete(result.copy(terms = listOf(renderedResponse)))
                }
            } catch (error: Throwable) {
                output.completeExceptionally(error)
            }
        }
        return output
    }

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

    private fun nextProjection(
        current: SymbolicConversationProjection?,
        conversationId: String,
        expectedGeneration: Long,
        runtimeGeneration: Long,
        context1: String,
    ): SymbolicConversationProjection {
        val base = current ?: SymbolicConversationProjection(
            conversationId = conversationId,
            projectionGeneration = 1L,
            runtimeGeneration = runtimeGeneration,
            outcome = "unknown",
            dialogueAct = "conversation",
            dialogueStateJson = SymbolicDialogueContextCodec.encode(context1),
            rendererProvenance = "symbolic-dcg/v1",
            providersEnabled = false,
            maxModelCalls = 0L,
            providerCalls = 0L,
            modelCalls = 0L,
        )
        return base.copy(
            projectionGeneration = expectedGeneration + 1L,
            runtimeGeneration = maxOf(base.runtimeGeneration, runtimeGeneration),
            outcome = "unknown",
            dialogueAct = "conversation",
            dialogueStateJson = SymbolicDialogueContextCodec.encode(context1),
            rendererProvenance = "symbolic-dcg/v1",
            providersEnabled = false,
            maxModelCalls = 0L,
            providerCalls = 0L,
            modelCalls = 0L,
        ).also(SymbolicConversationProjection::assertPureSymbolic)
    }

    /**
     * Serialize cancellation against the canonical projection commit.
     *
     * A normal CompletableFuture chain can be cancelled between the final cancellation check and
     * saveSymbolicProjection(), which would allow a late callback to mutate durable dialogue state
     * after the UI already observed cancellation. This small fence makes those outcomes mutually
     * exclusive without introducing another scheduler or persistence owner.
     */
    private class PersistenceFencedFuture(
        private val upstream: CompletableFuture<*>,
    ) : CompletableFuture<LocalQueryResult>() {
        private val fence = Any()

        fun commitOrCancel(commit: () -> Unit) = synchronized(fence) {
            if (!isDone) commit()
        }

        override fun cancel(mayInterruptIfRunning: Boolean): Boolean = synchronized(fence) {
            val cancelled = super.cancel(mayInterruptIfRunning)
            if (cancelled) {
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

    private const val DIALOGUE_CONTEXT_PREFIX = "dialogue_context("
    private const val MAX_UTTERANCE_CHARS = 8_192
}
