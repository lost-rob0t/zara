package ai.zara.app.prolog

import ai.zara.app.runtime.LocalQueryResult
import ai.zara.app.runtime.TextTurnResult
import java.util.UUID
import java.util.concurrent.CancellationException
import java.util.concurrent.CompletableFuture
import java.util.concurrent.CompletionException

enum class PureSymbolicRoute {
    EXPLICIT_QUERY,
    EXPLICIT_COMMAND,
    FRAME_RESOLVER,
}

data class PureSymbolicTurnResult(
    val turn: TextTurnResult,
    val route: PureSymbolicRoute,
    val renderer: String = when (route) {
        PureSymbolicRoute.FRAME_RESOLVER -> "symbolic-dcg/v1"
        PureSymbolicRoute.EXPLICIT_QUERY,
        PureSymbolicRoute.EXPLICIT_COMMAND -> "symbolic-term/v1"
    },
    val maxModelCalls: Int = 0,
    val maxProviderCalls: Int = 0,
    val modelCalls: Int = 0,
    val providerCalls: Int = 0,
) {
    init {
        require(maxModelCalls == 0) { "Pure symbolic turns require max_model_calls=0" }
        require(maxProviderCalls == 0) { "Pure symbolic turns require max_provider_calls=0" }
        require(modelCalls == 0) { "Pure symbolic turns require model_calls=0" }
        require(providerCalls == 0) { "Pure symbolic turns require provider_calls=0" }
    }
}

/**
 * One natural-language resolution bound to the canonical turn already owned by the durable
 * conversation store.
 *
 * The controller must not mint a second turn id for a natural turn after the factory has fenced
 * and persisted the real turn. Explicit Prolog/query routes do not use this wrapper because they
 * do not cross the natural conversation persistence path.
 */
data class PureSymbolicResolution(
    val turnId: String,
    val future: CompletableFuture<LocalQueryResult>,
) {
    init {
        require(turnId.isNotBlank()) { "Canonical pure-symbolic turn id is required" }
        require(turnId.none(Char::isISOControl)) {
            "Canonical pure-symbolic turn id contains control characters"
        }
    }
}

/**
 * Android pure-symbolic conversation boundary.
 *
 * This controller intentionally has no model client, provider client, socket fallback, or
 * alternate natural-language router dependency. Natural text goes through the canonical
 * Prolog dialogue-turn + deterministic renderer path supplied by the factory. Explicit
 * Prolog/expert commands remain inside the existing bounded Prolog command contract and retain
 * symbolic-term renderer provenance. A miss or runtime error is rendered deterministically and
 * ends the turn; it never escalates to a model/provider path.
 *
 * The controller does not own conversation history or expert registration. Natural-turn resolver
 * calls receive the normalized canonical conversation id so the factory can compose the existing
 * conversation projection without inventing controller-local dialogue state. Callers persist the
 * returned evidence against Zara's canonical conversation store/projection and keep actual
 * effects behind the existing registered-predicate capability/approval boundary.
 */
class PureSymbolicConversationController(
    private val catalog: () -> PrologWorkspaceCatalog,
    private val query: (String) -> CompletableFuture<LocalQueryResult>,
    private val resolve: (String, String) -> PureSymbolicResolution,
    private val turnIds: Iterator<String> = generateSequence {
        UUID.randomUUID().toString()
    }.iterator(),
) {
    private val turnIdLock = Any()

    fun submit(
        text: String,
        conversationId: String,
    ): CompletableFuture<PureSymbolicTurnResult> {
        val input = text.trim()
        require(input.isNotEmpty()) { "Text is required" }
        require(input.length <= MAX_INPUT_CHARS) { "Text exceeds pure-symbolic input limit" }
        val normalizedConversationId = conversationId.trim()
        require(normalizedConversationId.isNotEmpty()) { "Conversation id is required" }
        require(normalizedConversationId.length <= MAX_CONVERSATION_ID_CHARS) {
            "Conversation id is too long"
        }
        require(normalizedConversationId.none(Char::isISOControl)) {
            "Conversation id contains control characters"
        }

        val routed = try {
            route(input, normalizedConversationId)
        } catch (error: CancellationException) {
            return cancelledTurnFuture()
        } catch (error: Exception) {
            return CompletableFuture.completedFuture(
                failure(
                    conversationId = normalizedConversationId,
                    route = routeKind(input),
                    runtimeFailure = true,
                ),
            )
        }

        val output = LinkedTurnFuture<PureSymbolicTurnResult>(routed.future)
        routed.future.whenComplete { result, error ->
            if (output.isDone) {
                return@whenComplete
            }
            if (routed.future.isCancelled || isCancellation(error)) {
                output.cancel(false)
                return@whenComplete
            }
            try {
                val completed = when {
                    error != null || result == null -> failure(
                        conversationId = normalizedConversationId,
                        route = routed.route,
                        runtimeFailure = true,
                        turnId = routed.turnId,
                    )
                    result.terms.isEmpty() -> failure(
                        conversationId = normalizedConversationId,
                        route = routed.route,
                        runtimeFailure = false,
                        turnId = routed.turnId,
                    )
                    else -> PureSymbolicTurnResult(
                        turn = TextTurnResult(
                            conversationId = normalizedConversationId,
                            turnId = routed.turnId ?: nextTurnId(),
                            text = result.terms.joinToString("\n"),
                            success = true,
                        ),
                        route = routed.route,
                    )
                }
                output.complete(completed)
            } catch (completionError: Throwable) {
                output.completeExceptionally(completionError)
            }
        }
        return output
    }

    private fun route(input: String, conversationId: String): RoutedQuery = when (routeKind(input)) {
        PureSymbolicRoute.EXPLICIT_QUERY -> RoutedQuery(
            PureSymbolicRoute.EXPLICIT_QUERY,
            query(input),
        )
        PureSymbolicRoute.EXPLICIT_COMMAND -> {
            val command = LocalPrologCommand.parse(input, catalog())
            RoutedQuery(PureSymbolicRoute.EXPLICIT_COMMAND, query(command.query))
        }
        PureSymbolicRoute.FRAME_RESOLVER -> {
            val resolution = resolve(normalizeNaturalInput(input), conversationId)
            RoutedQuery(
                route = PureSymbolicRoute.FRAME_RESOLVER,
                future = resolution.future,
                turnId = resolution.turnId,
            )
        }
    }

    private fun normalizeNaturalInput(input: String): String {
        val punctuationCount = input.takeLastWhile { character ->
            character == '.' || character == '!' || character == '?'
        }.length
        if (punctuationCount !in 1..MAX_TERMINAL_PUNCTUATION) return input

        val normalized = input.dropLast(punctuationCount).trimEnd()
        return normalized.ifEmpty { input }
    }

    private fun routeKind(input: String): PureSymbolicRoute = when {
        input.startsWith("?-") -> PureSymbolicRoute.EXPLICIT_QUERY
        input.startsWith("/prolog ") || input.startsWith("/expert ") ->
            PureSymbolicRoute.EXPLICIT_COMMAND
        else -> PureSymbolicRoute.FRAME_RESOLVER
    }

    private fun failure(
        conversationId: String,
        route: PureSymbolicRoute,
        runtimeFailure: Boolean,
        turnId: String? = null,
    ): PureSymbolicTurnResult = PureSymbolicTurnResult(
        turn = TextTurnResult(
            conversationId = conversationId,
            turnId = turnId ?: nextTurnId(),
            text = if (runtimeFailure) {
                "The symbolic runtime could not complete this turn."
            } else {
                "I don't have a deterministic symbolic answer for that yet."
            },
            success = false,
        ),
        route = route,
    )

    private fun nextTurnId(): String = synchronized(turnIdLock) {
        check(turnIds.hasNext()) { "Pure symbolic turn id source is exhausted" }
        turnIds.next()
    }

    private fun cancelledTurnFuture(): CompletableFuture<PureSymbolicTurnResult> =
        CompletableFuture<PureSymbolicTurnResult>().also { it.cancel(false) }

    private fun isCancellation(error: Throwable?): Boolean {
        var current = error
        while (current is CompletionException && current.cause != null) {
            current = current.cause
        }
        return current is CancellationException
    }

    private data class RoutedQuery(
        val route: PureSymbolicRoute,
        val future: CompletableFuture<LocalQueryResult>,
        val turnId: String? = null,
    )

    private class LinkedTurnFuture<T>(
        private val upstream: CompletableFuture<*>,
    ) : CompletableFuture<T>() {
        override fun cancel(mayInterruptIfRunning: Boolean): Boolean {
            val cancelled = super.cancel(mayInterruptIfRunning)
            if (cancelled) {
                upstream.cancel(mayInterruptIfRunning)
            }
            return cancelled
        }
    }

    companion object {
        private const val MAX_INPUT_CHARS = 32 * 1024
        private const val MAX_CONVERSATION_ID_CHARS = 256
        private const val MAX_TERMINAL_PUNCTUATION = 3
    }
}
