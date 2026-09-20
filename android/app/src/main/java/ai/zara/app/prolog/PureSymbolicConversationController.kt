package ai.zara.app.prolog

import ai.zara.app.runtime.LocalQueryResult
import ai.zara.app.runtime.TextTurnResult
import java.util.UUID
import java.util.concurrent.CompletableFuture

enum class PureSymbolicRoute {
    EXPLICIT_QUERY,
    EXPLICIT_COMMAND,
    FRAME_RESOLVER,
}

data class PureSymbolicTurnResult(
    val turn: TextTurnResult,
    val route: PureSymbolicRoute,
    val renderer: String = "symbolic-term/v1",
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
 * Android pure-symbolic conversation boundary.
 *
 * This controller intentionally has no model client, provider client, socket fallback, or
 * alternate natural-language router dependency. Natural text goes through the canonical
 * Prolog frame resolver. Explicit Prolog/expert commands remain inside the existing bounded
 * Prolog command contract. A miss or runtime error is rendered deterministically and ends the
 * turn; it never escalates to a model/provider path.
 *
 * The controller does not own conversation history or expert registration. Callers persist the
 * returned evidence against Zara's canonical conversation store/projection and keep actual
 * effects behind the existing registered-predicate capability/approval boundary.
 */
class PureSymbolicConversationController(
    private val catalog: () -> PrologWorkspaceCatalog,
    private val query: (String) -> CompletableFuture<LocalQueryResult>,
    private val resolve: (String) -> CompletableFuture<LocalQueryResult>,
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
            route(input)
        } catch (error: Throwable) {
            return CompletableFuture.completedFuture(
                failure(
                    conversationId = normalizedConversationId,
                    route = routeKind(input),
                    runtimeFailure = true,
                ),
            )
        }

        return routed.future.handle { result, error ->
            when {
                error != null || result == null -> failure(
                    conversationId = normalizedConversationId,
                    route = routed.route,
                    runtimeFailure = true,
                )
                result.terms.isEmpty() -> failure(
                    conversationId = normalizedConversationId,
                    route = routed.route,
                    runtimeFailure = false,
                )
                else -> PureSymbolicTurnResult(
                    turn = TextTurnResult(
                        conversationId = normalizedConversationId,
                        turnId = nextTurnId(),
                        text = result.terms.joinToString("\n"),
                        success = true,
                    ),
                    route = routed.route,
                )
            }
        }
    }

    private fun route(input: String): RoutedQuery = when (routeKind(input)) {
        PureSymbolicRoute.EXPLICIT_QUERY -> RoutedQuery(
            PureSymbolicRoute.EXPLICIT_QUERY,
            query(input),
        )
        PureSymbolicRoute.EXPLICIT_COMMAND -> {
            val command = LocalPrologCommand.parse(input, catalog())
            RoutedQuery(PureSymbolicRoute.EXPLICIT_COMMAND, query(command.query))
        }
        PureSymbolicRoute.FRAME_RESOLVER -> RoutedQuery(
            PureSymbolicRoute.FRAME_RESOLVER,
            resolve(input),
        )
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
    ): PureSymbolicTurnResult = PureSymbolicTurnResult(
        turn = TextTurnResult(
            conversationId = conversationId,
            turnId = nextTurnId(),
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

    private data class RoutedQuery(
        val route: PureSymbolicRoute,
        val future: CompletableFuture<LocalQueryResult>,
    )

    companion object {
        private const val MAX_INPUT_CHARS = 32 * 1024
        private const val MAX_CONVERSATION_ID_CHARS = 256
    }
}
