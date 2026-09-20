package ai.zara.app.ui

import ai.zara.app.prolog.PureSymbolicTurnResult
import ai.zara.app.runtime.TextTurnResult
import java.util.concurrent.CompletableFuture

/**
 * Android chat execution-policy boundary.
 *
 * This is intentionally orthogonal to Auto/Local/Remote routing. STANDARD delegates to the
 * existing Android conversation submit path. PURE_SYMBOLIC delegates only to the canonical
 * zero-model symbolic controller and validates its zero-call evidence before exposing a turn.
 * No provider/model fallback is reachable from the pure-symbolic branch.
 *
 * The per-submit STANDARD supplier lets the real chat owner preserve its existing project/local/
 * remote routing without teaching this policy boundary about projects, transports, or provider
 * runtimes. The supplier is lazy and is never evaluated in PURE_SYMBOLIC mode.
 */
class ConversationExecutionPolicyController(
    private val store: ConversationExecutionPolicyStore,
    private val pureSymbolicSubmit: (String, String) -> CompletableFuture<PureSymbolicTurnResult>,
    private val standardSubmit: ((String, String) -> CompletableFuture<TextTurnResult>)? = null,
) {
    @Volatile
    private var current: ConversationExecutionPolicy = store.load()

    fun policy(): ConversationExecutionPolicy = current

    @Synchronized
    fun select(policy: ConversationExecutionPolicy) {
        store.save(policy)
        current = policy
    }

    fun submit(text: String, conversationId: String): CompletableFuture<TextTurnResult> =
        submit(text, conversationId) {
            val fallback = standardSubmit ?: return@submit CompletableFuture.failedFuture(
                IllegalStateException("STANDARD submit path was not supplied"),
            )
            fallback(text, conversationId)
        }

    fun submit(
        text: String,
        conversationId: String,
        standardTurn: () -> CompletableFuture<TextTurnResult>,
    ): CompletableFuture<TextTurnResult> = when (current) {
        ConversationExecutionPolicy.STANDARD -> standardTurn()
        ConversationExecutionPolicy.PURE_SYMBOLIC ->
            pureSymbolicSubmit(text, conversationId).thenApply(::requireZeroModelEvidence)
    }

    private fun requireZeroModelEvidence(result: PureSymbolicTurnResult): TextTurnResult {
        check(result.maxModelCalls == 0) { "Pure symbolic max_model_calls changed" }
        check(result.maxProviderCalls == 0) { "Pure symbolic max_provider_calls changed" }
        check(result.modelCalls == 0) { "Pure symbolic model_calls must remain zero" }
        check(result.providerCalls == 0) { "Pure symbolic provider_calls must remain zero" }
        return result.turn
    }
}
