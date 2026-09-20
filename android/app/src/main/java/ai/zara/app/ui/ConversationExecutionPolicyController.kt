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
 */
class ConversationExecutionPolicyController(
    private val store: ConversationExecutionPolicyStore,
    private val pureSymbolicSubmit: (String, String) -> CompletableFuture<PureSymbolicTurnResult>,
    private val standardSubmit: (String, String) -> CompletableFuture<TextTurnResult>,
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
        when (current) {
            ConversationExecutionPolicy.STANDARD -> standardSubmit(text, conversationId)
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
