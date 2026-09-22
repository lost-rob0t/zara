package ai.zara.app.prolog

import ai.zara.app.expert.ExpertLimits
import ai.zara.app.expert.PureSymbolicExpertConversationProjection
import ai.zara.app.expert.PureSymbolicExpertConversationResult
import ai.zara.app.expert.PureSymbolicExpertInvocationAdapter
import java.util.concurrent.CompletableFuture

/**
 * Conversation-facing composition for one already-selected natural expert turn.
 *
 * Selection remains deterministic in [LocalNaturalLanguageExpertRouter]. Authority, lifecycle,
 * budgets, effects, and evidence remain owned by the existing canonical ZARA-EXPERT/1 owner
 * consumed through [PureSymbolicExpertInvocationAdapter]. This class owns none of those planes.
 */
internal class CanonicalNaturalExpertTurn(
    private val adapter: PureSymbolicExpertInvocationAdapter,
) {
    fun invoke(
        selection: NaturalLanguageExpertSelection,
        principal: String,
        workspace: String,
        requestId: String,
        limits: ExpertLimits,
        idempotencyKey: String,
    ): CompletableFuture<PureSymbolicExpertConversationResult> {
        val admitted = adapter.invoke(
            principal = principal,
            workspace = workspace,
            expertId = selection.expertId,
            requestId = requestId,
            expertOperation = selection.expertOperation,
            input = selection.input,
            limits = limits,
            idempotencyKey = idempotencyKey,
        )
        val projected = admitted.thenApply(PureSymbolicExpertConversationProjection::from)
        projected.whenComplete { _, _ ->
            if (projected.isCancelled && !admitted.isDone) {
                admitted.cancel(true)
            }
        }
        return projected
    }
}
