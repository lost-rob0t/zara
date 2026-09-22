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
        try {
            requireCanonicalTurnIdentity(
                requestId = requestId,
                expertOperation = selection.expertOperation,
                idempotencyKey = idempotencyKey,
            )
        } catch (error: Throwable) {
            return CompletableFuture.failedFuture(error)
        }

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

    private fun requireCanonicalTurnIdentity(
        requestId: String,
        expertOperation: String,
        idempotencyKey: String,
    ) {
        require(portableRequestIdentity.matches(requestId)) {
            "Canonical expert conversation requestId must be the bounded durable turn identity"
        }
        val expectedIdempotencyKey = "$requestId:$expertOperation"
        require(portableRequestIdentity.matches(expectedIdempotencyKey)) {
            "Canonical expert conversation idempotency identity is outside the portable bound"
        }
        require(idempotencyKey == expectedIdempotencyKey) {
            "Canonical expert conversation idempotency identity is not bound to the durable turn"
        }
    }

    private companion object {
        val portableRequestIdentity = Regex("^[!-~]{1,128}$")
    }
}
