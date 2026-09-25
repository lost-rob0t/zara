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
        val projected = CancellationPropagatingFuture<PureSymbolicExpertConversationResult>(admitted)
        admitted.whenComplete { result, error ->
            if (projected.isDone) return@whenComplete
            if (error != null || result == null) {
                projected.completeExceptionally(
                    error ?: IllegalStateException("Canonical expert result is missing"),
                )
                return@whenComplete
            }
            try {
                projected.complete(PureSymbolicExpertConversationProjection.from(result))
            } catch (projectionError: Throwable) {
                projected.completeExceptionally(projectionError)
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

    /**
     * CompletableFuture does not normally propagate cancellation from a dependent stage to its
     * source. Android wraps an admitted expert result into the ordinary conversation envelope, so
     * that default would allow canonical expert work/effects to continue after the UI turn is
     * durably cancelled. Every dependent stage created from this future therefore inherits the
     * same backward cancellation chain until it reaches the canonical owner future.
     */
    private class CancellationPropagatingFuture<T>(
        private val upstream: CompletableFuture<*>,
    ) : CompletableFuture<T>() {
        override fun cancel(mayInterruptIfRunning: Boolean): Boolean {
            val cancelled = super.cancel(mayInterruptIfRunning)
            if (cancelled && !upstream.isDone) {
                upstream.cancel(mayInterruptIfRunning)
            }
            return cancelled
        }

        override fun <U> newIncompleteFuture(): CompletableFuture<U> =
            CancellationPropagatingFuture(this)
    }

    private companion object {
        val portableRequestIdentity = Regex("^[!-~]{1,128}$")
    }
}
