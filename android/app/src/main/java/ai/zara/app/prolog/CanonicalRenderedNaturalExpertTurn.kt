package ai.zara.app.prolog

import ai.zara.app.expert.ExpertLimits
import java.util.concurrent.CompletableFuture

/**
 * Composes canonical expert admission/projection with Zara's existing symbolic response renderer.
 *
 * This class owns neither expert authority nor rendering semantics. Cancellation is explicitly
 * fenced across both asynchronous stages so a cancelled UI turn cannot leave an in-flight
 * canonical invocation or a late local renderer query running behind it.
 */
internal class CanonicalRenderedNaturalExpertTurn(
    private val expertTurn: CanonicalNaturalExpertTurn,
    private val renderer: CanonicalExpertSymbolicRendererAdapter,
) {
    fun invoke(
        selection: NaturalLanguageExpertSelection,
        principal: String,
        workspace: String,
        requestId: String,
        limits: ExpertLimits,
        idempotencyKey: String,
    ): CompletableFuture<CanonicalRenderedExpertAnswer> {
        val projected = try {
            expertTurn.invoke(
                selection = selection,
                principal = principal,
                workspace = workspace,
                requestId = requestId,
                limits = limits,
                idempotencyKey = idempotencyKey,
            )
        } catch (error: Throwable) {
            return CompletableFuture.failedFuture(error)
        }
        val output = ActiveStageCancellationFuture<CanonicalRenderedExpertAnswer>(projected)
        projected.whenComplete admissionComplete@{ admitted, admissionError ->
            if (output.isDone) return@admissionComplete
            if (admissionError != null || admitted == null) {
                output.completeExceptionally(
                    admissionError ?: IllegalStateException("Canonical expert projection is missing"),
                )
                return@admissionComplete
            }

            val rendering = try {
                renderer.render(admitted)
            } catch (renderError: Throwable) {
                output.completeExceptionally(renderError)
                return@admissionComplete
            }
            if (!output.advanceTo(rendering)) return@admissionComplete
            rendering.whenComplete renderComplete@{ rendered, renderError ->
                if (output.isDone) return@renderComplete
                if (renderError != null || rendered == null) {
                    output.completeExceptionally(
                        renderError ?: IllegalStateException("Canonical symbolic expert rendering is missing"),
                    )
                } else {
                    output.complete(rendered)
                }
            }
        }
        return output
    }

    /**
     * Tracks whichever stage can still produce late work. A cancellation racing stage handoff
     * either cancels the current stage or observes the cancelled output and immediately cancels
     * the newly attached stage. Dependent futures inherit backward cancellation to this owner.
     */
    private class ActiveStageCancellationFuture<T>(
        initialStage: CompletableFuture<*>,
    ) : CompletableFuture<T>() {
        private val lock = Any()
        private var activeStage: CompletableFuture<*> = initialStage

        fun advanceTo(nextStage: CompletableFuture<*>): Boolean = synchronized(lock) {
            if (isCancelled) {
                if (!nextStage.isDone) nextStage.cancel(true)
                false
            } else if (isDone) {
                false
            } else {
                activeStage = nextStage
                true
            }
        }

        override fun cancel(mayInterruptIfRunning: Boolean): Boolean {
            val stageToCancel: CompletableFuture<*>?
            val cancelled: Boolean
            synchronized(lock) {
                cancelled = super.cancel(mayInterruptIfRunning)
                stageToCancel = if (cancelled) activeStage else null
            }
            if (stageToCancel != null && !stageToCancel.isDone) {
                stageToCancel.cancel(mayInterruptIfRunning)
            }
            return cancelled
        }

        override fun <U> newIncompleteFuture(): CompletableFuture<U> =
            BackwardCancellationFuture(this)
    }

    private class BackwardCancellationFuture<T>(
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
            BackwardCancellationFuture(this)
    }
}
