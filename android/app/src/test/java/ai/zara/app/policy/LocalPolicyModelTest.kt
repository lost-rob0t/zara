package ai.zara.app.policy

import ai.zara.app.localai.LocalGenerationResult
import ai.zara.app.localai.LocalModelQuantization
import java.util.concurrent.CompletableFuture
import org.junit.Test

class LocalPolicyModelTest {
    @Test fun revisionsRequireTheSameModelIdentityAndGeneration() {
        for (change in 0..4) {
            var calls = 0
            var inspections = 0
            var outcome: PolicyOutcome? = null
            val model = LocalPolicyModel(
                generate = { request ->
                    check(request.maxOutputTokens == 256)
                    calls++
                    val result = if (calls == 1) generated("original") else when (change) {
                        1 -> generated("revision", id = "other")
                        2 -> generated("revision", version = "2")
                        3 -> generated("revision", generation = 2)
                        4 -> generated("revision", quantization = LocalModelQuantization.BF16)
                        else -> generated("revision")
                    }
                    CompletableFuture.completedFuture(result)
                },
                inspect = { text ->
                    inspections++
                    CompletableFuture.completedFuture(
                        PolicyAdvice(if (text == "original") "Check evidence." else "", 1),
                    )
                },
                observe = { outcome = it },
            )
            check(model.answer("question").join() == if (change == 0) "revision" else "original")
            check(calls == 2 && inspections == if (change == 0) 2 else 1)
            check(outcome == if (change == 0) PolicyOutcome.REVISED else PolicyOutcome.REVISION_FAILED)
        }
    }

    @Test fun cancellationReachesTheActiveGenerationFuture() {
        val pending = CompletableFuture<LocalGenerationResult>()
        val cancelled = LocalPolicyModel(
            generate = { pending },
            inspect = { error("Cancelled request was inspected") },
        ).answer("question")
        check(cancelled.cancel(true) && pending.isCancelled)
    }

    @Test fun cancellationDuringInspectionCancelsPolicyWorkAndSkipsRevision() {
        var generationCalls = 0
        val inspection = CompletableFuture<PolicyAdvice>()
        val result = LocalPolicyModel(
            generate = {
                generationCalls++
                CompletableFuture.completedFuture(generated("original"))
            },
            inspect = { text ->
                check(text == "original")
                inspection
            },
        ).answer("question")

        check(generationCalls == 1)
        check(result.cancel(true))
        check(inspection.isCancelled)
        check(generationCalls == 1) { "Cancelled inspection must not start a revision generation" }
    }

    @Test fun cancellationDuringRevisionCancelsLocalGenerationAndSkipsRecheck() {
        var calls = 0
        var inspections = 0
        val revision = CompletableFuture<LocalGenerationResult>()
        val result = LocalPolicyModel(
            generate = {
                calls++
                if (calls == 1) CompletableFuture.completedFuture(generated("original")) else revision
            },
            inspect = { text ->
                inspections++
                check(text == "original") { "Cancelled revision must not be rechecked" }
                CompletableFuture.completedFuture(PolicyAdvice("Review this draft.", 1))
            },
        ).answer("question")

        check(calls == 2 && inspections == 1)
        check(result.cancel(true))
        check(revision.isCancelled)
        check(inspections == 1)
    }

    private fun generated(
        text: String,
        id: String = "qwen",
        version: String = "1",
        generation: Long = 1,
        quantization: LocalModelQuantization = LocalModelQuantization.FP16,
    ): LocalGenerationResult = LocalGenerationResult(text, id, version, quantization, generation)
}
