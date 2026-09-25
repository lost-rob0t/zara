package ai.zara.app.expert

import org.junit.Assert.assertThrows
import org.junit.Test

class PureSymbolicExpertLegacyVerifiedOutcomeCutoverTest {
    @Test
    fun newEffectSuccessRejectsLegacyGenerationlessVerifiedOutcomeEvidence() {
        val activation = ActivationHandle(
            activationId = "act:0123456789abcdef0123456789abcdef",
            principal = "local:owner",
            workspace = "local-device",
            expertId = "zara:expert/repair",
            expertVersion = "1.0.0",
            manifestDigest = "sha256:repair",
            registryGeneration = 5L,
            runtimeGeneration = 17L,
        )
        val request = PureSymbolicExpertAdmission.request(
            activation = activation,
            requestId = "turn:legacy-effect-cutover",
            expertOperation = "repair",
            input = mapOf("target" to "fixture"),
            limits = ExpertLimits(
                timeoutMs = 5_000,
                maxResults = 8,
                maxOutputBytes = 64 * 1024,
                maxModelCalls = 0,
            ),
            idempotencyKey = "turn:legacy-effect-cutover:repair",
        )
        val legacyRef = "zara.verified-outcome/v1:outcome:postcondition/repair-17"
        val result = ExpertResult(
            protocol = ZARA_EXPERT_PROTOCOL,
            requestId = requireNotNull(request.requestId),
            invocationId = "invocation:legacy-effect-cutover",
            activationId = activation.activationId,
            expertId = activation.expertId,
            expertVersion = activation.expertVersion,
            manifestDigest = activation.manifestDigest,
            expertOperation = request.expertOperation,
            resolvedRegistryGeneration = activation.registryGeneration,
            resolvedRuntimeGeneration = activation.runtimeGeneration,
            verdict = ExpertVerdict.SUCCEEDED,
            data = mapOf(
                "verified" to true,
                "verified_outcome_ref" to legacyRef,
                "postcondition_evidence" to mapOf(
                    "receipt_ref" to legacyRef,
                    "source_generation" to activation.runtimeGeneration,
                ),
            ),
            evidenceRefs = listOf(legacyRef),
            usage = mapOf(
                "provider_calls" to 0,
                "model_calls" to 0,
            ),
            effectReceipts = listOf(mapOf("receipt_ref" to "effect:repair-17")),
        )

        assertThrows(IllegalArgumentException::class.java) {
            PureSymbolicExpertAdmission.validateResult(
                activation = activation,
                request = request,
                result = result,
            )
        }
    }
}
