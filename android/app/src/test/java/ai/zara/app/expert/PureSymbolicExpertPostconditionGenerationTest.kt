package ai.zara.app.expert

import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class PureSymbolicExpertPostconditionGenerationTest {
    @Test
    fun fractionalPostconditionGenerationCannotTruncateIntoCurrentRuntimeGeneration() {
        val activation = activation()
        val request = request(activation)

        listOf<Number>(11.5, 11.9f).forEach { sourceGeneration ->
            val result = effectResult(
                activation = activation,
                request = request,
                sourceGeneration = sourceGeneration,
            )

            assertThrows(IllegalArgumentException::class.java) {
                PureSymbolicExpertAdmission.validateResult(activation, request, result)
            }
        }
    }

    @Test
    fun exactIntegralPostconditionGenerationRemainsAccepted() {
        val activation = activation()
        val request = request(activation)
        val result = effectResult(
            activation = activation,
            request = request,
            sourceGeneration = activation.runtimeGeneration,
        )

        assertEquals(
            result,
            PureSymbolicExpertAdmission.validateResult(activation, request, result),
        )
    }

    private fun activation(): ActivationHandle = ActivationHandle(
        activationId = "act:0123456789abcdef0123456789abcdef",
        principal = "local:owner",
        workspace = "local-device",
        expertId = "zara:expert/diagnosis",
        expertVersion = "1.0.0",
        manifestDigest = "sha256:diagnosis",
        registryGeneration = 7L,
        runtimeGeneration = 11L,
    )

    private fun request(activation: ActivationHandle): ExpertRequest =
        PureSymbolicExpertAdmission.request(
            activation = activation,
            requestId = "turn:postcondition-generation",
            expertOperation = "diagnose",
            input = mapOf("person" to "alex"),
            limits = ExpertLimits(
                timeoutMs = 5_000,
                maxResults = 8,
                maxOutputBytes = 64 * 1024,
                maxModelCalls = 0,
            ),
            idempotencyKey = "turn:postcondition-generation:diagnose",
        )

    private fun effectResult(
        activation: ActivationHandle,
        request: ExpertRequest,
        sourceGeneration: Number,
    ): ExpertResult {
        val evidenceRef = "zara.verified-outcome/v1:effect:diagnosis-42"
        return ExpertResult(
            protocol = ZARA_EXPERT_PROTOCOL,
            requestId = requireNotNull(request.requestId),
            invocationId = "invocation:postcondition-generation",
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
                "verified_outcome_ref" to evidenceRef,
                "postcondition_evidence" to mapOf(
                    "receipt_ref" to evidenceRef,
                    "source_generation" to sourceGeneration,
                ),
            ),
            evidenceRefs = listOf(evidenceRef),
            usage = mapOf(
                "provider_calls" to 0,
                "model_calls" to 0,
            ),
            effectReceipts = listOf(mapOf("receipt_ref" to "effect:diagnosis-42")),
        )
    }
}
