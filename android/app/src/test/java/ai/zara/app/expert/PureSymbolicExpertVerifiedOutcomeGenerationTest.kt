package ai.zara.app.expert

import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class PureSymbolicExpertVerifiedOutcomeGenerationTest {
    @Test
    fun effectSuccessRejectsGenerationBoundEvidenceFromAnotherRuntime() {
        val activation = activation()
        val request = request(activation)
        val staleRef = "zara.verified-outcome/v2:12:outcome:postcondition/diagnosis-42"
        val stale = result(activation, request, staleRef)

        assertThrows(IllegalArgumentException::class.java) {
            PureSymbolicExpertAdmission.validateResult(activation, request, stale)
        }
    }

    @Test
    fun effectSuccessAcceptsGenerationBoundEvidenceFromAdmittedRuntime() {
        val activation = activation()
        val request = request(activation)
        val freshRef = "zara.verified-outcome/v2:11:outcome:postcondition/diagnosis-42"
        val fresh = result(activation, request, freshRef)

        assertEquals(
            fresh,
            PureSymbolicExpertAdmission.validateResult(activation, request, fresh),
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
            requestId = "turn:verified-generation",
            expertOperation = "diagnose",
            input = mapOf("person" to "alex"),
            limits = ExpertLimits(
                timeoutMs = 5_000,
                maxResults = 8,
                maxOutputBytes = 64 * 1024,
                maxModelCalls = 0,
            ),
            idempotencyKey = "turn:verified-generation:diagnose",
        )

    private fun result(
        activation: ActivationHandle,
        request: ExpertRequest,
        evidenceRef: String,
    ): ExpertResult = ExpertResult(
        protocol = ZARA_EXPERT_PROTOCOL,
        requestId = requireNotNull(request.requestId),
        invocationId = "invocation:verified-generation",
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
                "source_generation" to activation.runtimeGeneration,
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
