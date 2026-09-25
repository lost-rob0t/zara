package ai.zara.app.expert

import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

/**
 * Keeps Android admission compatible with canonical ZARA-EXPERT/1 usage semantics.
 *
 * `usage.model_calls` is the contract-owned shared-budget counter. Provider runtime
 * enablement/accounting is owned outside the expert envelope; an optional provider_calls
 * extension may only report exact integer zero, but its absence must not invalidate a
 * canonical zero-model result.
 */
class CanonicalExpertUsageContractParityTest {
    @Test
    fun canonicalModelOnlyZeroUsageRemainsAdmissible() {
        val activation = activation()
        val request = request(activation)
        val result = result(activation, request, mapOf("model_calls" to 0))

        assertEquals(
            result,
            PureSymbolicExpertAdmission.validateResult(activation, request, result),
        )
    }

    @Test
    fun explicitNonZeroProviderExtensionStillFailsClosed() {
        val activation = activation()
        val request = request(activation)
        val result = result(
            activation,
            request,
            mapOf("provider_calls" to 1, "model_calls" to 0),
        )

        assertThrows(IllegalArgumentException::class.java) {
            PureSymbolicExpertAdmission.validateResult(activation, request, result)
        }
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
            requestId = "turn:usage-contract-parity",
            expertOperation = "diagnose",
            input = emptyMap(),
            limits = ExpertLimits(
                timeoutMs = 5_000,
                maxResults = 8,
                maxOutputBytes = 64 * 1024,
                maxModelCalls = 0,
            ),
            idempotencyKey = "turn:usage-contract-parity:diagnose",
        )

    private fun result(
        activation: ActivationHandle,
        request: ExpertRequest,
        usage: Map<String, Any?>,
    ): ExpertResult = ExpertResult(
        protocol = ZARA_EXPERT_PROTOCOL,
        requestId = requireNotNull(request.requestId),
        invocationId = "invocation:usage-contract-parity",
        activationId = activation.activationId,
        expertId = activation.expertId,
        expertVersion = activation.expertVersion,
        manifestDigest = activation.manifestDigest,
        expertOperation = request.expertOperation,
        resolvedRegistryGeneration = activation.registryGeneration,
        resolvedRuntimeGeneration = activation.runtimeGeneration,
        verdict = ExpertVerdict.SUCCEEDED,
        data = mapOf("summary" to "diagnosis(alex, flu)"),
        evidenceRefs = listOf("expert:diagnosis/turn:usage-contract-parity"),
        usage = usage,
    )
}
