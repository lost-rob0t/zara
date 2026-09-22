package ai.zara.app.expert

import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class PureSymbolicExpertLateGenerationFenceTest {
    @Test
    fun resultRejectsActivationAfterRuntimeGenerationAdvances() {
        val activation = activation()
        val request = request(activation)
        val result = result(activation, request)

        assertThrows(IllegalArgumentException::class.java) {
            PureSymbolicExpertAdmission.validateResult(
                activation = activation,
                request = request,
                result = result,
                currentRegistryGeneration = activation.registryGeneration,
                currentRuntimeGeneration = activation.runtimeGeneration + 1,
            )
        }
    }

    @Test
    fun resultRejectsActivationAfterRegistryGenerationAdvances() {
        val activation = activation()
        val request = request(activation)
        val result = result(activation, request)

        assertThrows(IllegalArgumentException::class.java) {
            PureSymbolicExpertAdmission.validateResult(
                activation = activation,
                request = request,
                result = result,
                currentRegistryGeneration = activation.registryGeneration + 1,
                currentRuntimeGeneration = activation.runtimeGeneration,
            )
        }
    }

    @Test
    fun resultAcceptsOnlyTheCurrentAdmittedGenerations() {
        val activation = activation()
        val request = request(activation)
        val result = result(activation, request)

        assertEquals(
            result,
            PureSymbolicExpertAdmission.validateResult(
                activation = activation,
                request = request,
                result = result,
                currentRegistryGeneration = activation.registryGeneration,
                currentRuntimeGeneration = activation.runtimeGeneration,
            ),
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
            requestId = "turn:late-result",
            expertOperation = "diagnose",
            input = mapOf("person" to "alex"),
            limits = ExpertLimits(
                timeoutMs = 5_000,
                maxResults = 8,
                maxOutputBytes = 64 * 1024,
                maxModelCalls = 0,
            ),
            idempotencyKey = "turn:late-result:diagnose",
        )

    private fun result(
        activation: ActivationHandle,
        request: ExpertRequest,
    ): ExpertResult = ExpertResult(
        protocol = ZARA_EXPERT_PROTOCOL,
        requestId = requireNotNull(request.requestId),
        invocationId = "invocation:late-result",
        activationId = activation.activationId,
        expertId = activation.expertId,
        expertVersion = activation.expertVersion,
        manifestDigest = activation.manifestDigest,
        expertOperation = request.expertOperation,
        resolvedRegistryGeneration = activation.registryGeneration,
        resolvedRuntimeGeneration = activation.runtimeGeneration,
        verdict = ExpertVerdict.SUCCEEDED,
        data = mapOf("summary" to "diagnosis(alex, flu)"),
        evidenceRefs = listOf("expert:diagnosis/turn:late-result"),
        usage = mapOf(
            "provider_calls" to 0,
            "model_calls" to 0,
        ),
    )
}
