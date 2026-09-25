package ai.zara.app.expert

import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class PureSymbolicExpertAdmissionUsageCounterTest {
    @Test
    fun fractionalProviderUsageCannotMasqueradeAsZero() {
        listOf(0.5, -0.5, Float.MIN_VALUE).forEach { providerCalls ->
            assertRejectsUsage(
                mapOf(
                    "provider_calls" to providerCalls,
                    "model_calls" to 0,
                ),
            )
        }
    }

    @Test
    fun fractionalModelUsageCannotMasqueradeAsZero() {
        listOf(0.5, -0.5, Float.MIN_VALUE).forEach { modelCalls ->
            assertRejectsUsage(
                mapOf(
                    "provider_calls" to 0,
                    "model_calls" to modelCalls,
                ),
            )
        }
    }

    @Test
    fun floatingPointZeroUsageIsRejected() {
        listOf(
            mapOf<String, Any?>("provider_calls" to 0.0, "model_calls" to 0),
            mapOf<String, Any?>("provider_calls" to 0.0f, "model_calls" to 0L),
            mapOf<String, Any?>("provider_calls" to 0, "model_calls" to 0.0),
            mapOf<String, Any?>("provider_calls" to 0L, "model_calls" to 0.0f),
        ).forEach(::assertRejectsUsage)
    }

    @Test
    fun exactIntegralZeroRemainsAccepted() {
        val activation = activation()
        val request = request(activation)
        listOf(
            mapOf<String, Any?>("provider_calls" to 0, "model_calls" to 0L),
            mapOf<String, Any?>("provider_calls" to 0L, "model_calls" to 0),
        ).forEach { usage ->
            val result = result(activation, request, usage)
            assertEquals(
                result,
                PureSymbolicExpertAdmission.validateResult(activation, request, result),
            )
        }
    }

    private fun assertRejectsUsage(usage: Map<String, Any?>) {
        val activation = activation()
        val request = request(activation)
        val result = result(activation, request, usage)
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
            requestId = "turn:usage-counter",
            expertOperation = "diagnose",
            input = emptyMap(),
            limits = ExpertLimits(
                timeoutMs = 5_000,
                maxResults = 8,
                maxOutputBytes = 64 * 1024,
                maxModelCalls = 0,
            ),
            idempotencyKey = "turn:usage-counter:diagnose",
        )

    private fun result(
        activation: ActivationHandle,
        request: ExpertRequest,
        usage: Map<String, Any?>,
    ): ExpertResult = ExpertResult(
        protocol = ZARA_EXPERT_PROTOCOL,
        requestId = requireNotNull(request.requestId),
        invocationId = "invocation:usage-counter",
        activationId = activation.activationId,
        expertId = activation.expertId,
        expertVersion = activation.expertVersion,
        manifestDigest = activation.manifestDigest,
        expertOperation = request.expertOperation,
        resolvedRegistryGeneration = activation.registryGeneration,
        resolvedRuntimeGeneration = activation.runtimeGeneration,
        verdict = ExpertVerdict.SUCCEEDED,
        data = mapOf("summary" to "diagnosis(alex, flu)"),
        evidenceRefs = listOf("expert:diagnosis/turn:usage-counter"),
        usage = usage,
    )
}
