package ai.zara.app.expert

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

class ExpertInvocationContractTest {
    @Test
    fun activationHandleCarriesCanonicalGenerationFences() {
        val handle = ActivationHandle(
            activationId = "act:0123456789abcdef0123456789abcdef",
            principal = "local:owner",
            workspace = "project-a",
            expertId = "zara:expert/diagnosis",
            expertVersion = "1.0.0",
            manifestDigest = "sha256:diagnosis",
            registryGeneration = 7L,
            runtimeGeneration = 11L,
        )

        assertEquals(7L, handle.registryGeneration)
        assertEquals(11L, handle.runtimeGeneration)
        expectFailure<IllegalArgumentException> {
            handle.copy(runtimeGeneration = -1L)
        }
    }

    @Test
    fun pureSymbolicInvokeCarriesZeroModelBudgetAndExpectedGenerations() {
        val request = ExpertRequest(
            requestId = "req:turn-42",
            operation = "expert.invoke",
            activationId = "act:0123456789abcdef0123456789abcdef",
            expertId = "zara:expert/diagnosis",
            expertOperation = "diagnosis_explain",
            expectedRegistryGeneration = 7L,
            expectedRuntimeGeneration = 11L,
            input = mapOf("entity" to "alex"),
            limits = ExpertLimits(
                timeoutMs = 5_000,
                maxResults = 1,
                maxOutputBytes = 16_384,
                maxModelCalls = 0,
            ),
            idempotencyKey = "turn:42",
        )

        assertEquals("expert.invoke", request.operation)
        assertEquals(7L, request.expectedRegistryGeneration)
        assertEquals(11L, request.expectedRuntimeGeneration)
        assertEquals(0, request.limits!!.maxModelCalls)
        expectFailure<IllegalArgumentException> {
            request.copy(operation = "provider.invoke")
        }
    }

    @Test
    fun resultCarriesCanonicalEvidenceAndEffectReceiptsWithoutInventingAuthority() {
        val result = ExpertResult(
            protocol = ZARA_EXPERT_PROTOCOL,
            requestId = "req:turn-42",
            invocationId = "inv:turn-42",
            activationId = "act:0123456789abcdef0123456789abcdef",
            expertId = "zara:expert/diagnosis",
            expertVersion = "1.0.0",
            manifestDigest = "sha256:diagnosis",
            expertOperation = "diagnosis_explain",
            resolvedRegistryGeneration = 7L,
            resolvedRuntimeGeneration = 11L,
            verdict = ExpertVerdict.SUCCEEDED,
            data = mapOf("summary" to "diagnosis(alex, flu)"),
            evidenceRefs = listOf("expert:diagnosis/turn-42"),
            usage = mapOf("model_calls" to 0L),
            effectReceipts = listOf(
                mapOf(
                    "effect" to "none",
                    "verified" to true,
                ),
            ),
        )

        assertEquals(listOf("expert:diagnosis/turn-42"), result.evidenceRefs)
        assertEquals(1, result.effectReceipts.size)
        assertTrue(result.effectReceipts.single()["verified"] == true)
        expectFailure<IllegalArgumentException> {
            result.copy(evidenceRefs = listOf("bad\nref"))
        }
    }

    @Test
    fun invocationPayloadsRejectNonFiniteNumbers() {
        val request = ExpertRequest(
            requestId = "req:finite",
            operation = "expert.invoke",
            activationId = "act:0123456789abcdef0123456789abcdef",
            expertId = "zara:expert/diagnosis",
            expertOperation = "diagnosis_explain",
        )
        val result = ExpertResult(
            protocol = ZARA_EXPERT_PROTOCOL,
            requestId = "req:finite",
            invocationId = "inv:finite",
            activationId = "act:0123456789abcdef0123456789abcdef",
            expertId = "zara:expert/diagnosis",
            expertVersion = "1.0.0",
            manifestDigest = "sha256:diagnosis",
            expertOperation = "diagnosis_explain",
            resolvedRegistryGeneration = 7L,
            resolvedRuntimeGeneration = 11L,
            verdict = ExpertVerdict.SUCCEEDED,
        )

        val nonFinite = listOf<Any>(
            Double.NaN,
            Double.POSITIVE_INFINITY,
            Double.NEGATIVE_INFINITY,
            Float.NaN,
            Float.POSITIVE_INFINITY,
            Float.NEGATIVE_INFINITY,
        )
        nonFinite.forEach { value ->
            expectFailure<IllegalArgumentException> {
                request.copy(input = mapOf("score" to value))
            }
            expectFailure<IllegalArgumentException> {
                result.copy(data = mapOf("score" to value))
            }
            expectFailure<IllegalArgumentException> {
                result.copy(usage = mapOf("provider_calls" to value))
            }
            expectFailure<IllegalArgumentException> {
                result.copy(effectReceipts = listOf(mapOf("latency" to value)))
            }
        }
    }

    private inline fun <reified T : Throwable> expectFailure(block: () -> Unit): T {
        try {
            block()
        } catch (error: Throwable) {
            if (error is T) return error
            throw error
        }
        fail("expected ${T::class.simpleName}")
        error("unreachable")
    }
}
