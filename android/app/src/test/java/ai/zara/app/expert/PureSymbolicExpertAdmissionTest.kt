package ai.zara.app.expert

import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class PureSymbolicExpertAdmissionTest {
    @Test
    fun requestPinsCanonicalInvokeIdentityGenerationsAndZeroModelBudget() {
        val request = PureSymbolicExpertAdmission.request(
            activation = activation(),
            requestId = "turn:42",
            expertOperation = "diagnose",
            input = mapOf("person" to "alex"),
            limits = zeroModelLimits(),
            idempotencyKey = "turn:42:diagnose",
        )

        assertEquals("expert.invoke", request.operation)
        assertEquals("act:0123456789abcdef0123456789abcdef", request.activationId)
        assertEquals("zara:expert/diagnosis", request.expertId)
        assertEquals(7L, request.expectedRegistryGeneration)
        assertEquals(11L, request.expectedRuntimeGeneration)
        assertEquals(0, request.limits!!.maxModelCalls)
    }

    @Test
    fun resultRejectsAnyModelUsageEvenWhenHostClaimsSuccess() {
        val activation = activation()
        val request = request(activation)
        val result = result(
            activation = activation,
            request = request,
            usage = mapOf(
                "provider_calls" to 0,
                "model_calls" to 1,
            ),
        )

        assertThrows(IllegalArgumentException::class.java) {
            PureSymbolicExpertAdmission.validateResult(activation, request, result)
        }
    }

    @Test
    fun resultRejectsAnyProviderUsageEvenWhenModelUsageIsZero() {
        val activation = activation()
        val request = request(activation)
        val result = result(
            activation = activation,
            request = request,
            usage = mapOf(
                "provider_calls" to 1,
                "model_calls" to 0,
            ),
        )

        assertThrows(IllegalArgumentException::class.java) {
            PureSymbolicExpertAdmission.validateResult(activation, request, result)
        }
    }

    @Test
    fun resultAcceptsCanonicalModelOnlyZeroUsageWithoutProviderExtension() {
        val activation = activation()
        val request = request(activation)
        val result = result(
            activation = activation,
            request = request,
            usage = mapOf("model_calls" to 0),
        )

        assertEquals(
            result,
            PureSymbolicExpertAdmission.validateResult(activation, request, result),
        )
    }

    @Test
    fun resultRejectsMissingModelUsageProof() {
        val activation = activation()
        val request = request(activation)
        val result = result(
            activation = activation,
            request = request,
            usage = mapOf("provider_calls" to 0),
        )

        assertThrows(IllegalArgumentException::class.java) {
            PureSymbolicExpertAdmission.validateResult(activation, request, result)
        }
    }

    @Test
    fun resultRejectsStaleRuntimeGeneration() {
        val activation = activation()
        val request = request(activation)
        val result = result(
            activation = activation,
            request = request,
            resolvedRuntimeGeneration = activation.runtimeGeneration + 1,
        )

        assertThrows(IllegalArgumentException::class.java) {
            PureSymbolicExpertAdmission.validateResult(activation, request, result)
        }
    }

    @Test
    fun nonSuccessVerdictsCannotBeProjectedAsConversationSuccess() {
        val activation = activation()
        val request = request(activation)
        val rejected = listOf(
            ExpertVerdict.FAILED to ExpertErrorCode.DENIED,
            ExpertVerdict.BLOCKED to ExpertErrorCode.APPROVAL_REQUIRED,
            ExpertVerdict.CANCELLED to ExpertErrorCode.CANCELLED,
            ExpertVerdict.ERROR to ExpertErrorCode.BUDGET_EXCEEDED,
        )

        rejected.forEach { (verdict, errorCode) ->
            val result = result(
                activation = activation,
                request = request,
                verdict = verdict,
                errorCode = errorCode,
                errorMessage = "canonical expert invocation did not succeed",
            )

            assertThrows(IllegalArgumentException::class.java) {
                PureSymbolicExpertAdmission.validateResult(activation, request, result)
            }
        }
    }

    @Test
    fun successfulConversationProjectionRequiresCanonicalEvidence() {
        val activation = activation()
        val request = request(activation)
        val result = result(
            activation = activation,
            request = request,
            evidenceRefs = emptyList(),
        )

        assertThrows(IllegalArgumentException::class.java) {
            PureSymbolicExpertAdmission.validateResult(activation, request, result)
        }
    }

    @Test
    fun effectSuccessRequiresFreshVerifiedPostconditionEvidence() {
        val activation = activation()
        val request = request(activation)
        val evidenceRef = verifiedOutcomeRef(activation)
        val unverified = result(
            activation = activation,
            request = request,
            evidenceRefs = listOf(evidenceRef),
            effectReceipts = listOf(mapOf("receipt_ref" to "effect:diagnosis-42")),
            data = mapOf("verified" to false),
        )

        assertThrows(IllegalArgumentException::class.java) {
            PureSymbolicExpertAdmission.validateResult(activation, request, unverified)
        }

        val verified = result(
            activation = activation,
            request = request,
            evidenceRefs = listOf(evidenceRef),
            effectReceipts = listOf(mapOf("receipt_ref" to "effect:diagnosis-42")),
            data = mapOf(
                "verified" to true,
                "verified_outcome_ref" to evidenceRef,
                "postcondition_evidence" to mapOf(
                    "receipt_ref" to evidenceRef,
                    "source_generation" to activation.runtimeGeneration,
                ),
            ),
        )

        assertEquals(
            verified,
            PureSymbolicExpertAdmission.validateResult(activation, request, verified),
        )
    }

    @Test
    fun effectSuccessRejectsStaleOrUnboundPostconditionEvidence() {
        val activation = activation()
        val request = request(activation)
        val evidenceRef = verifiedOutcomeRef(activation)
        val effectReceipts = listOf(mapOf("receipt_ref" to "effect:diagnosis-42"))

        val stale = result(
            activation = activation,
            request = request,
            evidenceRefs = listOf(evidenceRef),
            effectReceipts = effectReceipts,
            data = mapOf(
                "verified" to true,
                "verified_outcome_ref" to evidenceRef,
                "postcondition_evidence" to mapOf(
                    "receipt_ref" to evidenceRef,
                    "source_generation" to activation.runtimeGeneration - 1,
                ),
            ),
        )
        assertThrows(IllegalArgumentException::class.java) {
            PureSymbolicExpertAdmission.validateResult(activation, request, stale)
        }

        val unbound = result(
            activation = activation,
            request = request,
            evidenceRefs = listOf(evidenceRef),
            effectReceipts = effectReceipts,
            data = mapOf(
                "verified" to true,
                "verified_outcome_ref" to evidenceRef,
                "postcondition_evidence" to mapOf(
                    "receipt_ref" to "zara.verified-outcome/v2:${activation.runtimeGeneration}:effect:other-turn",
                    "source_generation" to activation.runtimeGeneration,
                ),
            ),
        )
        assertThrows(IllegalArgumentException::class.java) {
            PureSymbolicExpertAdmission.validateResult(activation, request, unbound)
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
            requestId = "turn:42",
            expertOperation = "diagnose",
            input = mapOf("person" to "alex"),
            limits = zeroModelLimits(),
            idempotencyKey = "turn:42:diagnose",
        )

    private fun zeroModelLimits(): ExpertLimits = ExpertLimits(
        timeoutMs = 5_000,
        maxResults = 8,
        maxOutputBytes = 64 * 1024,
        maxModelCalls = 0,
    )

    private fun verifiedOutcomeRef(activation: ActivationHandle): String =
        "zara.verified-outcome/v2:${activation.runtimeGeneration}:effect:diagnosis-42"

    private fun result(
        activation: ActivationHandle,
        request: ExpertRequest,
        resolvedRuntimeGeneration: Long = activation.runtimeGeneration,
        usage: Map<String, Any?> = mapOf(
            "provider_calls" to 0,
            "model_calls" to 0,
        ),
        evidenceRefs: List<String> = listOf("expert:diagnosis/turn:42"),
        effectReceipts: List<Map<String, Any?>> = emptyList(),
        data: Map<String, Any?> = mapOf("summary" to "diagnosis(alex, flu)"),
        verdict: ExpertVerdict = ExpertVerdict.SUCCEEDED,
        errorCode: ExpertErrorCode? = null,
        errorMessage: String = "",
    ): ExpertResult = ExpertResult(
        protocol = ZARA_EXPERT_PROTOCOL,
        requestId = requireNotNull(request.requestId),
        invocationId = "invocation:42",
        activationId = activation.activationId,
        expertId = activation.expertId,
        expertVersion = activation.expertVersion,
        manifestDigest = activation.manifestDigest,
        expertOperation = request.expertOperation,
        resolvedRegistryGeneration = activation.registryGeneration,
        resolvedRuntimeGeneration = resolvedRuntimeGeneration,
        verdict = verdict,
        data = data,
        evidenceRefs = evidenceRefs,
        usage = usage,
        effectReceipts = effectReceipts,
        errorCode = errorCode,
        errorMessage = errorMessage,
    )
}
