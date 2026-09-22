package ai.zara.app.expert

/**
 * Pure-symbolic conversation adapter for the canonical ZARA-EXPERT/1 envelope.
 *
 * This owns no registry, lifecycle, executor, permission state, provider runtime, or history.
 * It only constructs an invoke request against an already-issued activation and verifies that
 * the canonical owner returned the same authority/generation/budget identity before a result can
 * be projected into conversation state.
 */
object PureSymbolicExpertAdmission {
    fun request(
        activation: ActivationHandle,
        requestId: String,
        expertOperation: String,
        input: Map<String, Any?>,
        limits: ExpertLimits,
        idempotencyKey: String,
    ): ExpertRequest {
        require(limits.maxModelCalls == 0) {
            "Pure-symbolic expert invocation requires maxModelCalls=0"
        }
        return ExpertRequest(
            requestId = requestId,
            operation = "expert.invoke",
            activationId = activation.activationId,
            expertId = activation.expertId,
            expertOperation = expertOperation,
            expectedRegistryGeneration = activation.registryGeneration,
            expectedRuntimeGeneration = activation.runtimeGeneration,
            input = input,
            limits = limits,
            idempotencyKey = idempotencyKey,
        )
    }

    fun validateResult(
        activation: ActivationHandle,
        request: ExpertRequest,
        result: ExpertResult,
    ): ExpertResult = validateResult(
        activation = activation,
        request = request,
        result = result,
        currentRegistryGeneration = activation.registryGeneration,
        currentRuntimeGeneration = activation.runtimeGeneration,
    )

    fun validateResult(
        activation: ActivationHandle,
        request: ExpertRequest,
        result: ExpertResult,
        currentRegistryGeneration: Long,
        currentRuntimeGeneration: Long,
    ): ExpertResult {
        require(currentRegistryGeneration >= 0L) {
            "current registry generation must be non-negative"
        }
        require(currentRuntimeGeneration >= 0L) {
            "current runtime generation must be non-negative"
        }
        require(activation.registryGeneration == currentRegistryGeneration) {
            "activation registry generation is stale"
        }
        require(activation.runtimeGeneration == currentRuntimeGeneration) {
            "activation runtime generation is stale"
        }
        require(request.operation == "expert.invoke") {
            "Pure-symbolic expert result requires canonical expert.invoke admission"
        }
        val limits = requireNotNull(request.limits) {
            "Pure-symbolic expert invocation requires explicit shared limits"
        }
        require(limits.maxModelCalls == 0) {
            "Pure-symbolic expert invocation exceeded the zero-model budget"
        }
        require(request.activationId == activation.activationId) { "activation identity changed" }
        require(request.expertId == activation.expertId) { "expert identity changed" }
        require(request.expectedRegistryGeneration == activation.registryGeneration) {
            "registry generation is stale"
        }
        require(request.expectedRuntimeGeneration == activation.runtimeGeneration) {
            "runtime generation is stale"
        }

        val requestId = requireNotNull(request.requestId) {
            "Pure-symbolic expert invocation requires request identity"
        }
        require(result.requestId == requestId) { "expert result request identity changed" }
        require(result.activationId == activation.activationId) { "expert result activation identity changed" }
        require(result.expertId == activation.expertId) { "expert result expert identity changed" }
        require(result.expertVersion == activation.expertVersion) { "expert result version changed" }
        require(result.manifestDigest == activation.manifestDigest) { "expert result manifest changed" }
        require(result.expertOperation == request.expertOperation) { "expert result operation changed" }
        require(result.resolvedRegistryGeneration == activation.registryGeneration) {
            "expert result registry generation is stale"
        }
        require(result.resolvedRuntimeGeneration == activation.runtimeGeneration) {
            "expert result runtime generation is stale"
        }

        require(isExactZeroUsageCounter(result.usage["provider_calls"])) {
            "Pure-symbolic expert result must prove usage.provider_calls == 0"
        }
        require(isExactZeroUsageCounter(result.usage["model_calls"])) {
            "Pure-symbolic expert result must prove usage.model_calls == 0"
        }

        require(result.verdict == ExpertVerdict.SUCCEEDED) {
            "Only a succeeded canonical expert result may be projected as conversation success"
        }
        require(result.errorCode == null && result.errorMessage.isEmpty()) {
            "Successful expert result cannot carry an error"
        }
        require(result.evidenceRefs.isNotEmpty()) {
            "Successful pure-symbolic expert result requires canonical evidence"
        }
        if (result.effectReceipts.isNotEmpty()) {
            require(result.data["verified"] == true) {
                "Effect-dependent success requires fresh verified postcondition evidence"
            }
            val verifiedOutcomeRef = result.data["verified_outcome_ref"]
            require(verifiedOutcomeRef is String && verifiedOutcomeRef.isNotBlank()) {
                "Effect-dependent success requires a verified outcome reference"
            }
            require(
                isGenerationCompatibleVerifiedOutcomeRef(
                    verifiedOutcomeRef,
                    activation.runtimeGeneration,
                )
            ) {
                "Verified outcome reference is malformed or stale for the admitted runtime generation"
            }
            val postconditionEvidence = result.data["postcondition_evidence"]
            require(postconditionEvidence is Map<*, *> && postconditionEvidence.isNotEmpty()) {
                "Effect-dependent success requires postcondition evidence"
            }
            require(result.evidenceRefs.contains(verifiedOutcomeRef)) {
                "Verified outcome reference must be present in canonical evidenceRefs"
            }
            require(postconditionEvidence["receipt_ref"] == verifiedOutcomeRef) {
                "Postcondition evidence must be bound to the verified outcome reference"
            }
            require(
                isExactGeneration(
                    postconditionEvidence["source_generation"],
                    activation.runtimeGeneration,
                )
            ) {
                "Postcondition evidence is stale for the admitted runtime generation"
            }
        }
        return result
    }

    private fun isExactZeroUsageCounter(value: Any?): Boolean = when (value) {
        is Int -> value == 0
        is Long -> value == 0L
        is Float -> value.isFinite() && value == 0.0f
        is Double -> value.isFinite() && value == 0.0
        else -> false
    }

    private fun isExactGeneration(value: Any?, expected: Long): Boolean = when (value) {
        is Int -> value.toLong() == expected
        is Long -> value == expected
        else -> false
    }

    private fun isGenerationCompatibleVerifiedOutcomeRef(
        reference: String,
        expectedRuntimeGeneration: Long,
    ): Boolean {
        val match = verifiedOutcomeV2RefPattern.matchEntire(reference) ?: return false
        return match.groupValues[1].toLongOrNull() == expectedRuntimeGeneration
    }

    private val verifiedOutcomeV2RefPattern = Regex(
        "^zara\\.verified-outcome/v2:([1-9][0-9]*):(effect|outcome):" +
            "[A-Za-z0-9][A-Za-z0-9._:/#-]{0,383}$",
    )
}
