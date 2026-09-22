package ai.zara.app.prolog

import ai.zara.app.expert.ActivationHandle
import ai.zara.app.expert.CanonicalExpertInvocationPort
import ai.zara.app.expert.ExpertLimits
import ai.zara.app.expert.ExpertRequest
import ai.zara.app.expert.ExpertResult
import ai.zara.app.expert.ExpertVerdict
import ai.zara.app.expert.PureSymbolicExpertInvocationAdapter
import ai.zara.app.expert.ZARA_EXPERT_PROTOCOL
import java.util.concurrent.CompletableFuture
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class CanonicalNaturalExpertTurnTest {
    @Test
    fun typedSelectionCrossesCanonicalInvokeAndProjectsOnlyAdmittedEvidence() {
        val activation = activation()
        val port = FakeCanonicalPort(activation)
        port.resultFactory = { request -> successResult(activation, request) }
        val turn = CanonicalNaturalExpertTurn(PureSymbolicExpertInvocationAdapter(port))

        val projected = turn.invoke(
            selection = NaturalLanguageExpertSelection(
                expertId = activation.expertId,
                expertOperation = "explain",
                input = mapOf("entity" to "alex"),
            ),
            principal = activation.principal,
            workspace = activation.workspace,
            requestId = "turn:expert:42",
            limits = zeroModelLimits(),
            idempotencyKey = "turn:expert:42:explain",
        ).get()

        val request = requireNotNull(port.lastRequest)
        assertEquals("expert.invoke", request.operation)
        assertEquals(activation.expertId, request.expertId)
        assertEquals("explain", request.expertOperation)
        assertEquals(mapOf("entity" to "alex"), request.input)
        assertEquals(0, request.limits!!.maxModelCalls)
        assertEquals("triage says alex is stable", projected.summary)
        assertEquals("evidence:triage:42", projected.evidenceRef)
    }

    @Test
    fun cancellingProjectedTurnCancelsCanonicalOwnerInvocation() {
        val activation = activation()
        val ownerFuture = CompletableFuture<ExpertResult>()
        val port = FakeCanonicalPort(activation).also { it.invokeFuture = ownerFuture }
        val turn = CanonicalNaturalExpertTurn(PureSymbolicExpertInvocationAdapter(port))

        val projected = turn.invoke(
            selection = NaturalLanguageExpertSelection(
                expertId = activation.expertId,
                expertOperation = "explain",
                input = mapOf("entity" to "alex"),
            ),
            principal = activation.principal,
            workspace = activation.workspace,
            requestId = "turn:cancel:42",
            limits = zeroModelLimits(),
            idempotencyKey = "turn:cancel:42:explain",
        )

        assertTrue(projected.cancel(true))
        assertTrue(projected.isCancelled)
        assertTrue(ownerFuture.isCancelled)
    }

    private fun activation(): ActivationHandle = ActivationHandle(
        activationId = "act:0123456789abcdef0123456789abcdef",
        principal = "local:owner",
        workspace = "local-device",
        expertId = "triage",
        expertVersion = "1.0.0",
        manifestDigest = "sha256:triage",
        registryGeneration = 7L,
        runtimeGeneration = 11L,
    )

    private fun zeroModelLimits(): ExpertLimits = ExpertLimits(
        timeoutMs = 5_000,
        maxResults = 8,
        maxOutputBytes = 64 * 1024,
        maxModelCalls = 0,
    )

    private fun successResult(
        activation: ActivationHandle,
        request: ExpertRequest,
    ): ExpertResult = ExpertResult(
        protocol = ZARA_EXPERT_PROTOCOL,
        requestId = requireNotNull(request.requestId),
        invocationId = "invocation:triage:42",
        activationId = activation.activationId,
        expertId = activation.expertId,
        expertVersion = activation.expertVersion,
        manifestDigest = activation.manifestDigest,
        expertOperation = request.expertOperation,
        resolvedRegistryGeneration = activation.registryGeneration,
        resolvedRuntimeGeneration = activation.runtimeGeneration,
        verdict = ExpertVerdict.SUCCEEDED,
        data = mapOf("summary" to "triage says alex is stable"),
        evidenceRefs = listOf("evidence:triage:42"),
        usage = mapOf(
            "provider_calls" to 0,
            "model_calls" to 0,
        ),
    )

    private class FakeCanonicalPort(
        private val activation: ActivationHandle,
    ) : CanonicalExpertInvocationPort {
        var lastRequest: ExpertRequest? = null
        var resultFactory: ((ExpertRequest) -> ExpertResult)? = null
        var invokeFuture: CompletableFuture<ExpertResult>? = null

        override fun activeActivation(
            principal: String,
            workspace: String,
            expertId: String,
        ): ActivationHandle = activation

        override fun invoke(request: ExpertRequest): CompletableFuture<ExpertResult> {
            lastRequest = request
            invokeFuture?.let { return it }
            return CompletableFuture.completedFuture(requireNotNull(resultFactory)(request))
        }

        override fun currentRegistryGeneration(): Long = activation.registryGeneration

        override fun currentRuntimeGeneration(): Long = activation.runtimeGeneration
    }
}
