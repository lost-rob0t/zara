package ai.zara.app.expert

import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutionException
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertThrows
import org.junit.Assert.assertTrue
import org.junit.Test

class PureSymbolicExpertInvocationAdapterTest {
    @Test
    fun missingCanonicalActivationFailsClosedBeforeInvocation() {
        val port = FakeCanonicalPort(activation = null)
        val future = adapter(port).invoke(
            principal = "local:owner",
            workspace = "local-device",
            expertId = "zara:expert/diagnosis",
            requestId = "turn:42",
            expertOperation = "diagnose",
            input = mapOf("person" to "alex"),
            limits = zeroModelLimits(),
            idempotencyKey = "turn:42:diagnose",
        )

        val error = assertThrows(ExecutionException::class.java) { future.get() }
        assertTrue(error.cause is IllegalStateException)
        assertEquals(0, port.invokeCount)
    }

    @Test
    fun canonicalOwnerReceivesExpertInvokeWithSharedZeroModelBudget() {
        val activation = activation()
        val port = FakeCanonicalPort(activation)
        port.resultFactory = { request -> successResult(activation, request) }

        val result = adapter(port).invoke(
            principal = activation.principal,
            workspace = activation.workspace,
            expertId = activation.expertId,
            requestId = "turn:42",
            expertOperation = "diagnose",
            input = mapOf("person" to "alex"),
            limits = zeroModelLimits(),
            idempotencyKey = "turn:42:diagnose",
        ).get()

        val request = assertNotNull(port.lastRequest).let { port.lastRequest!! }
        assertEquals(1, port.invokeCount)
        assertEquals("expert.invoke", request.operation)
        assertEquals(activation.activationId, request.activationId)
        assertEquals(activation.registryGeneration, request.expectedRegistryGeneration)
        assertEquals(activation.runtimeGeneration, request.expectedRuntimeGeneration)
        assertEquals(0, request.limits!!.maxModelCalls)
        assertEquals(0, result.usage["provider_calls"])
        assertEquals(0, result.usage["model_calls"])
        assertEquals(listOf("evidence:diagnosis:42"), result.evidenceRefs)
    }

    @Test
    fun mismatchedActivationScopeFailsBeforeInvocation() {
        val activation = activation().copy(workspace = "other-workspace")
        val port = FakeCanonicalPort(activation)
        val future = adapter(port).invoke(
            principal = "local:owner",
            workspace = "local-device",
            expertId = activation.expertId,
            requestId = "turn:42",
            expertOperation = "diagnose",
            input = emptyMap(),
            limits = zeroModelLimits(),
            idempotencyKey = "turn:42:diagnose",
        )

        val error = assertThrows(ExecutionException::class.java) { future.get() }
        assertTrue(error.cause is IllegalArgumentException)
        assertEquals(0, port.invokeCount)
    }

    @Test
    fun liveGenerationAdvanceRejectsLateCanonicalResult() {
        val activation = activation()
        val port = FakeCanonicalPort(activation)
        port.resultFactory = { request -> successResult(activation, request) }
        port.liveRuntimeGeneration = activation.runtimeGeneration + 1L

        val future = adapter(port).invoke(
            principal = activation.principal,
            workspace = activation.workspace,
            expertId = activation.expertId,
            requestId = "turn:42",
            expertOperation = "diagnose",
            input = emptyMap(),
            limits = zeroModelLimits(),
            idempotencyKey = "turn:42:diagnose",
        )

        val error = assertThrows(ExecutionException::class.java) { future.get() }
        assertTrue(error.cause is IllegalArgumentException)
        assertEquals(1, port.invokeCount)
    }

    private fun adapter(port: CanonicalExpertInvocationPort) =
        PureSymbolicExpertInvocationAdapter(port)

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
        invocationId = "invocation:diagnosis:42",
        activationId = activation.activationId,
        expertId = activation.expertId,
        expertVersion = activation.expertVersion,
        manifestDigest = activation.manifestDigest,
        expertOperation = request.expertOperation,
        resolvedRegistryGeneration = activation.registryGeneration,
        resolvedRuntimeGeneration = activation.runtimeGeneration,
        verdict = ExpertVerdict.SUCCEEDED,
        data = mapOf("summary" to "diagnosis(alex,flu)"),
        evidenceRefs = listOf("evidence:diagnosis:42"),
        usage = mapOf(
            "provider_calls" to 0,
            "model_calls" to 0,
        ),
    )

    private class FakeCanonicalPort(
        private val activation: ActivationHandle?,
    ) : CanonicalExpertInvocationPort {
        var invokeCount: Int = 0
        var lastRequest: ExpertRequest? = null
        var resultFactory: ((ExpertRequest) -> ExpertResult)? = null
        var liveRegistryGeneration: Long = activation?.registryGeneration ?: 0L
        var liveRuntimeGeneration: Long = activation?.runtimeGeneration ?: 0L

        override fun activeActivation(
            principal: String,
            workspace: String,
            expertId: String,
        ): ActivationHandle? = activation

        override fun invoke(request: ExpertRequest): CompletableFuture<ExpertResult> {
            invokeCount += 1
            lastRequest = request
            val factory = requireNotNull(resultFactory) { "test result factory is missing" }
            return CompletableFuture.completedFuture(factory(request))
        }

        override fun currentRegistryGeneration(): Long = liveRegistryGeneration

        override fun currentRuntimeGeneration(): Long = liveRuntimeGeneration
    }
}
