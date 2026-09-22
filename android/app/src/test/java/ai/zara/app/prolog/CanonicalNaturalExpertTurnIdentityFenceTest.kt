package ai.zara.app.prolog

import ai.zara.app.expert.ActivationHandle
import ai.zara.app.expert.CanonicalExpertInvocationPort
import ai.zara.app.expert.ExpertLimits
import ai.zara.app.expert.ExpertRequest
import ai.zara.app.expert.ExpertResult
import ai.zara.app.expert.PureSymbolicExpertInvocationAdapter
import java.util.concurrent.CompletableFuture
import java.util.concurrent.CompletionException
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class CanonicalNaturalExpertTurnIdentityFenceTest {
    @Test
    fun mismatchedIdempotencyIdentityFailsBeforeCanonicalOwnerLookup() {
        assertIdentityFailsBeforeOwner(
            requestId = "turn:expert:42",
            idempotencyKey = "turn:expert:42:other",
        )
    }

    @Test
    fun nonPortableTurnRequestIdentityFailsBeforeCanonicalOwnerLookup() {
        assertIdentityFailsBeforeOwner(
            requestId = "turn expert 42",
            idempotencyKey = "turn expert 42:explain",
        )
    }

    private fun assertIdentityFailsBeforeOwner(
        requestId: String,
        idempotencyKey: String,
    ) {
        val port = CountingCanonicalPort(activation())
        val turn = CanonicalNaturalExpertTurn(PureSymbolicExpertInvocationAdapter(port))

        val failure = runCatching {
            turn.invoke(
                selection = NaturalLanguageExpertSelection(
                    expertId = "triage",
                    expertOperation = "explain",
                    input = mapOf("entity" to "alex"),
                ),
                principal = "local:owner",
                workspace = "local-device",
                requestId = requestId,
                limits = zeroModelLimits(),
                idempotencyKey = idempotencyKey,
            ).join()
        }.exceptionOrNull()

        assertTrue(failure is CompletionException)
        assertTrue(failure?.cause is IllegalArgumentException)
        assertEquals(0, port.activationLookupCount)
        assertEquals(0, port.invokeCount)
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

    private class CountingCanonicalPort(
        private val activation: ActivationHandle,
    ) : CanonicalExpertInvocationPort {
        var activationLookupCount = 0
        var invokeCount = 0

        override fun activeActivation(
            principal: String,
            workspace: String,
            expertId: String,
        ): ActivationHandle {
            activationLookupCount += 1
            return activation
        }

        override fun invoke(request: ExpertRequest): CompletableFuture<ExpertResult> {
            invokeCount += 1
            return CompletableFuture.failedFuture(
                IllegalStateException("canonical owner must not be invoked for invalid turn identity"),
            )
        }

        override fun currentRegistryGeneration(): Long = activation.registryGeneration

        override fun currentRuntimeGeneration(): Long = activation.runtimeGeneration
    }
}
