package ai.zara.app.expert

import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutionException
import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Assert.assertTrue
import org.junit.Test

class PureSymbolicExpertInvocationBudgetPreflightTest {
    @Test
    fun nonZeroModelBudgetFailsBeforeCanonicalOwnerLookupOrInvocation() {
        val port = CountingCanonicalPort()
        val future = PureSymbolicExpertInvocationAdapter(port).invoke(
            principal = "local:owner",
            workspace = "local-device",
            expertId = "zara:expert/diagnosis",
            requestId = "turn:budget-preflight",
            expertOperation = "diagnose",
            input = mapOf("person" to "alex"),
            limits = ExpertLimits(
                timeoutMs = 5_000,
                maxResults = 8,
                maxOutputBytes = 64 * 1024,
                maxModelCalls = 1,
            ),
            idempotencyKey = "turn:budget-preflight:diagnose",
        )

        val error = assertThrows(ExecutionException::class.java) { future.get() }
        assertTrue(error.cause is IllegalArgumentException)
        assertEquals(
            "Pure-symbolic budget drift must fail before touching canonical expert authority",
            0,
            port.activationLookupCount,
        )
        assertEquals(0, port.invokeCount)
    }

    private class CountingCanonicalPort : CanonicalExpertInvocationPort {
        var activationLookupCount = 0
        var invokeCount = 0

        override fun activeActivation(
            principal: String,
            workspace: String,
            expertId: String,
        ): ActivationHandle? {
            activationLookupCount += 1
            return null
        }

        override fun invoke(request: ExpertRequest): CompletableFuture<ExpertResult> {
            invokeCount += 1
            return CompletableFuture.failedFuture(
                IllegalStateException("owner invocation must not run for an invalid pure-symbolic budget"),
            )
        }

        override fun currentRegistryGeneration(): Long = 0L

        override fun currentRuntimeGeneration(): Long = 0L
    }
}
