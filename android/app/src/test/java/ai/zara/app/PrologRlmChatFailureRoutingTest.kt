package ai.zara.app

import ai.zara.app.runtime.AssistantRuntimeCancelledException
import ai.zara.app.runtime.AssistantRuntimeStaleGenerationException
import ai.zara.app.runtime.AssistantRuntimeTurnFailedException
import ai.zara.app.runtime.AssistantRuntimeUnavailableException
import java.io.File
import java.util.concurrent.CompletionException
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class PrologRlmChatFailureRoutingTest {
    @Test
    fun cancellationIsNotRuntimeDeathAndDoesNotRediscover() {
        val failure = classifyPrologRlmChatFailure(
            CompletionException(AssistantRuntimeCancelledException("cancelled")),
        )

        assertEquals(PrologRlmChatFailureKind.CANCELLED, failure.kind)
        assertFalse(failure.rediscover)
        assertTrue(failure.message.contains("cancelled", ignoreCase = true))
        assertFalse(failure.message.contains("unavailable", ignoreCase = true))
    }

    @Test
    fun staleGenerationIsDiscardedWithoutRediscovery() {
        val failure = classifyPrologRlmChatFailure(
            CompletionException(AssistantRuntimeStaleGenerationException("stale generation 17")),
        )

        assertEquals(PrologRlmChatFailureKind.STALE_GENERATION, failure.kind)
        assertFalse(failure.rediscover)
        assertTrue(failure.message.contains("discarded", ignoreCase = true))
        assertFalse(failure.message.contains("stale generation 17"))
    }

    @Test
    fun failedTurnLeavesLiveRuntimeSelectedWithoutRediscovery() {
        val failure = classifyPrologRlmChatFailure(
            CompletionException(AssistantRuntimeTurnFailedException("provider_error: bounded safe failure")),
        )

        assertEquals(PrologRlmChatFailureKind.TURN_FAILED, failure.kind)
        assertFalse(failure.rediscover)
        assertTrue(failure.message.contains("could not complete", ignoreCase = true))
        assertFalse(failure.message.contains("provider_error"))
        assertFalse(failure.message.contains("bounded safe failure"))
    }

    @Test
    fun unavailableRuntimeRequestsRediscovery() {
        val failure = classifyPrologRlmChatFailure(
            CompletionException(AssistantRuntimeUnavailableException("transport died")),
        )

        assertEquals(PrologRlmChatFailureKind.UNAVAILABLE, failure.kind)
        assertTrue(failure.rediscover)
        assertTrue(failure.message.contains("unavailable", ignoreCase = true))
        assertFalse(failure.message.contains("transport died"))
    }

    @Test
    fun appSessionConsumesTypedFailureRoutingBeforeRediscovery() {
        val source = File("src/main/java/ai/zara/app/AndroidAppSession.kt").readText()
        val generate = source
            .substringAfter("private fun generatePrologRlmTurn(")
            .substringBefore("private fun generateLocalModelTurn(")

        assertTrue(generate.contains("classifyPrologRlmChatFailure(failure)"))
        assertTrue(generate.contains("if (handling.rediscover)"))
        assertTrue(generate.contains("text = handling.message"))
        assertFalse(
            generate.contains(
                "text = \"The selected Prolog-RLM runtime is unavailable. Zara rechecked installed runtimes; choose another runtime in Settings → Runtime.\"",
            ),
        )
    }
}
