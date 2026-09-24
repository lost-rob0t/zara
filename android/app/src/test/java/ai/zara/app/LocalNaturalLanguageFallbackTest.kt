package ai.zara.app

import ai.zara.app.runtime.LocalQueryResult
import ai.zara.app.runtime.TextTurnResult
import java.io.File
import java.util.concurrent.CompletableFuture
import java.util.concurrent.TimeUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test

class LocalNaturalLanguageFallbackTest {
    @Test
    fun autoRuntimePrefersAuthenticatedRemoteBeforeLocalFallback() {
        val source = File("src/main/java/ai/zara/app/AndroidAppSession.kt").readText()
        val submit = source.substringAfter("fun submitText(")
            .substringBefore("private fun submitLocalText")

        assertTrue(submit.contains("RuntimeMode.Symbolic -> return submitLocalText("))
        assertTrue(submit.contains("allowModelFallback = false"))
        assertTrue(submit.contains("RuntimeMode.Local -> return submitLocalText(text, localConversationId)"))
        assertTrue(submit.contains("RuntimeMode.Auto -> return submitAutoRemoteFirst("))
        assertTrue(submit.contains("remoteConnected = remoteConnected"))
        assertTrue(submit.contains("localConversationId = localConversationId"))
        assertTrue(submit.contains("remoteConversationId = remoteConversationId"))
        assertFalse(submit.contains("RuntimeMode.Auto -> if (!remoteConnected) return submitLocalText(text)"))
        val auto = source.substringAfter("private fun submitAutoRemoteFirst(")
            .substringBefore("private fun submitRemoteText")
        assertTrue(auto.contains("if (remoteConnected)"))
        assertTrue(auto.contains("return submitRemoteText(text, remoteConversationId)"))
        assertTrue(auto.contains("return submitLocalText(text, localConversationId)"))
    }

    @Test
    fun autoRuntimeKeepsExplicitSymbolicCommandsLocalWhenRemoteIsConnected() {
        val source = File("src/main/java/ai/zara/app/AndroidAppSession.kt").readText()
        val auto = source.substringAfter("private fun submitAutoRemoteFirst(")
            .substringBefore("private fun submitRemoteText")

        val explicitIndex = auto.indexOf("if (explicitSymbolic)")
        val remoteIndex = auto.indexOf("if (remoteConnected)")

        assertTrue(auto.contains("query.startsWith(\"?-\")"))
        assertTrue(auto.contains("query.startsWith(\"/prolog \")"))
        assertTrue(auto.contains("query.startsWith(\"/expert \")"))
        assertTrue("explicit symbolic routing must be checked", explicitIndex >= 0)
        assertTrue(
            "explicit symbolic input must stay local before Auto considers Remote",
            remoteIndex >= 0 && explicitIndex < remoteIndex,
        )
        assertTrue(auto.contains("return submitLocalText(text, localConversationId)"))
    }

    @Test
    fun strictLocalModeSuspendsRemoteBeforePublishingLocalMode() {
        val source = File("src/main/java/ai/zara/app/AndroidAppSession.kt").readText()
        val setter = source.substringAfter("fun setRuntimeMode(mode: RuntimeMode) {")
            .substringBefore("fun localServerState()")

        val suspendIndex = setter.indexOf("controller.suspendRemoteForLocalMode()")
        val publishIndex = setter.indexOf("runtimeMode = mode")

        assertTrue("entering strict Local must suspend remote transport", suspendIndex >= 0)
        assertTrue(
            "remote transport must be fenced before Local becomes the published routing mode",
            publishIndex >= 0 && suspendIndex < publishIndex,
        )
        assertTrue(
            "strict local modes must fence remote transport",
            setter.contains("mode in setOf(RuntimeMode.Symbolic, RuntimeMode.Local)"),
        )
        assertTrue(
            "switching between Symbolic and Local must not churn the remote generation",
            setter.contains("previous !in setOf(RuntimeMode.Symbolic, RuntimeMode.Local)"),
        )
    }

    @Test
    fun symbolicFailureFallsThroughInsteadOfEscaping() {
        val symbolic = CompletableFuture.failedFuture<LocalQueryResult>(
            IllegalStateException("Trealla native evaluation failed"),
        )
        var fallbackError: Throwable? = null

        val result = recoverLocalNaturalLanguageTurn(
            symbolic = symbolic,
            onMatch = { error("symbolic result must not win") },
            onFallback = { error ->
                fallbackError = error
                CompletableFuture.completedFuture(
                    TextTurnResult(
                        conversationId = "local-device",
                        turnId = "turn-fallback",
                        text = "fallback",
                        success = false,
                    )
                )
            },
        ).get(2, TimeUnit.SECONDS)

        assertEquals("fallback", result.text)
        assertFalse(result.success)
        assertNotNull(fallbackError)
    }

    @Test
    fun emptySymbolicResultFallsThroughWithoutPretendingFailure() {
        val symbolic = CompletableFuture.completedFuture(
            LocalQueryResult(
                query = "resolve_frames(...)",
                terms = emptyList(),
                generation = 4,
            )
        )
        var fallbackError: Throwable? = IllegalStateException("not called")

        val result = recoverLocalNaturalLanguageTurn(
            symbolic = symbolic,
            onMatch = { error("empty symbolic result must not win") },
            onFallback = { error ->
                fallbackError = error
                CompletableFuture.completedFuture(
                    TextTurnResult(
                        conversationId = "local-device",
                        turnId = "turn-no-match",
                        text = "model",
                        success = true,
                    )
                )
            },
        ).get(2, TimeUnit.SECONDS)

        assertEquals("model", result.text)
        assertTrue(result.success)
        assertNull(fallbackError)
    }

    @Test
    fun symbolicMatchDoesNotInvokeFallback() {
        val symbolic = CompletableFuture.completedFuture(
            LocalQueryResult(
                query = "parent(alice, Result)",
                terms = listOf("bob"),
                generation = 2,
            )
        )
        var fallbackCalled = false

        val result = recoverLocalNaturalLanguageTurn(
            symbolic = symbolic,
            onMatch = { matched ->
                TextTurnResult(
                    conversationId = "local-device",
                    turnId = "turn-symbolic",
                    text = matched.terms.single(),
                    success = true,
                )
            },
            onFallback = {
                fallbackCalled = true
                CompletableFuture.failedFuture(AssertionError("fallback must not run"))
            },
        ).get(2, TimeUnit.SECONDS)

        assertEquals("bob", result.text)
        assertTrue(result.success)
        assertFalse(fallbackCalled)
    }
}
