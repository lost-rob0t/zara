package ai.zara.app.ui

import java.io.File
import org.junit.Assert.assertTrue
import org.junit.Test

class PureSymbolicStaleUiCompletionFenceContractTest {
    private fun activity(): String = File("src/main/java/ai/zara/app/MainActivity.kt").readText()
    private fun store(): String = File("src/main/java/ai/zara/app/conversations/CanonicalConversationStore.kt").readText()

    @Test
    fun chatSubmissionCapturesCanonicalTurnIdentityBeforeAsyncCompletion() {
        val submit = activity()
            .substringAfter("val submitChatText:")
            .substringBefore("ZaraApp(")

        assertTrue(
            "Async chat completion must capture the canonical zara.db turn identity immediately after beginTurn so a late callback cannot settle a newer turn.",
            submit.contains("expectedTurnId = conversationStore.runningTurnId(conversationId)"),
        )
        assertTrue(
            "Pure-symbolic completion must settle only the captured canonical turn.",
            submit.contains("expectedTurnId = requireNotNull(expectedTurnId)"),
        )
    }

    @Test
    fun canonicalTerminalWriterSupportsExpectedTurnFence() {
        val source = store()
        val completeTurn = source
            .substringAfter("fun completeTurn(")
            .substringBefore("fun failTurn(")

        assertTrue(
            "Canonical completion must accept an expected turn id rather than selecting an arbitrary newer running assistant row by conversation only.",
            completeTurn.contains("expectedTurnId: String? = null"),
        )
        assertTrue(
            "When an expected turn is supplied, terminal idempotence must resolve that exact historical assistant turn.",
            completeTurn.contains("it.turnId == expectedTurnId"),
        )
        assertTrue(
            "The UI facade must expose the running canonical turn id from the existing zara.db owner; do not mint a second identity.",
            source.contains("fun runningTurnId(conversationId: String): String?"),
        )
    }
}
