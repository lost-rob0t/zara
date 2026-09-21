package ai.zara.app.ui

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class PureSymbolicExplicitRoutePersistenceContractTest {
    private fun activity(): String = File("src/main/java/ai/zara/app/MainActivity.kt").readText()

    @Test
    fun pureSymbolicResultsTerminalizeCanonicalConversationEvenWhenControllerRouteIsExplicit() {
        val resultBranch = activity().substringAfter("else if (result != null) {")
        val pureSymbolicBranch = resultBranch
            .substringAfter("if (executionPolicy == ConversationExecutionPolicy.PURE_SYMBOLIC) {")
            .substringBefore("} else {")

        assertTrue(
            "Every pure-symbolic result must terminalize the canonical turn; explicit /prolog, /expert, ?-, and ? routes bypass the persisted natural-language resolver.",
            pureSymbolicBranch.contains("conversationStore.completeTurn("),
        )
        assertFalse(
            "Reloading state without terminalizing leaves explicit pure-symbolic assistant history Pending.",
            pureSymbolicBranch.contains("conversationStore.state()"),
        )
    }

    @Test
    fun exceptionalPureSymbolicCompletionCannotAbandonExplicitPendingHistory() {
        val errorBranch = activity()
            .substringAfter("if (error != null) {")
            .substringBefore("} else if (result != null) {")
        val pureSymbolicBranch = errorBranch
            .substringAfter("if (executionPolicy == ConversationExecutionPolicy.PURE_SYMBOLIC) {")
            .substringBefore("} else {")

        assertTrue(
            "Exceptional explicit pure-symbolic routes must terminalize a still-Running canonical assistant row.",
            pureSymbolicBranch.contains("recordTurnFailure(conversationId, error)"),
        )
        assertFalse(
            "Reloading canonical state is not terminalization and would preserve a Pending assistant row.",
            pureSymbolicBranch.contains("conversationStore.state()"),
        )
    }
}
