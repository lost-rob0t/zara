package ai.zara.app.ui

import java.io.File
import kotlin.test.Test
import kotlin.test.assertContains
import kotlin.test.assertFalse

class PureSymbolicExplicitRoutePersistenceContractTest {
    private fun activity(): String = File("src/main/java/ai/zara/app/MainActivity.kt").readText()

    @Test
    fun pureSymbolicResultsTerminalizeCanonicalConversationEvenWhenControllerRouteIsExplicit() {
        val resultBranch = activity().substringAfter("else if (result != null) {")
        val pureSymbolicBranch = resultBranch
            .substringAfter("if (executionPolicy == ConversationExecutionPolicy.PURE_SYMBOLIC) {")
            .substringBefore("} else {")

        assertContains(
            pureSymbolicBranch,
            "conversationStore.completeTurn(",
            "Every pure-symbolic result must terminalize the canonical turn; explicit /prolog, /expert, ?-, and ? routes bypass the persisted natural-language resolver.",
        )
        assertFalse(
            pureSymbolicBranch.contains("conversationStore.state()"),
            "Reloading state without terminalizing leaves explicit pure-symbolic assistant history Pending.",
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

        assertContains(
            pureSymbolicBranch,
            "recordTurnFailure(conversationId, error)",
            "Exceptional explicit pure-symbolic routes must terminalize a still-Running canonical assistant row.",
        )
        assertFalse(
            pureSymbolicBranch.contains("conversationStore.state()"),
            "Reloading canonical state is not terminalization and would preserve a Pending assistant row.",
        )
    }
}
