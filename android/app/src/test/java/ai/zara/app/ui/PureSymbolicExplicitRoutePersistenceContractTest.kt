package ai.zara.app.ui

import java.io.File
import kotlin.test.Test
import kotlin.test.assertContains
import kotlin.test.assertFalse

class PureSymbolicExplicitRoutePersistenceContractTest {
    @Test
    fun pureSymbolicResultsTerminalizeCanonicalConversationEvenWhenControllerRouteIsExplicit() {
        val source = File("src/main/java/ai/zara/app/MainActivity.kt").readText()
        val resultBranch = source.substringAfter("result != null").substringBefore("turnResult = result")
        val pureSymbolicBranch = resultBranch
            .substringAfter("ExecutionPolicy.PureSymbolic")
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
}
