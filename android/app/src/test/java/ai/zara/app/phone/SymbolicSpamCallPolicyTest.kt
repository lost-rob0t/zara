package ai.zara.app.phone

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class SymbolicSpamCallPolicyTest {
    @Test
    fun suspectedSpamGetsFixedGreetingAndOnlyTwoUserFacingActions() {
        val decision = SymbolicSpamCallPolicy().decide(isSuspectedSpam = true)

        val answer = decision as SymbolicSpamDecision.Answer
        assertEquals("Hi, I'm a symbolic system.", answer.greeting)
        assertEquals(
            setOf(SymbolicSpamAction.AlertUser, SymbolicSpamAction.VoiceTakeover),
            answer.allowedActions,
        )
        assertTrue(answer.prologTools.isEmpty())
    }

    @Test
    fun ordinaryCallsAreNeverHijacked() {
        assertEquals(
            SymbolicSpamDecision.PassThrough,
            SymbolicSpamCallPolicy().decide(isSuspectedSpam = false),
        )
    }
}
