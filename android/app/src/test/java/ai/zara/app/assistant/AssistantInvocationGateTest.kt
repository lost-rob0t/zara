package ai.zara.app.assistant

import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class AssistantInvocationGateTest {
    @Test
    fun `duplicate show cannot start a second capture`() {
        val gate = AssistantInvocationGate()

        assertTrue(gate.beginShow())
        assertFalse(gate.beginShow())
    }

    @Test
    fun `hide fences a late async start completion`() {
        val gate = AssistantInvocationGate()
        gate.beginShow()

        assertTrue(gate.endShow())
        assertTrue(gate.shouldCancelLateStart())
    }

    @Test
    fun `capture cancellation can only be claimed once per invocation`() {
        val gate = AssistantInvocationGate()
        gate.beginShow()
        gate.endShow()

        assertTrue(gate.claimCancellation())
        assertFalse(gate.claimCancellation())
    }

    @Test
    fun `next invocation resets cancellation claim`() {
        val gate = AssistantInvocationGate()
        gate.beginShow()
        gate.endShow()
        assertTrue(gate.claimCancellation())

        assertTrue(gate.beginShow())
        gate.endShow()
        assertTrue(gate.claimCancellation())
    }

    @Test
    fun `hide while already hidden is idempotent`() {
        val gate = AssistantInvocationGate()

        assertFalse(gate.endShow())
        assertTrue(gate.shouldCancelLateStart())
    }
}
