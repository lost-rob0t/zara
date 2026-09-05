package ai.zara.app.assistant

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class AssistantInvocationGateTest {
    @Test
    fun `press can start only while shown and only once`() {
        val gate = AssistantInvocationGate()

        assertFalse(gate.beginPress())
        assertTrue(gate.show())
        assertTrue(gate.beginPress())
        assertFalse(gate.beginPress())
    }

    @Test
    fun `release before async start completes commits immediately after start`() {
        val gate = AssistantInvocationGate()
        gate.show()
        gate.beginPress()

        assertEquals(AssistantCaptureFinish.None, gate.releasePress())
        assertEquals(AssistantCaptureFinish.Commit, gate.startSucceeded())
    }

    @Test
    fun `release after start commits exactly once`() {
        val gate = AssistantInvocationGate()
        gate.show()
        gate.beginPress()
        assertEquals(AssistantCaptureFinish.None, gate.startSucceeded())

        assertEquals(AssistantCaptureFinish.Commit, gate.releasePress())
        assertEquals(AssistantCaptureFinish.None, gate.releasePress())
    }

    @Test
    fun `cancel before async start completes cancels immediately after start`() {
        val gate = AssistantInvocationGate()
        gate.show()
        gate.beginPress()

        assertEquals(AssistantCaptureFinish.None, gate.cancelPress())
        assertEquals(AssistantCaptureFinish.Cancel, gate.startSucceeded())
    }

    @Test
    fun `hide fences late start and cancels active capture`() {
        val pending = AssistantInvocationGate()
        pending.show()
        pending.beginPress()
        assertEquals(AssistantCaptureFinish.None, pending.hide())
        assertEquals(AssistantCaptureFinish.Cancel, pending.startSucceeded())

        val active = AssistantInvocationGate()
        active.show()
        active.beginPress()
        active.startSucceeded()
        assertEquals(AssistantCaptureFinish.Cancel, active.hide())
        assertEquals(AssistantCaptureFinish.None, active.hide())
    }

    @Test
    fun `failed start resets gesture so user may retry`() {
        val gate = AssistantInvocationGate()
        gate.show()
        gate.beginPress()
        gate.startFailed()

        assertTrue(gate.beginPress())
    }
}
