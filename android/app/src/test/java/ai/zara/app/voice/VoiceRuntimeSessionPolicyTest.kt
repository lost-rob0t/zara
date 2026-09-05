package ai.zara.app.voice

import ai.zara.app.runtime.EnrollmentReadiness
import ai.zara.app.runtime.RuntimeState
import ai.zara.app.runtime.ServerConnection
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class VoiceRuntimeSessionPolicyTest {
    @Test
    fun `same authenticated session keeps playback`() {
        val runtime = connectedRuntime(generation = 4, sessionId = "session-1")
        val stream = VoiceStreamState.connected("session-1")
        assertFalse(VoiceRuntimeSessionPolicy.shouldInterruptPlayback(runtime, stream))
    }

    @Test
    fun `reconnecting runtime interrupts old session playback`() {
        val runtime = connectedRuntime(generation = 4, sessionId = "session-1").copy(
            server = ServerConnection.Reconnecting(generation = 5, attempt = 1),
            sessionId = null,
        )
        val stream = VoiceStreamState.connected("session-1")
        assertTrue(VoiceRuntimeSessionPolicy.shouldInterruptPlayback(runtime, stream))
    }

    @Test
    fun `replacement authenticated session interrupts stale playback`() {
        val runtime = connectedRuntime(generation = 5, sessionId = "session-2")
        val stream = VoiceStreamState.connected("session-1")
        assertTrue(VoiceRuntimeSessionPolicy.shouldInterruptPlayback(runtime, stream))
    }

    @Test
    fun `missing playback state needs no interruption`() {
        val runtime = connectedRuntime(generation = 4, sessionId = "session-1")
        assertFalse(VoiceRuntimeSessionPolicy.shouldInterruptPlayback(runtime, null))
    }

    private fun connectedRuntime(generation: Long, sessionId: String): RuntimeState =
        RuntimeState.initial().copy(
            enrollment = EnrollmentReadiness.Ready,
            server = ServerConnection.Connected(generation),
            sessionId = sessionId,
        )
}
