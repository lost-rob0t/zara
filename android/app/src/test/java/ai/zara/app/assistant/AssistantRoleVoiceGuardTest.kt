package ai.zara.app.assistant

import ai.zara.app.runtime.AssistantRole
import org.junit.Assert.assertEquals
import org.junit.Test

class AssistantRoleVoiceGuardTest {
    @Test
    fun `assistant capture is cancelled exactly once when role is lost`() {
        var cancellations = 0
        val guard = AssistantRoleVoiceGuard { cancellations += 1 }

        guard.onCaptureStarted(AssistantVoiceOwnership.Assistant)
        guard.onRoleChanged(AssistantRole.NotHeld)
        guard.onRoleChanged(AssistantRole.PlatformUnavailable)

        assertEquals(1, cancellations)
    }

    @Test
    fun `manual capture is not cancelled by assistant role loss`() {
        var cancellations = 0
        val guard = AssistantRoleVoiceGuard { cancellations += 1 }

        guard.onCaptureStarted(AssistantVoiceOwnership.Manual)
        guard.onRoleChanged(AssistantRole.NotHeld)

        assertEquals(0, cancellations)
    }

    @Test
    fun `completed assistant capture is inert on later role loss`() {
        var cancellations = 0
        val guard = AssistantRoleVoiceGuard { cancellations += 1 }

        guard.onCaptureStarted(AssistantVoiceOwnership.Assistant)
        guard.onCaptureStopped()
        guard.onRoleChanged(AssistantRole.NotHeld)

        assertEquals(0, cancellations)
    }

    @Test
    fun `held role leaves assistant capture active`() {
        var cancellations = 0
        val guard = AssistantRoleVoiceGuard { cancellations += 1 }

        guard.onCaptureStarted(AssistantVoiceOwnership.Assistant)
        guard.onRoleChanged(AssistantRole.Held)

        assertEquals(0, cancellations)
    }
}
