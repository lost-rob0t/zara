package ai.zara.app.assistant

import ai.zara.app.runtime.AssistantRole
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class AssistantRoleVoicePolicyTest {
    @Test
    fun `assistant-owned capture cancels when role is lost`() {
        assertTrue(
            shouldCancelForAssistantRole(
                AssistantVoiceOwnership.Assistant,
                AssistantRole.NotHeld,
            ),
        )
        assertTrue(
            shouldCancelForAssistantRole(
                AssistantVoiceOwnership.Assistant,
                AssistantRole.PlatformUnavailable,
            ),
        )
    }

    @Test
    fun `assistant-owned capture remains valid while role is held`() {
        assertFalse(
            shouldCancelForAssistantRole(
                AssistantVoiceOwnership.Assistant,
                AssistantRole.Held,
            ),
        )
    }

    @Test
    fun `manual capture is independent of system assistant role`() {
        assertFalse(
            shouldCancelForAssistantRole(
                AssistantVoiceOwnership.Manual,
                AssistantRole.NotHeld,
            ),
        )
        assertFalse(
            shouldCancelForAssistantRole(
                AssistantVoiceOwnership.Manual,
                AssistantRole.PlatformUnavailable,
            ),
        )
    }

    @Test
    fun `idle voice never needs role-loss cancellation`() {
        assertFalse(
            shouldCancelForAssistantRole(
                AssistantVoiceOwnership.None,
                AssistantRole.NotHeld,
            ),
        )
    }
}
