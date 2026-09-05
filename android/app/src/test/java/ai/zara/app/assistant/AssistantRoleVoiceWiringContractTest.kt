package ai.zara.app.assistant

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class AssistantRoleVoiceWiringContractTest {
    @Test
    fun `assistant and manual capture enter distinct ownership paths`() {
        val session = File("src/main/java/ai/zara/app/AndroidAppSession.kt").readText()
        val bridge = File("src/main/java/ai/zara/app/assistant/AssistantVoiceSessionBridge.kt").readText()

        assertTrue(session.contains("AssistantVoiceOwnership.Manual"))
        assertTrue(session.contains("AssistantVoiceOwnership.Assistant"))
        assertTrue(bridge.contains("pressAssistantToTalk(true)"))
        assertFalse(bridge.contains("pressToTalk(true)"))
    }

    @Test
    fun `role outcome is reduced before canonical assistant role reaches capture guard`() {
        val session = File("src/main/java/ai/zara/app/AndroidAppSession.kt").readText()

        assertTrue(session.contains("controller.observeAssistantRole(roleOutcome)"))
        assertTrue(session.contains("val role = state().assistantRole"))
        assertTrue(session.contains("assistantVoiceGuard.onRoleChanged(role)"))
    }
}
