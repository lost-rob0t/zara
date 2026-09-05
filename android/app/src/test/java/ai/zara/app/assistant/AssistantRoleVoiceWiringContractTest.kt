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
    fun `canonical assistant role observation reaches capture guard`() {
        val session = File("src/main/java/ai/zara/app/AndroidAppSession.kt").readText()

        assertTrue(session.contains("assistantVoiceGuard.onRoleChanged(role)"))
        assertTrue(session.contains("controller.observeAssistantRole(role)"))
    }
}
