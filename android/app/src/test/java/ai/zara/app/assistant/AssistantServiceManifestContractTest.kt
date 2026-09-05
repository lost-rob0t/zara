package ai.zara.app.assistant

import java.io.File
import org.junit.Assert.assertTrue
import org.junit.Test

class AssistantServiceManifestContractTest {
    @Test
    fun `manifest exposes only the public voice interaction service contract`() {
        val manifest = File("src/main/AndroidManifest.xml").readText()

        assertTrue(manifest.contains("android.service.voice.VoiceInteractionService"))
        assertTrue(manifest.contains("android.permission.BIND_VOICE_INTERACTION"))
        assertTrue(manifest.contains("android:name=\"android.voice_interaction\""))
        assertTrue(manifest.contains("android:resource=\"@xml/voice_interaction_service\""))
        assertTrue(manifest.contains("android:name=\".assistant.ZaraVoiceInteractionSessionService\""))
        assertTrue(manifest.contains("android:process=\":voice\""))
    }

    @Test
    fun `voice interaction metadata binds Zara session service and assist support`() {
        val metadata = File("src/main/res/xml/voice_interaction_service.xml").readText()

        assertTrue(metadata.contains("android:sessionService=\"ai.zara.app.assistant.ZaraVoiceInteractionSessionService\""))
        assertTrue(metadata.contains("android:supportsAssist=\"true\""))
    }
}
