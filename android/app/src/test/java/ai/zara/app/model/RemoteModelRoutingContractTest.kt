package ai.zara.app.model

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class RemoteModelRoutingContractTest {
    @Test
    fun `explicit remote mode can use configured provider without weakening local fences`() {
        val session = File("src/main/java/ai/zara/app/AndroidAppSession.kt").readText()
        val submit = session.substringAfter("fun submitText(")
            .substringBefore("private fun submitAutoRemoteFirst(")

        assertTrue(submit.contains("RuntimeMode.Symbolic -> return submitLocalText("))
        assertTrue(submit.contains("allowModelFallback = false"))
        assertTrue(submit.contains("RuntimeMode.Local -> return submitLocalText(text, localConversationId)"))
        assertTrue(submit.contains("RuntimeMode.Remote ->"))
        assertTrue(submit.contains("if (remoteConnected)"))
        assertTrue(submit.contains("if (cloudEnabled)"))
        assertTrue(submit.contains("submitCloudModelText(text, localConversationId)"))

        val auto = session.substringAfter("private fun submitAutoRemoteFirst(")
            .substringBefore("private fun submitCloudModelText(")
        assertFalse("Auto must not silently route to a cloud provider", auto.contains("cloudAi.generate"))
        assertFalse("Auto must not silently route to a cloud provider", auto.contains("submitCloudModelText"))
    }

    @Test
    fun `remote provider settings never render or persist a plaintext key`() {
        val settings = File(
            "src/main/java/ai/zara/app/ui/RemoteModelSettings.kt"
        ).readText()
        val storage = File(
            "src/main/java/ai/zara/app/model/AndroidCloudModelStorage.kt"
        ).readText()
        val keyStore = File(
            "src/main/java/ai/zara/app/model/CloudApiKeyStore.kt"
        ).readText()

        assertTrue(settings.contains("OpenRouter"))
        assertTrue(settings.contains("OpenAI-compatible"))
        assertTrue(settings.contains("PasswordVisualTransformation"))
        assertTrue(settings.contains("stored in Android Keystore"))
        assertTrue(settings.contains("Used only by explicit Remote routing"))
        assertFalse(settings.contains("KeyValueRow(\"API key\""))
        assertTrue(storage.contains("AndroidKeystoreCredentialCipher"))
        assertTrue(keyStore.contains("cipher.seal(plaintext)"))
    }

    @Test
    fun `provider endpoint and model are wired through the live activity`() {
        val activity = File("src/main/java/ai/zara/app/MainActivity.kt").readText()
        val shell = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()

        assertTrue(activity.contains("appSession.cloudModelState()"))
        assertTrue(activity.contains("onConfigureCloudModel"))
        assertTrue(activity.contains("appSession.configureCloudModel(config, apiKey)"))
        assertTrue(activity.contains("onClearCloudModelApiKey"))
        assertTrue(shell.contains("RemoteModelSettingsCard("))
        assertTrue(shell.contains("cloudModelState = cloudModelState"))
    }
}
